########################################################################################################################################################
#' @ Script name: campaign_crosswalk
#' @ Purpose: This script houses several crosswalking functions for MenA campaign data: campaign_clean, set_campaign_variance, cap_survey_doses, and campaign_crosswalk
#' @ Author:
#' @ Date: 2021-08-07
#' @ Notes: This script houses 4 functions: 3 helper functions followed by a main function
#' campaign_clean = formatting for year, age, and sex, and adding population
#' set_campaign_variance = assign variance to campaign coverage data points when not given
#' cap_survey_doses = cap survey datapoints at admin value when survey value is implausible, given population estimates
#' campaign_crosswalk = adjust for bias of admin data; adjust to the level of survey
#######################################################################################################################################################

pacman::p_load(readxl, dplyr, stringr)
# SOURCE SHARED FUNCTIONS ------------------------------
library(crosswalk002, lib.loc = "/FILEPATH/")
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))

campaign_clean <- function(data, loc_meta, pop){
  data[is.na(`Geo Area`), `:=` (`Geo Area`= "Other")]
  names(data) <- tolower(names(data)) 
  names(data) <- gsub(" ", "_", names(data))
  
  # merge in relevant location information
  data <- merge(data, loc_meta[,.(ihme_loc_id, location_id, location_name, most_detailed, level)], by="ihme_loc_id", all.x=T)
  if (nrow(data[is.na(location_id)]) > 0) stop("At least one row of data isn't matched to a GBD location_id by name. Figure out and confirm if should be dropped.")
  
  # clean up 'year' column (assume that if multi-year, apply in first year)
  data$year_start <- substr(data$year, 0, 4) %>% as.integer
  data$year_end <- str_sub(data$year, - 4, - 1) %>% as.integer
  data[, year_phases := (year_end+1)-year_start]
  data$year_id <- data$year_end # since the coverage % reflects value at the end of the campaign
  
  # do the same for age
  data$age_start <- substr(data$age_target, 1,1) %>% as.integer
  data$age_end <- substr(data$age_target, 3,4) %>% as.integer
  
  # all of this data is both-sex
  data$sex_id <- 3
  data$sex <- "Both"
  
  ## Offset coverage: Cap values of > 1 at 0.999, and offset zeros to calculate variance
  data[admin_cv >= 1, admin_cv := 0.999]
  data[admin_cv <= 0, admin_cv := 0.001]
  
  ## Fill population
  for(i in 1:nrow(data)){
    row <- data[i,]
    pop_tmp <- pop[year_id == row$year_id & location_id == row$location_id & age_group_id >= as.numeric(row$age_start) & age_group_id <= as.numeric(row$age_end)]
    total_pop <- sum(pop_tmp$population)
    data[i, population := total_pop]
  }
  
  return(data)
}

set_campaign_variance <- function(data, sample_size = "target") {
  # sample_size argument can either be "target" (use study-reported target population), or a numeric value, which is an arbitrary sample size to calculate variance from binomial
  if(sample_size == "target") data$sample_size <- data$target else data$sample_size <- sample_size
  data <- data[, admin_variance := admin_cv * (1 - admin_cv) / sample_size]
  data <- data[, survey_variance := coverage_survey * (1 - coverage_survey) / sample_size]
  data <- data[, `:=` (admin_se = sqrt(admin_variance), survey_se = sqrt(survey_variance))]
  return(data)
}

cap_survey_doses <- function(data, population){
  # Calculate the # of doses implied by survey data, using IHME population estimates
  # only valid where we have pop estimates (i.e., national level)
  data[geo_area == "Nation", implied_doses := coverage_survey*population]
  # If the # of doses is higher than the admin data say were given out during a campaign, we would cap at the admin value
  data[implied_doses > vaccinated & coverage_survey > admin_cv, corrected_coverage := admin_cv]
  if(nrow(data[implied_doses > vaccinated & coverage_survey > admin_cv]) >= 1) {
    print(paste("for", paste(unique(data[implied_doses > vaccinated & coverage_survey > admin_cv]$location_name), collapse = ", "), "coverage capped at admin value"))
  }
  return(data)
}

campaign_crosswalk <- function(data, out_dir){
  # Set up log(target) for crosswalk
  data[, log_target := log(target)]
  data[, definition := ifelse(is.na(coverage_survey), "admin", "survey")]
  data$alt <- "admin"
  data$ref <- "survey"
  data[, `:=` (mean = ifelse(definition == "survey", coverage_survey, admin_cv), se = ifelse(definition == "survey", survey_se, admin_se))]
  # This ratio is for informational purposes only; logit diff is actually used in crosswalk
  data[, ratio := admin_cv/coverage_survey]
  # Get matched data only
  df_matched <- copy(data)[!is.na(ratio)]
  
  # Get logit differences and logit SEs
  # Use crosswalk package to calculate ratios
  df_matched[, c("logit_mean_survey", "se_logit_mean_survey") := data.table(delta_transform(mean = coverage_survey, sd = survey_se, transformation = "linear_to_logit"))]
  df_matched[, c("logit_mean_admin", "se_logit_mean_admin") := data.table(delta_transform(mean = admin_cv, sd = admin_se, transformation = "linear_to_logit"))]

  df_matched[, c("logit_diff", "logit_diff_se") := data.table(calculate_diff(
    df = df_matched, 
    alt_mean = "logit_mean_survey", alt_sd = "se_logit_mean_survey",
    ref_mean = "logit_mean_admin", ref_sd = "se_logit_mean_admin" ))]
  
  df_matched <<- df_matched
  
  # Set up data
  df1 <- CWData(
    df = df_matched,          # dataset for metaregression
    obs = "logit_diff",       # column name for the observation mean
    obs_se = "logit_diff_se", # column name for the observation standard error
    alt_dorms = "alt",     # column name of the variable indicating the alternative method
    ref_dorms = "ref",     # column name of the variable indicating the reference method
    covs = list("log_target"),     # names of columns to be used as covariates later
    study_id = "ihme_loc_id",    # name of the column indicating group membership, usually the matching groups
    add_intercept = TRUE      # adds a column called "intercept" that may be used in CWModel()
  )
  
  # Run model
  fit1 <- CWModel(
    cwdata = df1,            # object returned by `CWData()`
    obs_type = "diff_logit", # "diff_log" or "diff_logit" depending on whether bounds are [0, Inf) or [0, 1]
    cov_models = list(       # specifying predictors in the model; see help(CovModel)
      CovModel(cov_name = "intercept"),
      CovModel(cov_name = "log_target") ),
    gold_dorm = "survey"   # the level of `alt_dorms` that indicates it's the gold standard
    # this will be useful when we can have multiple "reference" groups in NMA
  )
  
  # Save Results
  df_result <- fit1$create_result_df()
  write.csv(df_result, paste0(out_dir, "/df_result_crosswalk.csv"))
  py_save_object(object = fit1, filename = paste0(out_dir, "/fit1.pkl"), pickle = "dill")
  
  # Adjust the original values
  preds1 <- adjust_orig_vals(
    fit_object = fit1, # object returned by `CWModel()`
    df = data,
    orig_dorms = "definition",
    orig_vals_mean = "mean",
    orig_vals_se = "se",
    # data_id = "row_id"   # optional argument to add a user-defined ID to the predictions;
    # name of the column with the IDs
  )
  
  data[, c("corrected_coverage", "sd_corrected_coverage", "pred_logit", "pred_se_logit", "data_id")] <- preds1
  
  # do not adjust SE for reference definition
  data[definition == "survey", sd_corrected_coverage := se]
  return(data)
}
