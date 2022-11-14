########################################################################################################################################################
#' @ Script name: 04_cohort_model_draws
#' @ Purpose: This is the "main" script that runs the cohorting model. 
#' @ Author:
#' @ Date: 2021-08-07
#' @ Notes: Results from this code produce year-end coverage estimates for a given year. 
#' For year midpoint estimates for year y, would need to average y with y-1. 
#' Also uses age demographer, i.e. age 1 = all children age 12-23months.
########################################################################################################################################################

# Loading in catchup and initial campaigns
rm(list=ls())

denom <- "hi_risk" # is the goal denominator for coverage all or hi_risk pop only?
# set the last year you want to model for
model_end_year <- 2021
# pull from the newest routine immunization ST-GPR run_id
run_id <- 201580
# path to campaign data
data_path <- paste0(j, "FILEPATH/DataBase All Campaigns MenA 2022 08_with_new_data.xlsx")
# Generate version name
date <- gsub("-", "_", Sys.Date())
version <- paste(run_id, date, sep = "_")

# SOURCE SHARED FUNCTIONS ------------------------------
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))
main_dir <- "FILEPATH"
source(paste0(main_dir, "/code/helper_functions/campaign_crosswalk.R"))
source(paste0(main_dir, "/code/helper_functions/collapse_point.R"))
pacman::p_load(ggplot2, readxl, stringr, pbapply, openxlsx, data.table)

# Pull necessary shared function objects
# Locations
loc_meta <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, decomp_step = "iterative")
# Single year population from 1 to 29
pop <- get_population(age_group_id = c(238,50:77), 
                      year = 2005:model_end_year, 
                      location_id = loc_meta[super_region_name %like% "Africa"]$location_id, 
                      sex_id = 3, 
                      release_id = 9, 
                      single_year_age = T)
# Adjust formatting - make the "age group id" equal to the single year age reflected by the row
pop$sex_id <- NULL; pop$run_id <- NULL
pop[age_group_id == 238, age_group_id := 49]
pop[, age_group_id := age_group_id - 48]

# Get schedule
schedule <- as.data.table(read.xlsx(paste0(main_dir, "/Data/merged_schedule.xlsx")))
setnames(schedule, "ISO_code", "ihme_loc_id")
schedule <- merge(schedule, loc_meta[,.(location_name, location_id, ihme_loc_id)], by= "ihme_loc_id")
schedule$year_intro <- as.numeric(schedule$year_intro)

# Get STGPR results
results <- lapply(list.files(file.path("FILEPATH", run_id, "draws_temp_0"), 
                               full.names=TRUE), fread)
results <- rbindlist(results, use.names = TRUE)

results.keep <- merge(results, schedule, by = c("location_id"))
results.keep <- results.keep[year_id >= 2005]
# Coerce to zero before introduction
drawnames <- names(results.keep)[names(results.keep) %like% "draw_"]
results.keep[year_id < year_intro, (drawnames) := 0]

# Set output directory
out_dir <- paste0(main_dir, "/vetting/", version, "/")
dir.create(out_dir, showWarnings = FALSE)

#----PREP "PRE-PREPPED" RAW DATA----------------------------------------------------------------------------------------
# read in the data
mena_data <- read_excel(data_path, sheet="MenAcampaigns2019 06_published") %>% data.table

# fix formatting: add location information, population, split multi-year campaigns
mena_data <- campaign_clean(mena_data, loc_meta, pop)

# get the variance from the sample size
mena_data <- set_campaign_variance(mena_data, sample_size = 100)

# crosswalk admin to the level of survey
mena_data <- campaign_crosswalk(data = mena_data, out_dir = out_dir)

# cap survey doses based on population
mena_data <- cap_survey_doses(mena_data)

mena_data <- mena_data[year_id <= model_end_year]
#----START COHORT MODEL----------------------------------------------------------------------------------------
# Account for locations in meningitis belt with RI and no campaigns (if there are any)
data <- copy(mena_data)
locs_add <- setdiff(results.keep[WHO_REGION == "AFR"]$location_id, data$location_id)
init_list <- lapply(locs_add, function(l){
  year_ri <- as.numeric(subset(schedule, location_id ==l)$year_intro)
  # initialize an empty DF for the year prior to RI
  df_init <- data.table(location_id = l, year_id = year_ri - 1, ihme_loc_id = loc_meta[location_id == l]$ihme_loc_id, year_phases = 1,
                        country = loc_meta[location_id == l]$location_name, geo_area = "Nation", corrected_coverage = 0, type = "Mass")
  return(df_init)
})
# Add in initialized empty rows
data <- rbind(data, rbindlist(init_list), fill =T)

# Get the "high-risk" scalars
# prop at high risk = pop of areas that were targeted in a campaign / pop of whole country,
# i.e., we assume that the targeted area comprises the whole high-risk population.
# We get these scalars from the FIRST instance of a high-risk campaign in a country.
hi_risk_dt <- data[geo_area == "High-risk areas", .SD[which.min(year_start)], by = location_name]
hi_risk_dt[, hi_risk_scalar := target/population]
# write.xlsx(hi_risk_dt[,.(country, geo_area, geo_area_detail, age_target, target, year, population, hi_risk_scalar)], paste0(out_dir, "hi_risk_scalar_table.xlsx"))

# For all population, when a campaign comes for a subset of the country,
# doses / GBD population = coverage estimate
# doses = coverage estimate * target population from campaign
# coverage estimate * target population from campaign / GBD population = all pop coverage estimate

# However for a high-risk denominator:
# doses / high-risk population = coverage estimates
# high-risk population = GBD * hi_risk_scalar
# (coverage estimate * target population from campaign) / (GBD * hi_risk_scalar) = high-risk pop coverage estimate

data <- merge(data, hi_risk_dt[,.(location_name, hi_risk_scalar)], by = "location_name", all.x = TRUE)

if(denom == "all") {
  data[geo_area != "Nation", pop_scalar := target/population]
  # Make sure that grouped locations add up
  # For locations w/o a hi-risk scalar (grouped rows should comprise whole country): the grouped loc scalars should sum to 1
  data[is.na(hi_risk_scalar) & group == 1, pop_scalar := lapply(.SD, function(x){x/sum(x)}), by = c("location_name", "group"), .SDcols = "pop_scalar"]
  # For locations w/ a hi-risk scalar (grouped rows should comprise whole hi risk area): the grouped loc scalars should sum to the hi_risk scalar
  data[!is.na(hi_risk_scalar) & group == 1, pop_scalar := lapply(.SD, function(x){hi_risk_scalar*x/sum(x)}), by = c("location_name", "group"), .SDcols = "pop_scalar"]
} else if (denom == "hi_risk"){
  data[geo_area == "High-risk areas", pop_scalar := target/(population*hi_risk_scalar)]
  # Sometimes this population scalar is >1: i.e., the target pop reported by the campaign is greater than the GBD est pop for that country * the high-risk population
  # But we don't necessarily want to adjust the reported coverage estimates upward. Cap these scalars at 1
  data[pop_scalar > 1, pop_scalar := 1]
  # Special case: rows with multiple years extracted separately but is full-country
  data[geo_area != "Nation" & geo_area != "High-risk areas", pop_scalar := target/population]
  # These grouped location scalars should always sum to 1 
  data[group == 1, pop_scalar := lapply(.SD, function(x){x/sum(x)}), by = c("location_name", "group"), .SDcols = "pop_scalar"]
}

data[geo_area != "Nation", corrected_coverage := pop_scalar*corrected_coverage]

# phase-in vaccination coverage for multi-year campaigns
# i.e., assume coverage went up linearly over the course of multi-year campaigns
phase_data <- data[year_phases > 1]  # subset to rows with phased in mass campaign
phase_data <- phase_data[rep(seq_len(nrow(phase_data)), phase_data$year_phases)]  # duplicate rows by how many years the phase happened
phase_data[, corrected_coverage := corrected_coverage/year_phases]  # calculate the baseline/additive fraction to be added each campaign phase year
phase_data <- phase_data[, grp := .GRP, by=.(ihme_loc_id, type, year)]  # group and count by ihme_loc_id and type of campaign and year
phase_data$phase_count <- ave(phase_data$corrected_coverage, phase_data$grp, FUN = seq_along)  # sequence each row in the group
phase_data[, corrected_coverage := corrected_coverage*phase_count]  # coverage for each sequential year is additive

phase_data[, year_id := year_start + (phase_count-1)]  # assign correct year_id
phase_data[, c("grp", "phase_count") := NULL]  # get rid of unnecessary columns

# add back the rows of phased in data
data <- data[year_phases==1]
all_campaign <- rbind(data, phase_data)

# Write out this data for reference
write.xlsx(all_campaign, paste0(out_dir, "denom_", denom, "_input_data.xlsx"))

# Ignore mortality - assume same mortality in vaxx'd and unvaxx'd populations

# Generate draws in a binomial distribution for data based on sample size
# Coerce corrected coverage to 99% - above that, there is not enough uncertainty
all_campaign[corrected_coverage > .99, corrected_coverage := 0.99]
all_campaign[is.na(target), target := population]
ss <- 100
campaign_draws <- rbinom(n=1000 * length(all_campaign$corrected_coverage), prob=all_campaign$corrected_coverage, size=ss) %>% matrix(., ncol=1000, byrow=FALSE) %>% as.data.table
colnames(campaign_draws) <- drawnames
all_campaign <- cbind(all_campaign, campaign_draws)
all_campaign[, (drawnames) := .SD/ss, .SDcols = drawnames]

## For each location
vax_age_list <- pblapply(unique(all_campaign$location_id), function(l){
  # Initialize a dataframe with 0 coverage for 2009
  dt_init <- data.table(location_id = l, year_id = 2009, ihme_loc_id = loc_meta[location_id == l]$ihme_loc_id, year_phases = 1, sex_id = 3,
                        country = loc_meta[location_id == l]$location_name, geo_area = "Nation", type = "Mass")
  dt_init[, (drawnames) := 0]
  ## Initialize the cohort dataframe: get the population in the starting year
  pop_init <- copy(pop[location_id == l & year_id == 2009])
  dt_init <- merge(dt_init[,c("year_id", "location_id", drawnames), with = F], pop_init, by = c("year_id", "location_id"))
  coverage_list <- list()
  dt_init[, (drawnames) := .SD*population, .SDcols = drawnames]
  coverage_list[[as.character(2009)]] <- dt_init
  # For each passing year
  for (y in 2010:model_end_year){
    dt_tmp <- as.data.table(coverage_list[[as.character(y-1)]])
    dt_tmp$year_id <- y
    ## Age everyone up a year
    dt_tmp[, age_group_id := age_group_id + 1]
    ## Remove those who are now 30
    dt_tmp <- dt_tmp[age_group_id < 30]
    # Replace the population with the GBD population for that year (account for mort, migration)
    dt_tmp$population <- NULL
    pop_tmp <- copy(pop[location_id == l & year_id == y])
    dt_tmp <- merge(dt_tmp[year_id == y], pop_tmp, by = c("year_id", "location_id", "age_group_id"))
    ## Add new 1 year olds 
    dt_add <- pop[age_group_id == 1 & location_id == l & year_id == y]
    dt_add[, (drawnames) := 0]
    dt_tmp <- rbind(dt_add, dt_tmp, fill = T)
    ## IF there were ANY campaigns that year
    if(nrow(all_campaign[location_id == l & year_id == y])> 0){
      campaign_tmp <- all_campaign[location_id == l & year_id == y]
      campaign_ages <- campaign_tmp$age_start:campaign_tmp$age_end
      # For each age group in the campaign
      for(a in campaign_ages){
        if(sum(dt_tmp[age_group_id == a, drawnames, with = F]) == 0 |
           (campaign_tmp$group == 1)){
          # If this is the first campaign being delivered to this particular age
          # OR it is part of a multi-phase-year campaign and the data is MANUALLY extracted in separate years (group == 1)
          # Assume that the coverage is on top of the other coverage (i.e., coverage in different areas from before)
          # In the first case, the "other coverage" is zero
          dt_tmp[age_group_id == a, (drawnames) :=  lapply(0:999, function(i) {
            as.numeric(campaign_tmp[, paste0("draw_", i), with = F]) + get(paste0("draw_", i))
          })]
        } else{
          # Use the probability calculations for untargeted, unbiased from YFV paper - page 9 supplementary appendix
          dt_tmp[age_group_id == a, (drawnames) :=  lapply(0:999, function(i) {
            get(paste0("draw_", i)) + as.numeric(campaign_tmp[, paste0("draw_", i), with = F]) - get(paste0("draw_", i)) * as.numeric(campaign_tmp[, paste0("draw_", i), with = F])
          })]
        }
      }
    }
    ## IF RI occurs in that country-year
    ri <- nrow(results.keep[year_intro <= y & location_id == l & year_id == y])
    if(ri > 0){
      ## All AFR RI are in months
      age_imm_month <- as.numeric(substr(schedule[location_id == l]$Schedule, 1,2))
      age_imm_year <- floor(age_imm_month/12)
      ## Get estimated RI value from ST-GPR
      ## Add (RI from current year * fraction of 0(or 1)yo[age_imm_year] infants who were immunized at Xmo[age_imm_month] in current year)
      ## + (RI from previous year * fraction of 0(or 1)yo infants who were immunized at Xmo in previous year) to the numerator
      dt_tmp[age_group_id == age_imm_year+1, (paste0("ri_", drawnames)) :=  lapply(0:999, function(i) {
        as.numeric(results.keep[year_intro <= y & location_id == l & year_id == y, paste0("draw_", i), with = F]*(age_imm_month/12-age_imm_year) +
                     results.keep[year_intro <= y & location_id == l & year_id == y-1, paste0("draw_", i), with = F]*(1-(age_imm_month/12-age_imm_year)))
      })]
      # Use the probability calculations for untargeted, unbiased from YFV paper - page 9 supplementary appendix - takes care of if RI overlaps a campaign
      dt_tmp[age_group_id == age_imm_year+1, (drawnames) :=  lapply(0:999, function(i) {
        get(paste0("draw_", i)) + get(paste0("ri_draw_", i)) - get(paste0("draw_", i)) * get(paste0("ri_draw_", i))
      })]
      if(age_imm_year == 1){
        ## Only look at the current year
        ## (RI from current year * fraction of 0(or 1)yo[age_imm_year] infants who turned the age to be immunized at Xmo[age_imm_month] in current year
        dt_tmp[age_group_id == age_imm_year, (paste0("ri_", drawnames)) :=  lapply(0:999, function(i) {
          as.numeric(results.keep[year_intro <= y & location_id == l & year_id == y, paste0("draw_", i), with = F]*(1-(age_imm_month/12-age_imm_year)))
        })]
        # Use the probability calculations for untargeted, unbiased from YFV paper - page 9 supplementary appendix - takes care of if RI overlaps a campaign
        dt_tmp[age_group_id == age_imm_year, (drawnames) :=  lapply(0:999, function(i) {
          get(paste0("draw_", i)) + get(paste0("ri_draw_", i)) - get(paste0("draw_", i)) * get(paste0("ri_draw_", i))
        })]
      }
      # Delete temporary RI columns
      dt_tmp[, (paste0("ri_", drawnames)) := NULL]
    }
    # Recalculate covered population
    dt_tmp[, (paste0("covered_pop_", drawnames)) := .SD*population, .SDcols = drawnames]
    coverage_list[[as.character(y)]] <- dt_tmp
  }
  return(coverage_list)
}, cl = 24)

vax_summed_list <- pblapply(1:length(vax_age_list), function(i){
  # Get U5 and U30 values for each year
  coverage_list <- vax_age_list[[i]]
  dt_summed <- data.table()
  for (y in 2010:model_end_year){
    dt_tmp <- as.data.table(coverage_list[[as.character(y)]])
    dt_summed_new <- data.table(age_start = c(1,1), age_end = c(4,29), sex_id = 3, location_id = unique(dt_tmp$location_id), year_id = y)
    dt_5 <- dt_tmp[age_group_id < 5, lapply(.SD, sum), .SDcols = c("population", paste0("covered_pop_draw_", 0:999))]
    dt_5[, (paste0("coverage_", drawnames)) := .SD/population, .SDcols = paste0("covered_pop_draw_", 0:999)]
    dt_30 <- dt_tmp[, lapply(.SD, sum), .SDcols = c("population",paste0("covered_pop_draw_", 0:999))]
    dt_30[, (paste0("coverage_", drawnames)) := .SD/population, .SDcols = paste0("covered_pop_draw_", 0:999)]
    dt_summed_new <- cbind(dt_summed_new, rbind(dt_5, dt_30))
    dt_summed <- rbind(dt_summed, dt_summed_new)
  }
  # add zeros for years prior to 1st campaign
  year_fill <- min(dt_summed$year_id)-1
  dt_fill <- expand.grid(year_id = 2010:year_fill, age_start = 1, age_end = c(4,29), sex_id = 3, location_id = unique(dt_tmp$location_id)) %>% as.data.table
  dt_fill[, (paste0("coverage_", drawnames)) := 0]
  if(year_fill < 2010) dt_fill <- data.table()
  dt_summed <- rbind(dt_fill, dt_summed)
  return(dt_summed)
}, cl = 40)

all <- rbindlist(vax_summed_list)
all <- merge(all, loc_meta[,.(location_id, location_name, ihme_loc_id, super_region_name)], by = 'location_id')
# Take the mean of all national coverages, draw-wise to get the uncertainty
test <- all[,lapply(.SD, mean, na.rm=TRUE), by=c("age_start", "age_end", "sex_id", "year_id"), .SDcols=paste0("coverage_draw_", 0:999)]
test_collapse <- collapse_point(test, draws_name = "coverage")
write.xlsx(all, paste0(out_dir, "denom_", denom, "_cohorting_results_draw.xlsx"))
all_collapse <- collapse_point(all, draws_name = "coverage")
all <- cbind(all_collapse, age_start = all$age_start, age_end = all$age_end) 
write.xlsx(all, paste0(out_dir, "denom_", denom, "_cohorting_results_summary.xlsx"))

age <- pblapply(1:length(vax_age_list), function(i){
  # Get U5 and U30 values for each year
  coverage_list <- vax_age_list[[i]]
  # Drop 2009
  coverage_list <- coverage_list[2:length(coverage_list)]
  coverage <- rbindlist(coverage_list)
  return(coverage)
})
age <- rbindlist(age)
age <- merge(age, loc_meta[,.(location_id, location_name, ihme_loc_id, super_region_name)], by = 'location_id')
write.xlsx(age, paste0(out_dir, "denom_", denom, "_age_specific_cohorting_results_draw.xlsx"))
# Delete the covered population draws
age[, paste0("covered_pop_draw_", 0:999) := NULL]
age <- collapse_point(age, draws_name = "draw")
write.xlsx(age, paste0(out_dir, "denom_", denom, "_age_specific_cohorting_results_summary.xlsx"))