########################################################################################################################################################
#' @ Script name: figure_3
#' @ Purpose: Generate maps of routine immunization (ST-GPR) and cohort model results
#' @ Author: Rose Bender
#' @ Date: 2021-08-07
#' @ Notes: 
########################################################################################################################################################

rm(list=ls())

# SOURCE SHARED FUNCTIONS ------------------------------
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))

### GBD mapping function
main_dir <- "FILEPATH"
source(paste0(main_dir, "/code/helper_functions/gbd_map_custom.R"))
source(paste0(main_dir, "/code/helper_functions/campaign_crosswalk.R"))

# Load packages
pacman::p_load(data.table, openxlsx, rhdf5, ggplot2, readxl, ggpubr, cowplot)

# Define versions
run_id <- 201580
denom <- "hi_risk"
model_end_year <- 2021
result_date <- "2022_09_04"

if(denom == "hi_risk"){
  figure_name <- "3"
  pop_name <- "high risk"
} else if (denom == "all"){
  figure_name <- "S5"
  pop_name <- "full"
}

filename <- paste0("figure_", figure_name, ".pdf")

# Set output directory
out_dir <- file.path(main_dir, "figures", "second_resubmission")

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

# Load results from STGPR for included countries
results.keep <- as.data.table(read.xlsx(paste0(main_dir, "/vetting/", run_id, "/results_keep.xlsx")))

# ROUTINE IMMUNIZATION MAP
counts <- results.keep[year_id == model_end_year,c('location_id','gpr_mean')]
counts <- counts[,.(gpr_mean = mean(gpr_mean)), by = "location_id"]
names(counts) <- c("location_id","mapvar")
# Add other locations
other_locs <- loc_meta[level == 3 & !location_id %in% counts$location_id][,.(location_id)]
other_locs$mapvar <- NA
counts <- rbind(counts, other_locs, fill = T)
# For plotting: Get campaign data
data_path <- paste0(j, "/FILEPATH/DataBase All Campaigns MenA 2022 08_corrected.xlsx")
mena_data <- read_excel(data_path, sheet="MenAcampaigns2019 06_published") %>% data.table
mena_data <- campaign_clean(mena_data, loc_meta, pop)
# Set limits and labels across all plots 
limit <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
label <- c("<10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", ">90%")
# Create pattern variable
men_belt_loc_ids <- unique(mena_data$location_id)
counts[location_id %in% men_belt_loc_ids, patternvar := 0]
plot_list <- list()
plot_list[["routine"]] <- gbd_map(data = counts,
              just_menbelt = TRUE,
              limits = limit,
              labels= label,
              col= "Spectral", col.reverse= FALSE,
              color_legend = TRUE,
              intro_legend = FALSE,
              legend.title = paste0("Vaccine Coverage"),
              legend.cex = 1,
              legend.shift = c(5, -16),
              legend.columns = 10)

# COHORTING RESULTS MAPS
# Map!
cohort_results_all <-  as.data.table(read.xlsx(paste0(main_dir, "/vetting/", run_id, "_", result_date, "/", "denom_", denom, "_cohorting_results_summary.xlsx")))
cohort_results_all <- merge(cohort_results_all, loc_meta[,.(location_id, location_name, super_region_name)], by = "location_id")

for (age in c(4,29)){
  cohort_results <- cohort_results_all[age_end == age]
  counts <- cohort_results[year_id == model_end_year,c('location_id','mean')]
  names(counts) <- c("location_id","mapvar")
  # Add other locations
  other_locs <- loc_meta[level == 3 & !location_id %in% counts$location_id][,.(location_id)]
  other_locs$mapvar <- NA
  counts <- rbind(counts, other_locs, fill = T)
  # Create pattern variable
  counts[location_id %in% men_belt_loc_ids, patternvar := 0]
  plot_list[[as.character(age)]] <- gbd_map(data = counts,
                                            just_menbelt = TRUE,
                                            limits = limit,
                                            labels= label,
                                            col= "Spectral", col.reverse= FALSE,
                                            color_legend = FALSE,
                                            intro_legend = ifelse(age == 4, TRUE, FALSE),
                                            legend.title = paste0("Vaccine Coverage"),
                                            legend.cex = 1,
                                            legend.shift = c(-15, -25),
                                            legend.columns = 10)
}

# Now plot them all together!
graphics.off()
pdf(file = file.path(out_dir, filename), height = 6, width = 18)
plot <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], labels = c('a', 'b', 'c'), common.legend = FALSE, ncol = 3)
# annotate_figure(plot, bottom = text_grob(paste0("Figure ", figure_name, ": Coverage estimates for ", pop_name, " country populations for the meningitis belt, at year end ", model_end_year, ". (a) shows the routine immunization values for the target age, which varies by country from 9 to 18 months. \n (b) shows combined coverage estimates for mass campaigns, catch-up campaigns, and routine immunization coverage combined for ages 1-4. (c) shows analogous combined coverage estimates for ages 1-29."), 
#                                          color = "black", size = 10))
annotate_figure(plot)
dev.off()
