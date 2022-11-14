########################################################################################################################################################
#' @ Script name: figure_4
#' @ Purpose: Time series of cohort model results by age group
#' @ Author: Rose Bender
#' @ Date: 2021-08-07
#' @ Notes:
########################################################################################################################################################

rm(list=ls())

# SOURCE SHARED FUNCTIONS ------------------------------
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))

# Helper functions
main_dir <- "FILEPATH"
source(paste0(main_dir, "/code/helper_functions/campaign_crosswalk.R"))

# Load packages
pacman::p_load(data.table, openxlsx, rhdf5, ggplot2, readxl, ggpubr, cowplot)

# Define versions
run_id <- 201580
result_date <- "2022_09_04"
denom <- "hi_risk"
model_end_year <- 2021

if(denom == "hi_risk"){
  figure_name <- "4"
  pop_name <- "high risk"
} else if (denom == "all"){
  figure_name <- "S6"
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

# Load results from cohorting model
cohort_results_all <-  as.data.table(read.xlsx(paste0(main_dir, "/vetting/", run_id, "_", result_date, "/", "denom_", denom, "_cohorting_results_summary.xlsx")))
cohort_results_all <- merge(cohort_results_all, loc_meta[,.(location_id, location_name, super_region_name)], by = "location_id")

# Set age group name
cohort_results_all$age_group_name <- ifelse(cohort_results_all$age_end == 4, "1 to 4", "1 to 29")

# Create the plot & assign to an object
plot <- ggplot(data = cohort_results_all[year_id <= model_end_year], aes(x = as.integer(year_id), color = age_group_name)) + 
  geom_line(aes(y = mean)) + labs(color = "Age Group") +
  facet_wrap(~location_name, nrow = 4) + scale_x_continuous(name="Year", breaks = c(2010, 2015, 2020)) + theme_bw() + ylab("Coverage") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, linetype = 0) + theme(axis.text.x = element_text(angle = 45, hjust=1))

# write it out!
graphics.off()
pdf(file = file.path(out_dir, filename), height = 6, width = 18, onefile=F)
# annotate_figure(plot, bottom = text_grob(paste0("Figure ", figure_name, ": Coverage estimates for ", pop_name, " country populations for the meningitis belt, for 1 to 29 and 1 to 4 age groups, from 2010 to ", model_end_year, ". Year-end coverage values shown for each year."), 
#                                          color = "black", size = 10))
annotate_figure(plot)
dev.off()
