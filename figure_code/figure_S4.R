########################################################################################################################################################
#' @ Script name: figure_S4
#' @ Purpose: Explore coverage v. disease burden pre-MenAfriVac
#' @ Author: Rose Bender
#' @ Date: 2021-08-07
#' @ Notes:
########################################################################################################################################################

rm(list=ls())

# SOURCE SHARED FUNCTIONS ------------------------------
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))

# Set output directory
out_dir <- file.path(main_dir, "figures", "second_resubmission")

### GBD mapping function
main_dir <- "FILEPATH"
source(paste0(main_dir, "/code/helper_functions/gbd_map_custom.R"))
source(paste0(main_dir, "/code/helper_functions/campaign_crosswalk.R"))

# Load packages
pacman::p_load(data.table, openxlsx, rhdf5, ggplot2, readxl, ggpubr, cowplot)

# Define versions
run_id <- 201580
denom <- "hi_risk"
result_date <- "2022_09_04"

filename <- paste0("figure_S4.pdf")

# Set output directory
out_dir <- file.path(main_dir, "figures", "second_resubmission")

# Pull necessary shared function objects
# Locations
loc_meta <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, decomp_step = "iterative")
# Single year population from 1 to 29
pop <- get_population(age_group_id = c(238,50:77), 
                      year = 2005:2020, 
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

# Select the max coverage
cohort_results_max <- cohort_results_all[, .SD[which.max(mean)], by=c("location_id", "age_group_name")]

# Get pre-MenAfriVac disease burden
burden <- get_outputs(topic = "rei",
                      rei_id = 386,
                      measure_id = 1,
                      metric_id = 3,
                      version = "best",
                      location_id = unique(cohort_results_all$location_id),
                      sex_id = 3,
                      year_id = 2009,
                      age_group_id = 1,
                      release_id = 6)
burden$val <- burden$val*100000
burden$logval <- log(burden$val)

cohort_results_max <- merge(cohort_results_max, burden, by = c("location_id", "location_name", "sex_id"))

# Create the plot & assign to an object
plot <- ggplot(data = cohort_results_max, aes(x = logval, y = mean)) + 
  geom_point() + labs(color = "Country") +
  facet_wrap(~age_group_name.x, nrow = 4) + theme_bw() + ylab("Peak MenAfriVac Coverage, 2011-2021") + xlab("Log pre-MenAfriVac meningococcal meningitis under 5 mortality rates per 100K, 2009") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_smooth(method = "lm") +
  stat_regline_equation(label.y = 0.5, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 0.4, aes(label = ..rr.label..))

# write it out!
graphics.off()
pdf(file = file.path(out_dir, filename), height = 8.5, width = 11, onefile=F)
print(plot)
dev.off()
