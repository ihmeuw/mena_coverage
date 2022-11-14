########################################################################################################################################################
#' @ Script name: 03_vet_stgpr
#' @ Purpose: Writing out file that will later be used in plotting and vetting ST-GPR results for MenA routine immunization
#' @ Author:
#' @ Date: 2021-08-07
#' @ Notes: Key file generated here is results_keep.xlsx, which is called later for making Figure 3. 
########################################################################################################################################################

rm(list=ls())

# SOURCE SHARED FUNCTIONS ------------------------------
source("/FILEPATH/utility.r")
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))

main_dir <- "FILEPATH"
source(paste0(main_dir, "/code/helper_functions/campaign_crosswalk.R"))

pacman::p_load(data.table, openxlsx, rhdf5, ggplot2)
run_id <- 201580
model_end_year <- 2021
out_dir <- paste0(main_dir, "/vetting/", run_id, "/")
dir.create(out_dir, showWarnings = F)

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

# For plotting: Get campaign data
data_path <- paste0("/FILEPATH/DataBase All Campaigns MenA 2022 08_corrected.xlsx")
mena_data <- read_excel(data_path, sheet="MenAcampaigns2019 06_published") %>% data.table
mena_data <- campaign_clean(mena_data, loc_meta = loc_meta, pop = pop)

# Assign men_belt_loc_ids to be ever campaign locations
men_belt_loc_ids <- unique(mena_data$location_id)

results <- model_load(run_id, "raked")

# Subset the results to countries who have it in their vaccination schedule
schedule <- read.xlsx(paste0(main_dir, "/Data/merged_schedule.xlsx"))
setnames(schedule, "ISO_code", "ihme_loc_id")
loc_meta <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, decomp_step = "iterative")
schedule <- merge(schedule, loc_meta[,.(ihme_loc_id, location_id, super_region_name)], by = "ihme_loc_id")
schedule <- subset(schedule, year_intro <= model_end_year)

# Allow for USA subnationals with parent_location field
results <- merge(results, loc_meta[,.(ihme_loc_id, location_id, parent_id, level, location_name)], by = "location_id")
results$parent_location <- ifelse(results$level == 3, results$location_id, results$parent_id)
results <- merge(results, schedule, by.x = "parent_location", by.y = "location_id", all.x = T)
results <- results[parent_location %in% schedule$location_id]
results.keep <- results[year_id >= 2005]

# Keep only meningitis belt
results.keep <- results.keep[super_region_name == "Sub-Saharan Africa" | location_name == "Sudan"]

# Coerce to zero before introduction
results.keep <- results.keep[year_id < year_intro, `:=` (gpr_mean = 0, gpr_lower = 0, gpr_upper = 0)]

# Write this out for use later in plotting/vetting code
write.xlsx(results.keep, paste0(main_dir, "/vetting/", run_id, "/results_keep.xlsx"))