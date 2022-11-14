########################################################################################################################################################
#' @ Script name: 01_data_prep
#' @ Purpose: Data prep script for MenA routine immunization data extraction
#' @ Author: 
#' @ Date: 2021-08-07
#' @ Notes: This script also applies the crosswalk to the routine immunization data by calling crosswalk_mena
########################################################################################################################################################

rm(list=ls())

pacman::p_load(data.table, openxlsx, dplyr, readr)
main_dir <- "FILEPATH"
source(paste0(main_dir, "/code/helper_functions/ri_crosswalk.R"))
source(paste0(main_dir, "/code/helper_functions/campaign_crosswalk.R"))
date <- gsub("-", "_", Sys.Date())

# set the last year you want to model for
model_end_year <- 2021

# load in data
extraction <- as.data.table(read.xlsx(paste0(main_dir, "/Extractions/Extraction_2022_09_04.xlsx")))

# Outlier any rows of data for second dose or more
extraction[doses.given %like% "2"|doses.given %like% "two",`:=` (is_outlier = 1, outlier_reason = "second dose")]

# Outlier any rows of data for countries without RI
merged.schedule <- read.xlsx(paste0(main_dir, "/Data/merged_schedule.xlsx"))
setnames(merged.schedule, "ISO_code", "ihme_loc_id")
# Allow for USA subnationals with parent_location field
extraction <- merge(extraction, merged.schedule, by.x = "parent_location", by.y = "ihme_loc_id", all.x = T)
extraction$parent_location <- substr(extraction$ihme_loc_id, 1, 3)
extraction <- extraction[!parent_location %in% merged.schedule$ihme_loc_id, `:=` (is_outlier = 1, outlier_reason = "not an RI country")]

# Outlier any rows of data for countries where vaccine was not yet introduced into schedule
extraction[, year_id := floor((year_start+year_end)/2)]
extraction[year_id < year_intro, `:=` (is_outlier = 1, outlier_reason = "year of data before vaccine in schedule")]
extraction[year_id < year_intro & ihme_loc_id == "CHE", `:=` (is_outlier = 1, outlier_reason = "CHE used MenC at time of this data source")]

# Outlier OFFICIAL data in favor of ADMIN from the JRF source
# Reason: Admin data is more complete than official data in MenBelt countries, admin data is already being crosswalked to the level of survey, & for consistency
extraction[nid == 483802, `:=` (is_outlier = 1, outlier_reason = "outlier JRF official and keep JRF admin")]

# merge on location id
loc_meta <- fread(paste0("FILEPATH", "locations_2020.csv"))
extraction  <- merge(extraction, loc_meta[,.(ihme_loc_id, location_id, location_name, super_region_name)], by = "ihme_loc_id")

# Run a both-sex model
# Keep only both-sex rows, except for sources that have only single-sex data available
extraction_both_sex <- extraction[sex == "both"]
extraction_single_keep <- extraction[sex != "both" & !nid %in% extraction_both_sex$nid]
# In that case, average the single sex data. 
extraction_single_keep[, `:=` (vacc_mena=mean(vacc_mena), sample_size=mean(sample_size)), by = c("location_id", "year_start","year_end", "age_start","age_end")]
extraction_single_keep$sex <- "both"
extraction_single_keep <- unique(extraction_single_keep)
extraction <- rbind(extraction_both_sex, extraction_single_keep)
extraction[sex == "both", sex_id := 3]

# temporarily set NID
extraction$nid <- as.numeric(extraction$nid)
extraction[is.na(nid), nid := 9999]

# age_group_id
# Deal with sources that have more than 1 age group per location-year
extraction[, age_group_id := 22]

# measure_id
extraction[, measure_id := 18] # proportion
extraction[, measure := "proportion"]

# val, 
setnames(extraction, "vacc_mena", "val")
extraction[, val := val/100]

# Now apply the crosswalk!
coefficient_path <- paste0("FILEPATH", "/hybridized_bias.rds")
extraction <- crosswalk_mena(coefficient_path = coefficient_path, data = extraction)

## Offset coverage: Cap values of > 1 at 0.999, and offset zeros to calculate variance
extraction[val >= 1, val := 0.999]
extraction[val <= 0, val := 0.001]

# Set variance and sample size after the crosswalk to deal with means > 1 and means == 0 so no variance < -1 or == 0
# based on survey data availability by country-year-vaccine - no points have any survey data for the country-year-vaccine combination that went into the xwalk
# Where do these 50 and 100 numbers come from? 
# Use 10 in locations where we do have survey data, and 50 in locations where we don't have survey data. 
# Use an artificial sample size to calculate the variance for the admin data, because otherwise population-based denominator ST-GPR would only follow admin data (want to give preference to the survey data where we have it).
# The data with 100 as the sample size is likely all survey report (aka "lit") data, and we assume a sample size of 100 when not provided in the report
extraction <- extraction[cv_admin==1, variance := val * (1 - val) / 50]
# Set lit variance where we have sample_size
extraction[!is.na(sample_size) & is.na(variance), variance := val * (1 - val) / sample_size]
# Set lit variance where we dont have sample_size
extraction[(cv_admin == 0) & is.na(variance), variance := val * (1 - val) / 100]
# Set sample_size based off of variance
extraction[!is.na(val) & is.na(sample_size), sample_size := val * (1 - val) / variance]

# Drop NA values
extraction <- extraction[!is.na(val) & !(is.na(variance))]

# Drop values after end modeling
extraction[year_id > model_end_year, is_outlier := 1]

write.csv(extraction, paste0(main_dir, "/Extractions/Extraction_crosswalked_", date, ".csv"), row.names = F)

# Keep only meningitis belt
data_path <- paste0("FILEPATH", "/DataBase All Campaigns MenA 2022 04_corrected.xlsx")
mena_data <- read_excel(data_path, sheet="MenAcampaigns2019 06_published") %>% data.table
mena_data  <- merge(mena_data, loc_meta[,.(ihme_loc_id, location_id, location_name, super_region_name)], by = "ihme_loc_id")
extraction[!location_id %in% unique(mena_data$location_id), `:=` (is_outlier = 1, outlier_reason = "outlier non meningitis belt locations")]
write.csv(extraction, paste0(main_dir, "/Extractions/MenBeltOnly_Extraction_crosswalked_", date, ".csv"), row.names = F)