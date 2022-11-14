########################################################################################################################################################
#' @ Script name: ri_crosswalk
#' @ Purpose: Crosswalk script for MenA routine immunization data extraction
#' @ Author:
#' @ Date: 2021-08-07
#' @ Notes: 
########################################################################################################################################################

pacman::p_load(data.table, ggplot2, readxl, dplyr, stringr)
# SOURCE SHARED FUNCTIONS ------------------------------

crosswalk_mena <- function(coefficient_path, data){
  # Read in previously-generated coefficients for all 4 GBD modeled vaccines
  coeffs <- as.data.table(readRDS(coefficient_path))
  
  # Get the meningitis belt locs (for plotting)
  men_belt_loc_ids <- c(522, 169, 171, 175, 178, 179, 180, 185, 189, 435, 190, 200, 201, 202, 204, 205, 206, 207, 208, 209, 211, 212, 213, 214, 216, 218)

  # MenA was introduced only in 2005, so subset to 2005 and later
  coeffs <- coeffs[year_id >= 2005]
  # Keep only national level
  coeffs <- coeffs[!ihme_loc_id %like% "_"]
  loc_meta <- fread(paste0("FILEPATH", "locations_2020.csv"))
  coeffs <- merge(coeffs, loc_meta[,.(ihme_loc_id, location_id, location_name, super_region_name)], by = "ihme_loc_id")
  locations <- copy(loc_meta)
  
  # Average across the 4 vaccines in linear space
  coeffs_avg <- coeffs[, .(cv_admin_bias_ratio = mean(cv_admin_bias_ratio)), by = c("ihme_loc_id", "year_id", "sex_id", "age_group_id")]
  
  # Average across the 4 vaccines in log space
  coeffs[, log_ratio := log(cv_admin_bias_ratio)]
  coeffs_log_avg <- coeffs[, .(cv_admin_bias_ratio = exp(mean(log_ratio))), by = c("ihme_loc_id", "year_id", "sex_id", "age_group_id")]
  
  # Copy original unadjusted mean
  data[, unadjusted := val]
  
  # Find which data are the admin data
  to_adjust <- data[cv_admin == 1]
  
  # Merge with coefficients
  to_adjust <- merge(to_adjust, coeffs_avg[,.(ihme_loc_id, year_id, cv_admin_bias_ratio)], by = c("ihme_loc_id", "year_id"))
  
  # Apply coefficients
  to_adjust[, val := cv_admin_bias_ratio*val]
  to_adjust[, note_modeler := paste(note_modeler, 
                                    "| adjusted for admin bias using admin bias adjustment ratio from GBD 2020 for this location-year averaged across 4 vaccines:", 
                                    cv_admin_bias_ratio)]
  
  data <- rbind(data[cv_admin == 0], to_adjust, fill = T)

  # Done!
  return(data)
}
