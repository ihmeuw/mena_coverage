########################################################################################################################################################
#' @ Script name: figure_S1_to_S3
#' @ Purpose: Crosswalk process vetting plots
#' @ Author: Rose Bender
#' @ Date: 2021-08-07
#' @ Notes:
########################################################################################################################################################

rm(list=ls())

main_dir <- "FILEPATH"
model_end_year <- 2021

# Set output directory
out_dir <- file.path(main_dir, "figures", "second_resubmission")

# SOURCE SHARED FUNCTIONS ------------------------------
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))
pacman::p_load(data.table, openxlsx, dplyr, readr, readxl, stringr, ggplot2, gridExtra, grid, lattice)

coefficient_path <- paste0("/FILEPATH/hybridized_bias.rds")
coeffs <- readRDS(coefficient_path)
setDT(coeffs)

# Explore the coefficients
summary(coeffs)

# Formatting
coeffs$me_name <- substr(toupper(coeffs$me_name), 6, nchar(coeffs$me_name))
setnames(coeffs, c("me_name"), c("Vaccine"))

# Look only at meningitis belt countries, with vaccine in schedule in 2019 or earlier
schedule <- read.xlsx(paste0(main_dir, "/Data/merged_schedule.xlsx"))
schedule <- subset(schedule, WHO_REGION == "AFR" | ISO_code == "SDN")
schedule <- subset(schedule, year_intro <= model_end_year)
coeffs <- coeffs[ihme_loc_id %in% schedule$ISO_code]

# Subset only to years of RI in men belt
coeffs <- coeffs[year_id >= 2016 & year_id <= model_end_year]

# loc_meta <- get_location_metadata(location_set_id = 35, gbd_round_id = 7, decomp_step = "iterative")
loc_meta <- fread(paste0("FILEPATH/locations_2020.csv"))
coeffs <- merge(coeffs, loc_meta[,.(ihme_loc_id, location_id, location_name, super_region_name)], by = "ihme_loc_id")
locations <- copy(loc_meta)

# Average across the 4 vaccines in linear space
coeffs_avg <- coeffs[, .(cv_admin_bias_ratio = mean(cv_admin_bias_ratio)), by = c("ihme_loc_id", "year_id", "sex_id", "age_group_id")]

# Average across the 4 vaccines in log space
coeffs[, log_ratio := log(cv_admin_bias_ratio)]
coeffs_log_avg <- coeffs[, .(cv_admin_bias_ratio = exp(mean(log_ratio))), by = c("ihme_loc_id", "year_id", "sex_id", "age_group_id")]

# Plots 

coeffs_wide <- merge(coeffs_avg, coeffs, by = c("ihme_loc_id", "year_id", "age_group_id", "sex_id"))

pdf(file = paste0(out_dir, "/figure_S1.pdf"), width = 11, height = 8.5)
# Compare the coefficients to each other, and compare to the linear and log space averages
p <- ggplot(data = coeffs_wide) + geom_point(aes(x = year_id, y = cv_admin_bias_ratio.y, color = Vaccine)) + 
  geom_line(aes(x = year_id, y = cv_admin_bias_ratio.x)) + 
  xlab("Year") + ylab(paste("Bias Adjustment Coefficient Value")) +
  facet_wrap(~location_name) + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust=1))
print(p)
dev.off()

# Compare the individual vaccine coefficients to the average coefficient 
pdf(file = paste0(out_dir, "/figure_S2.pdf"), width = 11, height = 8.5)
p2 <- ggplot(data = coeffs_wide[year_id == 2021]) + geom_text(aes(x = cv_admin_bias_ratio.x, y = cv_admin_bias_ratio.y, label = ihme_loc_id)) + 
  facet_wrap(~Vaccine) + theme_bw() + xlab("Averaged Coefficient") + ylab(paste("Vaccine Specific Coefficient")) + 
  geom_abline(slope = 1, intercept = 0) + geom_abline(slope = 1, intercept = 0)
print(p2)
dev.off()

# load in data
extraction <- fread(paste0(main_dir, "/Extractions/MenBeltOnly_Extraction_crosswalked_2022_09_04.csv"))

# Look at included data only - ALSO keep official data for plotting 
extraction <- extraction[is_outlier == 0 | is.na(is_outlier) | nid == 483802]
extraction[, location_year := paste(ihme_loc_id, year_start, sep = "_")]
official <- extraction[nid == 483802]
admin <- extraction[nid == 483803]

setdiff(admin$location_year, official$location_year)
setdiff(official$location_year, admin$location_year)
intersect(official$location_year, admin$location_year)

# For those matched pairs which do exist, what is the ratio of admin:official?
intersect <- merge(official[,.(ihme_loc_id, year_start, val, unadjusted)], admin[,.(ihme_loc_id, year_start, val, unadjusted)], by = c("ihme_loc_id", "year_start"))
setnames(intersect, c("unadjusted.x","unadjusted.y"), c("Official","Admin"))
intersect[, ratio := Official/Admin]

# Export it!

# Plot adjusted v unadjusted
# Compare the individual vaccine coefficients to the average coefficient 
pdf(file = paste0(out_dir, "/figure_S3.pdf"), width = 13.5, height = 4.5)
p1 <- ggplot(data = intersect) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_text(aes(x = Admin, y = Official,label = ihme_loc_id)) +
  theme_bw() + xlab("Admin (reported)") + ylab("Country Official (reported)") +
  ggtitle("a") + theme(plot.margin = margin(1,1,1.5,1.2, "cm"))
p2 <- ggplot(data = admin) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_text(aes(x = unadjusted, y = val,label = ihme_loc_id)) +
  theme_bw() + xlab("Admin (reported)") + ylab("Admin (adjusted)") +
  ggtitle("b") + theme(plot.margin = margin(1,1,1.5,1.2, "cm"))
p3 <- ggplot(data = intersect) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_text(aes(x = Official, y = val.y,label = ihme_loc_id)) +
  theme_bw() + xlab("Country Official (reported)") + ylab("Admin (adjusted)") +
  ggtitle("c") + theme(plot.margin = margin(1,1,1.5,1.2, "cm")) 
grid.arrange(p1, p2, p3, ncol=3)
dev.off()
