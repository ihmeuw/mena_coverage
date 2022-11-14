########################################################################################################################################################
#' @ Script name: figure_2
#' @ Purpose: Generate maps of MenA routine and campaign schedule
#' @ Author: Rose Bender
#' @ Date: 2021-08-07
#' @ Notes:
########################################################################################################################################################

rm(list=ls())

pacman::p_load(data.table, openxlsx, ggplot2, gridExtra, ggpubr)

main_dir <- "FILEPATH"
source(paste0(main_dir, "/code/helper_functions/gbd_map_custom.R"))
source(paste0(main_dir, "/code/helper_functions/campaign_crosswalk.R"))

# SOURCE SHARED FUNCTIONS ------------------------------
invisible(sapply(list.files("/FILEPATH/", full.names = T), source))

# Set output directory
out_dir <- file.path(main_dir, "figures", "second_resubmission")
filename <- "figure_2.pdf"
model_end_year <- 2021

# Read in schedule
dt <- as.data.table(read.xlsx(paste0(main_dir, "/Data/merged_schedule.xlsx")))

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

# Get campaign data
data_path <- paste0("/FILEPATH/DataBase All Campaigns MenA 2022 08_with_new_data.xlsx")
mena_data <- read_excel(data_path, sheet="MenAcampaigns2019 06_published") %>% data.table
mena_data <- campaign_clean(mena_data, loc_meta, pop)

# Assign men_belt_loc_ids to be ever campaign locations
men_belt_loc_ids <- unique(mena_data$location_id)

# Prep
setnames(dt, c("ISO_code", "year_intro"), c("ihme_loc_id", "mapvar"))
dt$mapvar <- as.numeric(dt$mapvar)
dt <- merge(dt, loc_meta[,.(location_id, ihme_loc_id)], by = "ihme_loc_id")
# Subset to meningitis belt
dt <- dt[location_id %in% men_belt_loc_ids]
# Add other locations
other_locs <- loc_meta[level == 3 & !location_id %in% dt$location_id][,.(location_id)]
other_locs$mapvar <- NA
dt <- rbind(dt, other_locs, fill = T)
# Create pattern variable
dt[location_id %in% men_belt_loc_ids, patternvar := 0]

# Plot using MODIFIED GBD map function
limit <- c(0, 2010.9, 2011.9, 2012.9, 2013.9, 2014.9, 2015.9, 2016.9, 2017.9, 2018.9, 2019.9, 2020.9, 2021.9)
label <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

plot_list <- list()
plot_list[[1]] <- gbd_map(data = dt,
                          just_menbelt = TRUE,
                          limits = limit,
                          labels= label,
                          col= "YlGnBu", col.reverse= FALSE,
                          color_legend = FALSE,
                          intro_legend = FALSE,
                          legend.title = "Year",
                          legend.cex = 1,
                          legend.shift = c(0,0),
                          legend.columns = 10)

# make an analogous plot for year of campaign

# Prep
setnames(mena_data, c("year_start"), c("mapvar"))
mena_data$mapvar <- as.numeric(mena_data$mapvar)
# Subset to meningitis belt
mena_data <- mena_data[location_id %in% men_belt_loc_ids]
# Add other locations
other_locs <- loc_meta[level == 3 & !location_id %in% mena_data$location_id][,.(location_id)]
other_locs$mapvar <- NA
mena_data <- rbind(mena_data, other_locs, fill = T)
# Create pattern variable
mena_data[location_id %in% men_belt_loc_ids, patternvar := 0]

# Print first campaign
dt <- copy(mena_data[year_id <= model_end_year , .SD[which.min(mapvar)], by = location_id])
plot_list[[2]] <- gbd_map(data = dt,
                          just_menbelt = TRUE,
                          limits = limit,
                          labels= label,
                          col= "YlGnBu", col.reverse= FALSE,
                          color_legend = TRUE,
                          intro_legend = FALSE,
                          legend.title = "Year",
                          legend.cex = 1,
                          legend.shift = c(5, -16),
                          legend.columns = 12)
dt <- copy(mena_data[year_id <= model_end_year , .SD[which.max(mapvar)], by = location_id])
plot_list[[3]] <- gbd_map(data = dt,
                          just_menbelt = TRUE,
                          limits = limit,
                          labels= label,
                          col= "YlGnBu", col.reverse= FALSE,
                          color_legend = FALSE,
                          intro_legend = TRUE,
                          legend.cex = 1,
                          legend.shift = c(-15, -25),
                          legend.columns = 12)

# Now plot them all together!
graphics.off()
pdf(file = file.path(out_dir, filename), height = 6, width = 18)
plot <- ggarrange(plot_list[[2]], plot_list[[3]], plot_list[[1]], labels = c('a', 'b', 'c'), common.legend = FALSE, ncol = 3)
# annotate_figure(plot, bottom = text_grob("Figure 2: Meningococcal A containing conjugate vaccine (MACV) in the meningitis belt, by year of introduction. (a) shows the year of the first mass campaign. \n (b) shows shows the year of the most recent campaign (mass or catchup). (c) shows the year of introduction into routine immunization schedules.", 
#                                          color = "black", size = 10))
annotate_figure(plot)
dev.off()
