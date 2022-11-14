########################################################################################################################################################
#' @ Script name: 02_launch_stgpr
#' @ Purpose: ST-GPR launch script for MenA routine immunization ST-GPR model
#' @ Author:
#' @ Date: 2021-08-07
#' @ Notes: This script automatically reads and writes config file
########################################################################################################################################################

# Running an ST-GPR model -------------------------------------------------

# Set working directory to where the ST-GPR launch code lives and source functions
central_root <- "FILEPATH"
setwd(central_root)
source("FILEPATH/register.R")  # register_stgpr_model
source("FILEPATH/sendoff.R")  # stgpr_sendoff
source("FILEPATH/utility.r")  # check_run

main_dir <- "FILEPATH"

# Register and launch ST-GPR model
run_id <- register_stgpr_model(paste0(main_dir, "/code/config.csv"),
                               model_index_id = 1)
# Write the config in the tracking file
config <- read.csv(paste0(main_dir, "/code/config.csv"))
# Send off STGPR model
stgpr_sendoff(run_id, "proj_tb", log_path = "FILEPATH")

# Check model status every minute (so we don't overwhelm the db!) until it finished
status <- check_run(run_id)
while (status == 2) {
  cat("Model still running! Waiting a minute...\n")
  Sys.sleep(60)
  status <- check_run(run_id, verbose = TRUE)
}
if (status == 0) print("Model failed :( ")
if (status == 1) print("Model finished!")

# Save config
write.csv(config, paste0(main_dir, "/version_tracking/config_", run_id, ".csv"))
# Save fit statistics
fit_stat <- read.csv(file.path("FILEPATH", run_id, "fit_stats.csv"))
write.csv(fit_stat, paste0(main_dir, "/version_tracking/fit_stats_", run_id, ".csv"), row.names = F)
