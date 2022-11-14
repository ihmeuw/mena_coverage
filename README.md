# Meningococcal A Vaccine Coverage in the Meningitis Belt

Instructions for run:
  * The four numbered scripts should be run sequentially. The `helper_functions` folder contains contain functions that are called by the four sequential scripts and do not need to be run directly; they are marked by sub-bullets. The `figure_code` folder contains scripts that can be run individually to create each figure.

1. `01_data_prep.R` 
  * First, run `01_data_prep.R`. This script takes the extraction for routine immunization data from the systematic review & preps it for ST-GPR. This prep includes ensuring that only first-dose rows for location-years that we are modeling for are kept in the dataset. It also caps coverage >1 at 0.999 and sets variance and sample size when that information is not available as described in the methods. Additionally, it performs transformations sourcing the following scripts:
    * `ri_crosswalk.R` - this script contains the crosswalk_mena function, which crosswalks the routine immunization data - admin to the level of survey, as described in the methods.

2. `02_launch_stgpr.R`
  * Second, run `02_launch_stgpr.R`. This script runs ST-GPR. Make sure to update your config with your correct input data path, description, & settings. 
    * `config.csv` - the config file that contains the parameters used for final ST-GPR - is called by ST-GPR during modeling process.

3. `03_vet_stgpr.R`
  * Third, run `03_vet_stgpr.R`. This script generates results_keep.xlsx, which is a single spreadsheet of the routine immunization ST-GPR results for just the location-years of interest, for plotitng purposes in the `figure_code` later.

4. `04_cohort_model_draws.R`
  * Fourth, run `04_cohort_model_draws.R`. This is the "main" script that runs the cohorting model. Make sure to update the denominator of choice ("hi_risk" or "all"), model end year, run_id (ST-GPR version), path to campaign data, and today's date (if set manually) at the top of the script. This script starts by crosswalking campaign data using several functions from the following script:
    * `campaign_crosswalk.R` - this script contains the `campaign_crosswalk` function that adjusts admin data from campaigns to the level of survey; it also includes three helper functions to prep the data prior to crosswalk.
  * Once the crosswalked campaign data and the ST-GPR routine immunization results are prepped, the cohorting model is initialized and run for each country being modeled. These calculations are performed and saved at the draw level. 