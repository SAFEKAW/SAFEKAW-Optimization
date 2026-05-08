# SAFEKAW Optimization Pipeline

This document describes the full modeling and scenario workflow for the SAFEKAW optimization project.

## Overview

The pipeline proceeds in five stages:
0. Get climate data (historical + future) and put into workflow compatible inputs 
1. Fit and save component statistical models
2. Run historical integration
3. Build scenario-specific precompute objects
4. Run deterministic/factorial scenario analyses
5. Summarize and quality-check outputs

The full workflow can be run with:

```bash
Rscript hpc_opt/scripts/run_full_pipeline.R

or in R console by 
source(here::here("hpc_opt", "scripts", "run_full_pipeline.R"))




## Detailed description of scripts 
0. Get climate data (precip, temp, and gdd) from two places
	- 00_make_climate_inputs_gridmet.R for historical period
	- 00_make_climate_inputs_maca for future periods, for each climate model (5) and each scenario (2; RCP 4.5 and 8.5)
		- The individual GCM MACA files are intermediate products!
	- Then use 00_make_climate_ensemble to create single climate files that average climate models 
	- Run these all in 01_common_inputs_make.R using this line in console:
future_scenarios <- c(
  "rcp45_early", "rcp45_mid", "rcp45_late",
  "rcp85_early", "rcp85_mid", "rcp85_late"
)

for (sc in future_scenarios) {

  message("\n==============================")
  message("Running common inputs for: ", sc)
  message("==============================\n")

  scenario_tag <- sc
  gcm_name <- "ensemble"

  source(here::here("hpc_opt", "scripts", "01_common_inputs_make.R"))
}

#THIS IS NOT REPEATED EACH TIME AND ONLY DONE AT ONSET - below is scripts run each time the whole workflow is ran:

1. 01_fit_and_save_models.R

- Fits and saves the component models used throughout the workflow:
	- irrigation model
	- crop yield models
	- water-quality model

- Outputs:
	- hpc_opt/models/irr_lm.rds
	- hpc_opt/models/yield_kg_<Crop>.rds
	- hpc_opt/models/yield_kcal_<Crop>.rds
  	- hpc_opt/models/wq_lm.rds

- Also writes model diagnostics and validation figures to:
	- hpc_opt/outputs/model_checks/


2. 02_integrate_historical_run.R

- Uses the saved models to generate historical integrated outputs.
- Inputs:
	- historical common inputs
	- observed yield data
	- observed nitrate data
 	- saved model .rds files

- Outputs:
	- hpc_opt/outputs/integration/int_crop_areanorm_annual.csv
 	- hpc_opt/outputs/integration/int_basin_annual.csv
 	- hpc_opt/outputs/integration/irr_frac_annual.csv



3. 03_precompute_inputs.R

- Builds scenario-specific precompute bundles for optimization and deterministic runs. Run all from console using this: 
# historical first
scenario_tag <- "hist_baseline"
rm(gcm_name)

source(here::here("hpc_opt", "scripts", "03_precompute_inputs.R"))

# future ensemble
future_scenarios <- c(
  "rcp45_early", "rcp45_mid", "rcp45_late",
  "rcp85_early", "rcp85_mid", "rcp85_late"
)

for (sc in future_scenarios) {
  scenario_tag <- sc
  gcm_name <- "ensemble"

  source(here::here("hpc_opt", "scripts", "03_precompute_inputs.R"))
}

- Inputs:
	- common_inputs_basin_*
	- common_inputs_county_*
	- historical baseline integration outputs

- Outputs:
	- hpc_opt/outputs/precompute/precomp_<scenario>.rds
	- hpc_opt/outputs/precompute/precomp_<gcm>_<scenario>.rds
	- hpc_opt/outputs/precompute/baseline_reference.rds


4. 04_run_factorial_all.R

- Loops across climate scenarios and GCMs and runs deterministic factorial scenarios. Actual engine for this script is 04_run_deterministic_factorial.R

- Inputs:
	- saved model .rds files
 	- precompute bundles
	- scenario configuration files

- Outputs:
	- hpc_opt/outputs/factorial_runs/...

- run with:
source(here::here("hpc_opt", "scripts", "04_run_factorial_all.R"))


5. 05_check_deterministic_factorial.R

- Reads deterministic/factorial outputs and generates summary tables and plots.

- Outputs:
	- scenario summaries
	- normalized comparisons
	- diagnostic plots
