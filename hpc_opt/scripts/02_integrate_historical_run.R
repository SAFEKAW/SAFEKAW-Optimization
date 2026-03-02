# hpc_opt/scripts/02_integrate_historical_run.R
source(here::here("hpc_opt","R","10_helpers.R"))
source(here::here("hpc_opt","R","12_irrigation_predict.R"))
source(here::here("hpc_opt","R","13_yield_predict.R"))
source(here::here("hpc_opt","R","14_wq_features.R"))
source(here::here("hpc_opt","R","15_wq_predict.R"))
source(here::here("hpc_opt","R","16_net_returns.R"))
source(here::here("hpc_opt","R","11_models_load.R"))
source(here::here("hpc_opt","R","20_integrate_historical.R"))

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
})

yrs_common <- 2006:2023

# Inputs (use your original data folder for now)
df_combined_county <- read_csv(here("hpc_opt","outputs","common_inputs_county.csv"), show_col_types = FALSE)
common_input_basin <- read_csv(here("hpc_opt","outputs","common_inputs_basin.csv"), show_col_types = FALSE)
df_yield_obs       <- read_csv(here("data","CropYieldData_County.csv"), show_col_types = FALSE)
df_nitrate_obs     <- read_csv(here("data","RiverNitrateData_EKSRB.csv"), show_col_types = FALSE)

# Models (pre-fit + saved)
models <- load_models(here("hpc_opt","models"))

# Economics params (same as your script)
universal_costs <- list(irr_cost_per_m3 = 0.029)

crop_params <- tibble::tribble(
  ~Crop,      ~income_per_kg, ~direct_cost_per_kg, ~fixed_cost_per_kg, ~total_cost_per_kg, ~fert_kgHa,
  "Wheat",     0.2,           0.12,                0.05,               0.17,               90,
  "Corn",      0.18,          0.11,                0.05,               0.15,              250,
  "Sorghum",   0.17,          0.09,                0.06,               0.15,              110,
  "Soybeans",  0.37,          0.18,                0.14,               0.32,               55
) %>% mutate(net_return_per_kg = income_per_kg - total_cost_per_kg)

# Run
res <- integrate_historical(
  df_combined_county, common_input_basin, df_yield_obs, df_nitrate_obs,
  models = models, crop_params = crop_params, universal_costs = universal_costs,
  yrs_common = yrs_common
)

# Write outputs (HPC-safe)
outdir <- here("hpc_opt","outputs","integration")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

write_csv(res$crop_basin_perha, file.path(outdir, "int_crop_areanorm_annual.csv"))
write_csv(res$basin_totals,     file.path(outdir, "int_basin_annual.csv"))
write_csv(res$irr_frac_YR,      file.path(outdir, "irr_frac_annual.csv"))

message("Wrote integration outputs to: ", outdir)
