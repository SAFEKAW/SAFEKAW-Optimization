suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(here)
})

# helpers (hist_mix + baseline_irrig_frac + baseline crop mix)
source(here("hpc_opt","R","10_helpers.R"))

# build_precomp()
source(here("hpc_opt","R","32_precomp_build.R"))

years_vec <- 2006:2023

# Load inputs created by 01_common_inputs_make.R
common_input_basin <- read_csv(
  here("hpc_opt","outputs","common_inputs_basin.csv"),
  show_col_types = FALSE
)

df_county <- read_csv(
  here("hpc_opt","outputs","common_inputs_county.csv"),
  show_col_types = FALSE
)

# Build irrigation baseline pieces from county table
hist_mix <- build_hist_mix_from_county(df_county, years_vec)
baseline_irrig_frac <- build_baseline_irrig_frac_from_county(df_county, years_vec)

# Build precomp bundle
precomp <- build_precomp(
  common_input_basin = common_input_basin,
  years_vec = years_vec,
  fert_ref_by_year = NULL,               # uses Management_FertilizerUse_kg from basin if present
  baseline_irrig_frac = baseline_irrig_frac,
  hist_mix = hist_mix
)

# Save it (THIS is the file your feasible-space and optimization scripts should load)
dir.create(here("hpc_opt","outputs","precompute"), recursive = TRUE, showWarnings = FALSE)
saveRDS(precomp, here("hpc_opt","outputs","precompute","precomp_baseline.rds"))

print("Wrote: hpc_opt/outputs/precompute/precomp_baseline.rds")
str(precomp)


baseline_mix <- get_baseline_obs_x(
  here("hpc_opt","outputs","integration","int_crop_areanorm_annual.csv")
)

baseline_mix$obs_tbl   # tidy table (mean shares for all 4 crops)
baseline_mix$obs_x     # numeric vector for eval (corn/soy/sor)
baseline_mix$wheat     # implied wheat share

baseline_policy <- list(
  cult_area_factor   = 1,
  irrig_frac_factor  = 1,
  irr_eff            = 1,
  fert_factor        = 1,
  fert_gamma         = 0.1
)

out <- list(
  obs_tbl = baseline_mix$obs_tbl,
  obs_x = baseline_mix$obs_x,
  wheat = baseline_mix$wheat,
  baseline_policy = baseline_policy
)

dir.create(here("hpc_opt","outputs","precompute"), recursive = TRUE, showWarnings = FALSE)
saveRDS(out, here("hpc_opt","outputs","precompute","baseline_reference.rds"))

# sanity prints LAST
print(out$obs_tbl)
print(out$obs_x)