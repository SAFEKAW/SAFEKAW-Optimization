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

# ---- choose scenario ----
#scenario_tag <- "hist_baseline"
# scenario_tag <- "rcp45_early"
#scenario_tag <- "rcp45_mid"
 #scenario_tag <- "rcp45_late"
#scenario_tag <- "rcp85_early"
 #scenario_tag <- "rcp85_mid"
#scenario_tag <- "rcp85_late"

# ---- choose scenario ----
if (!exists("scenario_tag")) {
  scenario_tag <- "hist_baseline"
}

if (!exists("gcm_name")) {
  gcm_name <- ifelse(scenario_tag == "hist_baseline", NA_character_, "ensemble")
}

message("Using scenario_tag = ", scenario_tag)
message("Using gcm_name    = ", gcm_name)

# ---- year windows ----
if (scenario_tag == "hist_baseline") {
  years_vec <- 2006:2023
} else if (scenario_tag == "rcp45_early") {
  years_vec <- 2025:2049
} else if (scenario_tag == "rcp45_mid") {
  years_vec <- 2050:2074
} else if (scenario_tag == "rcp45_late") {
  years_vec <- 2075:2099
} else if (scenario_tag == "rcp85_early") {
  years_vec <- 2025:2049
} else if (scenario_tag == "rcp85_mid") {
  years_vec <- 2050:2074
} else if (scenario_tag == "rcp85_late") {
  years_vec <- 2075:2099
} else {
  stop("Unknown scenario_tag: ", scenario_tag)
}

# ---- input/output paths ----
is_future <- scenario_tag != "hist_baseline"

if (!is_future) {
  in_basin  <- here("hpc_opt", "outputs", paste0("common_inputs_basin_", scenario_tag, ".csv"))
  in_county <- here("hpc_opt", "outputs", paste0("common_inputs_county_", scenario_tag, ".csv"))
  out_precomp <- here("hpc_opt", "outputs", "precompute", paste0("precomp_", scenario_tag, ".rds"))
} else {
  in_basin  <- here("hpc_opt", "outputs", paste0("common_inputs_basin_", gcm_name, "_", scenario_tag, ".csv"))
  in_county <- here("hpc_opt", "outputs", paste0("common_inputs_county_", gcm_name, "_", scenario_tag, ".csv"))
  out_precomp <- here("hpc_opt", "outputs", "precompute", paste0("precomp_", gcm_name, "_", scenario_tag, ".rds"))
}

out_baseline_reference <- here("hpc_opt", "outputs", "precompute", "baseline_reference.rds")

dir.create(here("hpc_opt","outputs","precompute"), recursive = TRUE, showWarnings = FALSE)

needed_files <- c(in_basin, in_county)
missing_files <- needed_files[!file.exists(needed_files)]

if (length(missing_files) > 0) {
  stop("Missing input files:\n", paste(" -", missing_files, collapse = "\n"))
}

# ---- load scenario-specific common inputs ----
common_input_basin <- read_csv(in_basin, show_col_types = FALSE)
df_county <- read_csv(in_county, show_col_types = FALSE)

# For irrigation references, always use historical water-use years
irrig_ref_years <- 2006:2023

# The evaluator's cultivated-area denominator is the historical modeled
# LC_cult_base. Direct CDL denominators for both geographic domains are also
# retained in irrigation_reference for reporting and future constraints.
if (scenario_tag == "hist_baseline") {
  lu_for_irrig_ref <- common_input_basin %>%
    transmute(
      Year,
      LC_cult_base = `LandCover_m2_all_cult`
    )
} else {
  precomp_hist_ref <- readRDS(
    here("hpc_opt", "outputs", "precompute", "precomp_hist_baseline.rds")
  )
  
  lu_for_irrig_ref <- precomp_hist_ref$lu_baseline
}

irrigation_reference <- build_dual_domain_irrigation_reference(
  wu_basin = read_csv(here("data", "WaterUseByCrop_EKSRB.csv"), show_col_types = FALSE),
  wu_alluvial = read_csv(
    here("data", "WaterUseByCrop_AlluvialCorridor.csv"), show_col_types = FALSE
  ),
  lu_basin = read_csv(here("data", "LandCoverData-CDL_EKSRB.csv"), show_col_types = FALSE),
  lu_alluvial = read_csv(
    here("data", "LandCoverData-CDL_AlluvialCorridor.csv"), show_col_types = FALSE
  ),
  model_basin_cultivated = lu_for_irrig_ref,
  years_vec = irrig_ref_years
)

baseline_irrig_frac <- irrigation_reference$evaluator$baseline_irrig_frac
hist_mix <- irrigation_reference$evaluator$hist_mix

message(
  "Evaluator baseline irrigated fraction (basin four-crop area / model cultivated area) = ",
  round(baseline_irrig_frac, 4)
)
print(hist_mix)
# ---- build precomp bundle ----
precomp <- build_precomp(
  common_input_basin = common_input_basin,
  years_vec = years_vec,
  fert_ref_by_year = NULL,               # uses Management_FertilizerUse_kg from basin if present
  baseline_irrig_frac = baseline_irrig_frac,
  hist_mix = hist_mix,
  irrigation_reference = irrigation_reference
)

if (anyNA(precomp$lu_baseline$LC_cult_base) ||
    anyNA(precomp$lu_baseline$LC_dev_base) ||
    anyNA(precomp$lu_baseline$LC_total)) {
  stop("precomp lu_baseline contains NA land-cover baseline values for scenario_tag = ", scenario_tag)
}

saveRDS(precomp, out_precomp)

message("Wrote precomp: ", out_precomp)
str(precomp)

# ---- baseline reference should only come from historical baseline ----
if (scenario_tag == "hist_baseline") {
  write_csv(
    irrigation_reference$totals_by_year,
    here("hpc_opt", "outputs", "precompute", "irrigation_reference_totals_by_domain_year.csv")
  )
  write_csv(
    irrigation_reference$crop_by_year,
    here("hpc_opt", "outputs", "precompute", "irrigation_reference_by_domain_crop_year.csv")
  )
  write_csv(
    irrigation_reference$crop_summary,
    here("hpc_opt", "outputs", "precompute", "irrigation_reference_by_domain_crop_summary.csv")
  )

  baseline_mix <- get_baseline_obs_x(
    here("hpc_opt","outputs","integration","int_crop_areanorm_annual.csv")
  )
  
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
  
  saveRDS(out, out_baseline_reference)
  
  message("Wrote baseline reference: ", out_baseline_reference)
  
  print(out$obs_tbl)
  print(out$obs_x)
}
