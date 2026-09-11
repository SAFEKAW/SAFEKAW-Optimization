# Collaborator entry point for the deterministic 32-scenario workflow.
#
# This runner uses the derived inputs packaged in the repository. It does not
# download climate data, rebuild common inputs, refit models, rerun historical
# integration, or rebuild precompute objects.

rm(list = ls())
gc()

suppressPackageStartupMessages(library(here))

run_stage <- function(script) {
  message("\n==============================")
  message("Running: ", script)
  message("==============================")
  sys.source(
    here("hpc_opt", "scripts", script),
    envir = new.env(parent = globalenv())
  )
  invisible(TRUE)
}

scenario_tags <- c(
  "rcp45_early", "rcp45_mid", "rcp45_late",
  "rcp85_early", "rcp85_mid", "rcp85_late"
)
crops <- c("Corn", "Soybeans", "Sorghum", "Wheat")

required_packages <- c(
  "dplyr", "ggplot2", "here", "purrr", "readr", "scales",
  "stringr", "tibble", "tidyr", "yaml"
)

required_files <- c(
  here("hpc_opt", "outputs", paste0(
    "common_inputs_county_ensemble_", scenario_tags, ".csv"
  )),
  here("hpc_opt", "outputs", "precompute", paste0(
    "precomp_ensemble_", scenario_tags, ".rds"
  )),
  here("hpc_opt", "outputs", "precompute", "precomp_hist_baseline.rds"),
  here("hpc_opt", "outputs", "precompute", "baseline_reference.rds"),
  here("hpc_opt", "outputs", "integration", "int_basin_annual.csv"),
  here("hpc_opt", "outputs", "common_inputs_basin_hist_baseline.csv"),
  here("hpc_opt", "models", "irr_lm.rds"),
  here("hpc_opt", "models", "wq_lm.rds"),
  here("hpc_opt", "models", paste0("yield_kg_", crops, ".rds")),
  here("hpc_opt", "models", paste0("yield_kcal_", crops, ".rds"))
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_packages) > 0L || length(missing_files) > 0L) {
  problems <- c(
    if (length(missing_packages) > 0L) {
      paste0("Missing R packages: ", paste(missing_packages, collapse = ", "))
    },
    if (length(missing_files) > 0L) {
      paste0("Missing packaged files:\n - ", paste(missing_files, collapse = "\n - "))
    }
  )
  stop(paste(problems, collapse = "\n\n"), call. = FALSE)
}

message("Packaged deterministic input preflight passed.")
if (tolower(Sys.getenv("SAFEKAW_PREFLIGHT_ONLY", unset = "false")) %in%
    c("1", "true", "yes")) {
  message("SAFEKAW_PREFLIGHT_ONLY is set; stopping before scenario execution.")
  quit(save = "no", status = 0L)
}

run_stage("04_run_factorial_all.R")
run_stage("05_check_deterministic_counterfactuals.R")
run_stage("05_check_deterministic_factorial.R")
message("\nPackaged deterministic workflow complete.")
