# hpc_opt/scripts/01_build_historical_baseline_inputs.R
# Canonical entry point for the complete historical baseline common-input set.

suppressPackageStartupMessages({
  library(here)
})

baseline_env <- new.env(parent = globalenv())
baseline_env$scenario_tag <- "hist_baseline"

sys.source(
  here::here("hpc_opt", "scripts", "01_common_inputs_make.R"),
  envir = baseline_env
)

message("Historical baseline common inputs are complete.")
