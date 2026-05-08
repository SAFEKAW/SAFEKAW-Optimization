rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(here)
})

run_stage <- function(script, envir = new.env(parent = globalenv())) {
  message("\n==============================")
  message("Running: ", script)
  message("==============================")
  sys.source(here("hpc_opt", "scripts", script), envir = envir)
  invisible(TRUE)
}

# -----------------------------
# user settings
# -----------------------------
run_fit_models        <- TRUE
run_hist_integration  <- TRUE
run_precompute        <- TRUE
run_factorial         <- TRUE
run_check             <- TRUE

# future scenarios / gcms to process
gcm_names <- c("ccsm4")
scenario_tags <- c(
  "hist_baseline",
  "rcp45_early", "rcp45_mid", "rcp45_late",
  "rcp85_early", "rcp85_mid", "rcp85_late"
)

# -----------------------------
# 1. fit and save component models
# -----------------------------
if (run_fit_models) {
  run_stage("01_fit_and_save_models.R")
}

# -----------------------------
# 2. historical integration
# -----------------------------
if (run_hist_integration) {
  run_stage("02_integrate_historical_run.R")
}

# -----------------------------
# 3. precompute inputs
# -----------------------------
if (run_precompute) {
  for (sc in scenario_tags) {
    env <- new.env(parent = globalenv())
    env$scenario_tag <- sc
    
    if (sc != "hist_baseline") {
      for (gcm in gcm_names) {
        env2 <- new.env(parent = globalenv())
        env2$scenario_tag <- sc
        env2$gcm_name <- gcm
        run_stage("03_precompute_inputs.R", envir = env2)
      }
    } else {
      run_stage("03_precompute_inputs.R", envir = env)
    }
  }
}

# -----------------------------
# 4. deterministic / factorial runs
# -----------------------------
if (run_factorial) {
  run_stage("04_run_factorial_all.R")
}

# -----------------------------
# 5. summary / checks
# -----------------------------
if (run_check) {
  run_stage("05_check_deterministic_factorial.R")
}

message("\nFull pipeline complete.")