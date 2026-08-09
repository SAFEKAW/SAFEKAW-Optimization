# Resume only incomplete deterministic-factorial scenarios. This is useful on
# synced filesystems where a transient file lock can interrupt a long batch.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
})

grid_file <- here(
  "hpc_opt", "outputs", "deterministic_factorial_grid_32_scenarios.csv"
)
if (!file.exists(grid_file)) stop("Missing deterministic grid: ", grid_file)

factorial_grid <- read_csv(grid_file, show_col_types = FALSE)
if (nrow(factorial_grid) != 32L) {
  stop("Expected 32 deterministic scenarios; found ", nrow(factorial_grid), ".")
}

expected_outputs <- function(combo_name) {
  outdir <- here("hpc_opt", "outputs", "factorial_runs", combo_name)
  file.path(
    outdir,
    c(
      "scenario_path.csv", "deterministic_results.csv",
      "irrigation_by_crop.csv", "summary_by_period.csv", "summary_overall.csv"
    )
  )
}

scenario_complete <- function(combo_name) {
  files <- expected_outputs(combo_name)
  legacy_files <- file.path(
    dirname(files[[1]]),
    paste0(
      c(
        "scenario_path_", "deterministic_results_",
        "irrigation_area_by_domain_crop_", "deterministic_summary_by_period_",
        "deterministic_summary_overall_"
      ),
      combo_name, ".csv"
    )
  )
  use_legacy <- !file.exists(files) & file.exists(legacy_files)
  files[use_legacy] <- legacy_files[use_legacy]
  if (!all(file.exists(files)) || any(file.info(files)$size <= 0)) return(FALSE)

  annual_file <- files[[2]]
  period_file <- files[[4]]
  overall_file <- files[[5]]

  nrow(read_csv(annual_file, show_col_types = FALSE)) == 75L &&
    nrow(read_csv(period_file, show_col_types = FALSE)) == 3L &&
    nrow(read_csv(overall_file, show_col_types = FALSE)) == 1L
}

run_one <- function(scenario) {
  env <- new.env(parent = globalenv())
  env$gcm_name <- scenario$gcm_name
  env$climate_pathway <- scenario$climate_pathway
  env$landuse_file <- scenario$landuse_file
  env$irrigation_technology_file <- scenario$irrigation_technology_file
  env$irrigation_extent_file <- scenario$irrigation_extent_file
  env$fertilizer_file <- scenario$fertilizer_file
  sys.source(
    here("hpc_opt", "scripts", "04_run_deterministic_factorial.R"),
    envir = env
  )
}

for (i in seq_len(nrow(factorial_grid))) {
  scenario <- factorial_grid[i, ]
  if (scenario_complete(scenario$combo_name)) {
    message("Complete; skipping ", i, "/32: ", scenario$combo_name)
    next
  }

  success <- FALSE
  for (attempt in 1:3) {
    message(
      "Running incomplete scenario ", i, "/32 (attempt ", attempt,
      "/3): ", scenario$combo_name
    )
    result <- try(run_one(scenario), silent = TRUE)
    if (!inherits(result, "try-error") && scenario_complete(scenario$combo_name)) {
      success <- TRUE
      break
    }
    message("Attempt failed: ", as.character(result))
    Sys.sleep(5 * attempt)
  }

  if (!success) {
    stop("Scenario remained incomplete after three attempts: ", scenario$combo_name)
  }
}

complete <- vapply(factorial_grid$combo_name, scenario_complete, logical(1))
if (!all(complete)) {
  stop(
    "Deterministic factorial remains incomplete: ",
    paste(factorial_grid$combo_name[!complete], collapse = ", ")
  )
}

message("All 32 deterministic scenarios are complete and structurally valid.")
