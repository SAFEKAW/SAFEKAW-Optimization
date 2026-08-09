suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
})

# RCP 4.5-only convenience batch. The full two-pathway factorial is defined in
# 04_run_factorial_all.R; this script uses the identical scenario dimensions.
factorial_grid <- expand.grid(
  gcm_name = "ensemble",
  climate_pathway = "rcp45",
  landuse_file = c(
    here("hpc_opt", "config", "landuse", "fixed.yaml"),
    here("hpc_opt", "config", "landuse", "bau.yaml")
  ),
  irrigation_technology_file = c(
    here("hpc_opt", "config", "irrigation_technology", "current.yaml"),
    here("hpc_opt", "config", "irrigation_technology", "efficient.yaml")
  ),
  irrigation_extent_file = c(
    here("hpc_opt", "config", "irrigation_extent", "baseline.yaml"),
    here("hpc_opt", "config", "irrigation_extent", "expanded.yaml")
  ),
  fertilizer_file = c(
    here("hpc_opt", "config", "fertilizer", "current.yaml"),
    here("hpc_opt", "config", "fertilizer", "efficient.yaml")
  ),
  stringsAsFactors = FALSE
) %>%
  mutate(
    scenario_id = row_number(),
    landuse_name = tools::file_path_sans_ext(basename(landuse_file)),
    irrigation_technology_name = tools::file_path_sans_ext(
      basename(irrigation_technology_file)
    ),
    irrigation_extent_name = tools::file_path_sans_ext(
      basename(irrigation_extent_file)
    ),
    fertilizer_name = tools::file_path_sans_ext(basename(fertilizer_file)),
    combo_name = paste(
      landuse_name, irrigation_technology_name, irrigation_extent_name,
      fertilizer_name, gcm_name, climate_pathway, sep = "_"
    )
  ) %>%
  select(scenario_id, combo_name, everything())

stopifnot(nrow(factorial_grid) == 16L)
grid_file <- here(
  "hpc_opt", "outputs", "deterministic_factorial_grid_rcp45_16_scenarios.csv"
)
dir.create(dirname(grid_file), recursive = TRUE, showWarnings = FALSE)
write_csv(factorial_grid, grid_file)

for (i in seq_len(nrow(factorial_grid))) {
  scenario <- factorial_grid[i, ]
  env <- new.env(parent = globalenv())
  env$gcm_name <- scenario$gcm_name
  env$climate_pathway <- scenario$climate_pathway
  env$landuse_file <- scenario$landuse_file
  env$irrigation_technology_file <- scenario$irrigation_technology_file
  env$irrigation_extent_file <- scenario$irrigation_extent_file
  env$fertilizer_file <- scenario$fertilizer_file

  message("Running ", i, "/", nrow(factorial_grid), ": ", scenario$combo_name)
  sys.source(
    here("hpc_opt", "scripts", "04_run_deterministic_factorial.R"),
    envir = env
  )
}

message("All RCP 4.5 deterministic factorial runs complete.")
