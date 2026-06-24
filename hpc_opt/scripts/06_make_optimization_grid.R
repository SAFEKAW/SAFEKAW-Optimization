# hpc_opt/scripts/06_make_optimization_grid.R

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
})

grid <- expand.grid(
  gcm = "ensemble",
  climate = c("rcp45", "rcp85"),
  period = c("early", "mid", "late"),
  landuse = c("fixed", "bau"),
  seed = 1:20,
  stringsAsFactors = FALSE
) %>%
  mutate(
    job_id = row_number(),
    scenario_name = paste(landuse, gcm, climate, period, sep = "_")
  )

out_file <- here("hpc_opt", "outputs", "optimization_grid_unconstrained.csv")
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

write_csv(grid, out_file)

message("Wrote ", nrow(grid), " jobs to: ", out_file)