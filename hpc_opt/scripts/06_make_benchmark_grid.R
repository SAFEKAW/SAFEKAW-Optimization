suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
})

# One representative late-century BAU/RCP8.5 context. Budgets intentionally
# remain modest until the first HPC timings establish a feasible scale.
grid <- tidyr::crossing(
  popsize = c(8L, 16L, 32L),
  generations = c(3L, 10L, 25L)
) %>%
  filter(
    (popsize == 8 & generations == 3) |
      (popsize == 16 & generations == 10) |
      (popsize == 32 & generations == 25)
  ) %>%
  mutate(
    job_id = row_number(),
    workflow = "unconstrained",
    gcm = "ensemble",
    climate = "rcp85",
    period = "late",
    landuse = "bau",
    seed = 7001L,
    scenario_name = paste(landuse, gcm, climate, period, sep = "_"),
    run_tag = paste0("unconstrained_benchmark_p", popsize, "_g", generations)
  ) %>%
  select(job_id, workflow, gcm, climate, period, landuse, seed, scenario_name,
         popsize, generations, run_tag)

out_file <- here("hpc_opt", "outputs", "optimization_grid_unconstrained_benchmark.csv")
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write_csv(grid, out_file)
message("Wrote ", nrow(grid), " benchmark jobs to: ", out_file)
