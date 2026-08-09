suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(purrr)
  library(mco)
})

grid_file <- here("hpc_opt", "outputs", "optimization_grid_unconstrained_benchmark.csv")
grid <- read_csv(grid_file, show_col_types = FALSE)
if (!"workflow" %in% names(grid)) grid$workflow <- "unconstrained"

fronts <- pmap_dfr(grid, function(job_id, workflow, gcm, climate, period,
                                  landuse, seed, scenario_name, popsize,
                                  generations, run_tag) {
  path <- here(
    "hpc_opt", "outputs", "runs", run_tag, scenario_name,
    sprintf("pareto_front_%s_seed%d.csv", scenario_name, seed)
  )
  if (!file.exists(path)) stop("Missing benchmark output: ", path)
  read_csv(path, show_col_types = FALSE) %>%
    mutate(job_id = job_id, .before = 1)
})

objective_cols <- c("nitrate", "profit_minimized", "irrigation")
objective_min <- vapply(fronts[objective_cols], min, numeric(1), na.rm = TRUE)
objective_max <- vapply(fronts[objective_cols], max, numeric(1), na.rm = TRUE)
objective_span <- pmax(objective_max - objective_min, .Machine$double.eps)
reference_point <- rep(1.1, length(objective_cols))

manifest_rows <- map_dfr(grid$run_tag, function(tag) {
  files <- list.files(
    here("hpc_opt", "outputs", "optimization_manifests", tag),
    pattern = "^job_[0-9]+[.]csv$", full.names = TRUE
  )
  if (!length(files)) return(tibble())
  map_dfr(files, read_csv, show_col_types = FALSE)
}) %>%
  filter(status == "complete") %>%
  select(run_tag, elapsed_seconds)

summary <- fronts %>%
  group_by(job_id, run_tag, scenario_name, seed, popsize, generations) %>%
  group_modify(function(.x, .y) {
    normalized <- sweep(as.matrix(.x[objective_cols]), 2, objective_min, "-")
    normalized <- sweep(normalized, 2, objective_span, "/")
    tibble(
      pareto_points = nrow(.x),
      dominated_hypervolume = mco::dominatedHypervolume(normalized, reference_point),
      nitrate_min = min(.x$nitrate),
      nitrate_max = max(.x$nitrate),
      profit_min = min(.x$profit),
      profit_max = max(.x$profit),
      irrigation_min = min(.x$irrigation),
      irrigation_max = max(.x$irrigation)
    )
  }) %>%
  ungroup() %>%
  left_join(manifest_rows, by = "run_tag") %>%
  arrange(popsize, generations) %>%
  mutate(
    hypervolume_change_fraction =
      dominated_hypervolume / lag(dominated_hypervolume) - 1
  )

out_file <- here("hpc_opt", "outputs", "optimization_benchmark_summary.csv")
write_csv(summary, out_file)
print(summary, n = Inf)
message("Wrote benchmark summary: ", out_file)
