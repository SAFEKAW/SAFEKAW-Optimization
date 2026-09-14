suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(purrr)
  library(mco)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}

grid_file <- get_arg(
  "--grid",
  here(
    "hpc_opt", "outputs",
    "optimization_grid_unconstrained_overlapfix_wave1.csv"
  )
)
threshold <- as.numeric(get_arg("--threshold", "0.02"))
if (!file.exists(grid_file)) stop("Missing grid: ", grid_file)
if (!is.finite(threshold) || threshold <= 0) stop("Invalid threshold.")

grid <- read_csv(grid_file, show_col_types = FALSE)
required_grid <- c(
  "job_id", "scenario_name", "seed", "run_tag", "popsize", "generations"
)
missing_grid <- setdiff(required_grid, names(grid))
if (length(missing_grid)) {
  stop("Grid missing columns: ", paste(missing_grid, collapse = ", "))
}

read_front <- function(scenario_name, seed, run_tag, job_id, ...) {
  path <- here(
    "hpc_opt", "outputs", "runs", run_tag, scenario_name,
    sprintf("pareto_front_%s_seed%d.csv", scenario_name, seed)
  )
  if (!file.exists(path)) stop("Missing Pareto output: ", path)
  read_csv(path, show_col_types = FALSE) %>%
    transmute(
      job_id = job_id,
      scenario_name = scenario_name,
      seed = seed,
      nitrate = nitrate,
      profit_minimized = profit_minimized,
      irrigation = irrigation,
      feasible = feasible
    )
}

fronts <- pmap_dfr(grid, read_front) %>%
  filter(feasible) %>%
  filter(if_all(c(nitrate, profit_minimized, irrigation), is.finite))
if (!nrow(fronts)) stop("No feasible finite optimization results found.")

objective_cols <- c("nitrate", "profit_minimized", "irrigation")
nondominated <- function(x) {
  x <- as.matrix(x)
  keep <- vapply(seq_len(nrow(x)), function(i) {
    !any(vapply(seq_len(nrow(x)), function(j) {
      j != i && all(x[j, ] <= x[i, ]) && any(x[j, ] < x[i, ])
    }, logical(1)))
  }, logical(1))
  x[keep, , drop = FALSE]
}

scenario_stability <- function(df) {
  seed_order <- sort(unique(df$seed))
  mins <- vapply(df[objective_cols], min, numeric(1))
  maxs <- vapply(df[objective_cols], max, numeric(1))
  spans <- pmax(maxs - mins, .Machine$double.eps)
  normalize <- function(x) sweep(sweep(x, 2, mins, "-"), 2, spans, "/")
  hypervolume <- function(x) {
    nd <- nondominated(x)
    mco::dominatedHypervolume(normalize(nd), rep(1.1, length(objective_cols)))
  }

  cumulative <- map_dfr(seq_along(seed_order), function(k) {
    included <- seed_order[seq_len(k)]
    x <- as.matrix(df[df$seed %in% included, objective_cols])
    nd <- nondominated(x)
    tibble(
      seeds_included = k,
      last_seed_added = seed_order[k],
      pooled_points = nrow(x),
      nondominated_points = nrow(nd),
      dominated_hypervolume = hypervolume(x),
      nitrate_min = min(x[, "nitrate"]),
      profit_max = -min(x[, "profit_minimized"]),
      irrigation_min = min(x[, "irrigation"])
    )
  }) %>%
    mutate(
      hypervolume_gain_fraction =
        dominated_hypervolume / lag(dominated_hypervolume) - 1
    )

  full_x <- as.matrix(df[objective_cols])
  full_hv <- hypervolume(full_x)
  loo <- map_dfr(seed_order, function(s) {
    without <- as.matrix(df[df$seed != s, objective_cols])
    hv_without <- hypervolume(without)
    tibble(
      omitted_seed = s,
      full_hypervolume = full_hv,
      hypervolume_without_seed = hv_without,
      hypervolume_loss_fraction = (full_hv - hv_without) / full_hv
    )
  })

  list(cumulative = cumulative, leave_one_out = loo)
}

results <- fronts %>%
  group_split(scenario_name) %>%
  map(function(df) {
    scenario <- first(df$scenario_name)
    x <- scenario_stability(df)
    x$cumulative$scenario_name <- scenario
    x$leave_one_out$scenario_name <- scenario
    x
  })

cumulative <- map_dfr(results, "cumulative") %>%
  relocate(scenario_name)
leave_one_out <- map_dfr(results, "leave_one_out") %>%
  relocate(scenario_name)

decision <- cumulative %>%
  group_by(scenario_name) %>%
  summarise(
    seeds = max(seeds_included),
    final_incremental_gain = last(hypervolume_gain_fraction),
    penultimate_incremental_gain = nth(
      hypervolume_gain_fraction,
      pmax(1L, n() - 1L)
    ),
    .groups = "drop"
  ) %>%
  left_join(
    leave_one_out %>%
      group_by(scenario_name) %>%
      summarise(
        max_leave_one_out_loss = max(hypervolume_loss_fraction),
        .groups = "drop"
      ),
    by = "scenario_name"
  ) %>%
  mutate(
    incremental_stable =
      abs(final_incremental_gain) <= threshold &
      abs(penultimate_incremental_gain) <= threshold,
    leave_one_out_stable = max_leave_one_out_loss <= threshold,
    five_seeds_sufficient = incremental_stable & leave_one_out_stable
  )

outdir <- here("hpc_opt", "outputs", "optimization_seed_stability")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
write_csv(cumulative, file.path(outdir, "cumulative_seed_stability.csv"))
write_csv(leave_one_out, file.path(outdir, "leave_one_seed_out_stability.csv"))
write_csv(decision, file.path(outdir, "five_seed_sufficiency.csv"))

print(decision, n = Inf)
message(
  "Scenarios passing the ", 100 * threshold, "% stability rule: ",
  sum(decision$five_seeds_sufficient), "/", nrow(decision)
)
message("Wrote seed-stability outputs to: ", outdir)
