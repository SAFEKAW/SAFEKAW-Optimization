# hpc_opt/scripts/05_run_optimization_batch.R

suppressPackageStartupMessages({
  library(here)
})

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}

gcm_name <- "ensemble"
climates <- c("rcp45", "rcp85")
periods <- c("early", "mid", "late")
landuses <- c("fixed", "bau")
irrigation_scenarios <- c("current", "efficient")
seeds <- as.integer(get_arg("--seed", "1"))
phase <- get_arg("--phase", "unconstrained")
popsize <- as.integer(get_arg("--popsize", "8"))
generations <- as.integer(get_arg("--generations", "3"))
skip_existing <- tolower(get_arg("--skip-existing", "false")) %in%
  c("true", "t", "1", "yes", "y")

constrained <- !identical(phase, "unconstrained")
worker <- here(
  "hpc_opt", "scripts",
  if (constrained) "04_run_optimization_constrained.R" else "04_run_optimization.R"
)

grid <- expand.grid(
  climate = climates,
  period = periods,
  landuse = landuses,
  irrigation = if (constrained) irrigation_scenarios else "current",
  seed = seeds,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(grid))) {
  scenario_name <- if (constrained) {
    paste(
      grid$landuse[i], grid$irrigation[i], gcm_name,
      grid$climate[i], grid$period[i], sep = "_"
    )
  } else {
    paste(
      grid$landuse[i], gcm_name, grid$climate[i], grid$period[i], sep = "_"
    )
  }
  expected_output <- if (constrained) {
    here(
      "hpc_opt", "outputs", "runs", phase, scenario_name,
      sprintf("pareto_front_%s_seed%d.csv", scenario_name, grid$seed[i])
    )
  } else {
    here(
      "hpc_opt", "outputs", "runs", scenario_name,
      sprintf("pareto_front_%s_seed%d.csv", scenario_name, grid$seed[i])
    )
  }

  if (skip_existing && file.exists(expected_output)) {
    message("\nSkipping existing output: ", expected_output)
    next
  }

  cmd_args <- sprintf(
    '--gcm %s --climate %s --period %s --landuse %s --seed %s --popsize %s --generations %s',
    gcm_name,
    grid$climate[i],
    grid$period[i],
    grid$landuse[i],
    grid$seed[i],
    popsize,
    generations
  )

  if (constrained) {
    cmd_args <- paste(
      cmd_args, "--phase", phase, "--irrigation", grid$irrigation[i]
    )
  }

  cmd <- sprintf(
    'Rscript "%s" %s',
    worker,
    cmd_args
  )
  
  message("\nRunning: ", cmd)
  status <- system(cmd)
  
  if (status != 0) {
    stop("Optimization failed for row ", i, ": ", paste(grid[i, ], collapse = " | "))
  }
}
