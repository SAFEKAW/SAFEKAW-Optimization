suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
})

source(here("hpc_opt", "R", "10_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}
grid_file <- get_arg(
  "--grid",
  here("hpc_opt", "outputs", "optimization_grid_unconstrained.csv")
)

required_packages <- c("here", "readr", "dplyr", "tidyr", "mco", "yaml")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
}
if (!file.exists(grid_file)) stop("Missing grid: ", grid_file)

required_runtime_files <- c(
  here("hpc_opt", "R", c(
    "10_helpers.R", "11_models_load.R", "12_irrigation_predict.R",
    "13_yield_predict.R", "17_fert_multiplier.R", "18_irrig_allocation.R",
    "32_precomp_build.R", "33_eval_engine.R", "34_read_config_yaml.R",
    "36_build_policy_from_configs.R", "41_objective_wrapper.R"
  )),
  here("hpc_opt", "scripts", "04_run_optimization.R")
)
missing_runtime_files <- required_runtime_files[!file.exists(required_runtime_files)]
if (length(missing_runtime_files)) {
  stop(
    "Missing optimization runtime files: ",
    paste(missing_runtime_files, collapse = ", ")
  )
}

shared_scenario_files <- c(
  here("hpc_opt", "config", "irrigation_technology", c("current.yaml", "efficient.yaml")),
  here("hpc_opt", "config", "irrigation_extent", c("baseline.yaml", "expanded.yaml")),
  here("hpc_opt", "config", "fertilizer", c("current.yaml", "efficient.yaml"))
)
missing_scenario_files <- shared_scenario_files[!file.exists(shared_scenario_files)]
if (length(missing_scenario_files)) {
  stop("Missing shared scenario configs: ", paste(missing_scenario_files, collapse = ", "))
}

grid <- read_csv(grid_file, show_col_types = FALSE)
if (!"workflow" %in% names(grid)) grid$workflow <- "unconstrained"
if (!all(grid$workflow %in% c("unconstrained", "constrained"))) {
  stop("Grid workflow must be unconstrained or constrained.")
}
required_grid_cols <- c(
  "job_id", "gcm", "climate", "period", "landuse", "seed",
  "scenario_name", "popsize", "generations", "run_tag"
)
missing_cols <- setdiff(required_grid_cols, names(grid))
if (length(missing_cols)) stop("Grid missing columns: ", paste(missing_cols, collapse = ", "))
if (any(grid$workflow == "constrained")) {
  constrained_cols <- c("phase", "irrigation")
  missing_constrained <- setdiff(constrained_cols, names(grid))
  if (length(missing_constrained)) {
    stop(
      "Constrained grid missing columns: ",
      paste(missing_constrained, collapse = ", ")
    )
  }
  if (!all(grid$phase[grid$workflow == "constrained"] %in%
           c("crop_bounds", "crop_fert", "crop_fert_irrigfrac"))) {
    stop("Constrained grid contains an unsupported phase.")
  }
  if (!all(grid$irrigation[grid$workflow == "constrained"] %in%
           c("current", "efficient"))) {
    stop("Constrained grid contains an unsupported irrigation scenario.")
  }
}
if (anyDuplicated(grid$job_id)) stop("Grid job_id values are not unique.")
if (any(sort(as.integer(grid$job_id)) != seq_len(nrow(grid)))) {
  stop("Grid job_id values must be contiguous from 1 to nrow(grid).")
}
if (any(grid$popsize < 4 | grid$popsize %% 4 != 0)) stop("Invalid popsize in grid.")
if (any(grid$generations < 1)) stop("Invalid generations in grid.")

if (any(grid$workflow == "constrained")) {
  constrained_runtime_files <- c(
    here("hpc_opt", "R", "41_objective_wrapper_constrained.R"),
    here("hpc_opt", "scripts", "04_run_optimization_constrained.R")
  )
  missing_constrained_runtime <- constrained_runtime_files[
    !file.exists(constrained_runtime_files)
  ]
  if (length(missing_constrained_runtime)) {
    stop(
      "Missing constrained runtime files: ",
      paste(missing_constrained_runtime, collapse = ", ")
    )
  }
}

models <- c(
  "irr_lm.rds", "wq_lm.rds",
  paste0("yield_kg_", c("Corn", "Soybeans", "Sorghum", "Wheat"), ".rds"),
  paste0("yield_kcal_", c("Corn", "Soybeans", "Sorghum", "Wheat"), ".rds")
)
checks <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  x <- grid[i, ]
  tibble(
    job_id = x$job_id,
    workflow = x$workflow,
    scenario_name = x$scenario_name,
    county_inputs = file.exists(here(
      "hpc_opt", "outputs",
      sprintf("common_inputs_county_%s_%s_%s.csv", x$gcm, x$climate, x$period)
    )),
    precompute = file.exists(here(
      "hpc_opt", "outputs", "precompute",
      sprintf("precomp_%s_%s_%s.rds", x$gcm, x$climate, x$period)
    )),
    scenario_path = file.exists(resolve_baseline_scenario_path(
      x$landuse, x$gcm, x$climate, require_existing = FALSE
    ))
  )
}))
model_checks <- file.exists(here("hpc_opt", "models", models))

outdir <- here("hpc_opt", "outputs", "preflight")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
grid_label <- tools::file_path_sans_ext(basename(grid_file))
report_file <- file.path(outdir, paste0(grid_label, "_preflight.csv"))
write_csv(checks, report_file)

failed <- checks %>% filter(!county_inputs | !precompute | !scenario_path)
if (nrow(failed) || !all(model_checks)) {
  if (!all(model_checks)) message("Missing models: ", paste(models[!model_checks], collapse = ", "))
  if (nrow(failed)) print(failed, n = Inf)
  stop("Optimization preflight failed. Report: ", report_file)
}
message(
  "Preflight passed for ", nrow(grid), " jobs (",
  n_distinct(grid$scenario_name), " contexts). Report: ", report_file
)
