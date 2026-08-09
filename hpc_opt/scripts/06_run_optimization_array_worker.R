suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
`%||%` <- function(a, b) if (!is.null(a)) a else b
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}

job_id <- as.integer(get_arg(
  "--task-id",
  Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA_character_)
))
grid_file <- get_arg(
  "--grid",
  here("hpc_opt", "outputs", "optimization_grid_unconstrained.csv")
)
force <- identical(tolower(get_arg("--force", "false")), "true")

if (is.na(job_id)) stop("Supply --task-id or SLURM_ARRAY_TASK_ID.")
grid <- read_csv(grid_file, show_col_types = FALSE)
this_job <- grid %>% filter(.data$job_id == !!job_id)
if (nrow(this_job) != 1) stop("Invalid or duplicate job_id: ", job_id)

workflow <- if ("workflow" %in% names(this_job)) {
  this_job$workflow
} else {
  "unconstrained"
}
if (!workflow %in% c("unconstrained", "constrained")) {
  stop("Unknown workflow for job_id=", job_id, ": ", workflow)
}

scenario <- this_job$scenario_name
seed <- this_job$seed
run_tag <- this_job$run_tag
outdir <- here("hpc_opt", "outputs", "runs", run_tag, scenario)
pareto_file <- file.path(outdir, sprintf("pareto_front_%s_seed%d.csv", scenario, seed))
manifest_dir <- here("hpc_opt", "outputs", "optimization_manifests", run_tag)
manifest_file <- file.path(manifest_dir, sprintf("job_%04d.csv", job_id))
dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)

valid_output <- function(path) {
  if (!file.exists(path)) return(FALSE)
  x <- tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  required <- c(
    "scenario_name", "seed", "popsize", "generations", "run_tag",
    "nitrate", "irrigation", "profit", "feasible"
  )
  !is.null(x) && nrow(x) > 0 && all(required %in% names(x)) &&
    !anyNA(x[required]) && all(x$scenario_name == scenario) && all(x$seed == seed) &&
    all(x$popsize == this_job$popsize) &&
    all(x$generations == this_job$generations) && all(x$run_tag == run_tag)
}

write_manifest <- function(status, started_at, finished_at = NA_character_,
                           elapsed_seconds = NA_real_, exit_status = NA_integer_,
                           message = "") {
  write_csv(tibble(
    job_id = job_id,
    slurm_job_id = Sys.getenv("SLURM_JOB_ID", unset = NA_character_),
    slurm_array_job_id = Sys.getenv("SLURM_ARRAY_JOB_ID", unset = NA_character_),
    host = Sys.info()[["nodename"]],
    status = status,
    started_at_utc = started_at,
    finished_at_utc = finished_at,
    elapsed_seconds = elapsed_seconds,
    exit_status = exit_status,
    scenario_name = scenario,
    seed = seed,
    popsize = this_job$popsize,
    generations = this_job$generations,
    workflow = workflow,
    phase = if ("phase" %in% names(this_job)) this_job$phase else NA_character_,
    irrigation = if ("irrigation" %in% names(this_job)) {
      this_job$irrigation
    } else {
      "current"
    },
    run_tag = run_tag,
    output_file = pareto_file,
    message = message
  ), manifest_file)
}

started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
if (!force && valid_output(pareto_file)) {
  write_manifest("skipped_valid", started_at, started_at, 0, 0, "Existing output validated.")
  message("Skipping validated output for job_id=", job_id)
  quit(save = "no", status = 0)
}

write_manifest("running", started_at)
worker <- here(
  "hpc_opt", "scripts",
  if (workflow == "constrained") {
    "04_run_optimization_constrained.R"
  } else {
    "04_run_optimization.R"
  }
)
worker_args <- c(
  worker,
  "--gcm", this_job$gcm,
  "--climate", this_job$climate,
  "--period", this_job$period,
  "--landuse", this_job$landuse,
  "--seed", as.character(seed),
  "--popsize", as.character(this_job$popsize),
  "--generations", as.character(this_job$generations),
  "--run-tag", run_tag
)
if (workflow == "constrained") {
  worker_args <- c(
    worker_args,
    "--phase", this_job$phase,
    "--irrigation", this_job$irrigation
  )
}

message("Running job_id=", job_id, " scenario=", scenario, " seed=", seed)
start_clock <- proc.time()[["elapsed"]]
status <- tryCatch(
  system2("Rscript", worker_args),
  error = function(e) structure(1L, error_message = conditionMessage(e))
)
elapsed <- proc.time()[["elapsed"]] - start_clock
ok <- identical(as.integer(status), 0L) && valid_output(pareto_file)
finished_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
error_message <- attr(status, "error_message") %||% ""

write_manifest(
  if (ok) "complete" else "failed",
  started_at, finished_at, elapsed, as.integer(status), error_message
)
if (!ok) stop("Optimization failed or produced invalid output for job_id=", job_id)
message("Completed job_id=", job_id, " in ", round(elapsed, 1), " seconds.")
