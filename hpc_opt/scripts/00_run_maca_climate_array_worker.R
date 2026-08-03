# Run one GCM × RCP-period MACA climate job.
# Intended for SLURM array indices 1-30, but can also be run locally with:
# Rscript hpc_opt/scripts/00_run_maca_climate_array_worker.R --task-id 1

suppressPackageStartupMessages({
  library(here)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}

task_id <- as.integer(
  get_arg("--task-id", Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA_character_))
)

gcm_lookup <- data.frame(
  gcm_name = c(
    "ccsm4",
    "bcc_csm1_1",
    "gfdl_esm2m",
    "canesm2",
    "cnrm_cm5"
  ),
  maca_model = c(
    "CCSM4",
    "bcc-csm1-1",
    "GFDL-ESM2M",
    "CanESM2",
    "CNRM-CM5"
  )
)

scenario_tags <- c(
  "rcp45_early", "rcp45_mid", "rcp45_late",
  "rcp85_early", "rcp85_mid", "rcp85_late"
)

job_grid <- tidyr::crossing(
  gcm_lookup,
  scenario_tag = scenario_tags
)

if (is.na(task_id) || task_id < 1L || task_id > nrow(job_grid)) {
  stop("task_id must be between 1 and ", nrow(job_grid), ".")
}

job <- job_grid[task_id, ]
message("MACA climate array task ", task_id, " of ", nrow(job_grid))
print(job)

worker_env <- new.env(parent = globalenv())
worker_env$gcm_name <- job$gcm_name[[1]]
worker_env$maca_model <- job$maca_model[[1]]
worker_env$scenario_tag <- job$scenario_tag[[1]]

sys.source(
  here("hpc_opt", "scripts", "00_make_climate_inputs_maca.R"),
  envir = worker_env
)
