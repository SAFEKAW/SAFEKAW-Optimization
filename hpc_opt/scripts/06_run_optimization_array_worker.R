# hpc_opt/scripts/07_run_optimization_array_worker.R

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)

job_id <- as.integer(args[[1]])

grid <- read_csv(
  here("hpc_opt", "outputs", "optimization_grid_unconstrained.csv"),
  show_col_types = FALSE
)

this_job <- grid %>% filter(job_id == !!job_id)

if (nrow(this_job) != 1) {
  stop("Invalid job_id: ", job_id)
}

worker <- here("hpc_opt", "scripts", "04_run_optimization.R")

cmd <- sprintf(
  'Rscript "%s" --gcm %s --climate %s --period %s --landuse %s --seed %s',
  worker,
  this_job$gcm,
  this_job$climate,
  this_job$period,
  this_job$landuse,
  this_job$seed
)

message("Running job_id=", job_id)
message(cmd)

status <- system(cmd)

if (status != 0) {
  stop("Optimization failed for job_id=", job_id)
}