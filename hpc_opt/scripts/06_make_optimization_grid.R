suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}
parse_ints <- function(x) {
  values <- as.integer(strsplit(x, ",", fixed = TRUE)[[1]])
  if (length(values) == 0 || anyNA(values)) stop("Invalid integer list: ", x)
  values
}

seeds <- parse_ints(get_arg("--seeds", paste(1:20, collapse = ",")))
popsize <- as.integer(get_arg("--popsize", "8"))
generations <- as.integer(get_arg("--generations", "3"))
run_tag <- get_arg("--run-tag", "unconstrained_production")
out_file <- get_arg(
  "--output",
  here("hpc_opt", "outputs", "optimization_grid_unconstrained.csv")
)

if (popsize < 4 || popsize %% 4 != 0) stop("popsize must be a multiple of 4.")
if (generations < 1) stop("generations must be positive.")
if (!grepl("^[A-Za-z0-9._-]+$", run_tag)) stop("Invalid run tag.")

grid <- expand.grid(
  gcm = "ensemble",
  climate = c("rcp45", "rcp85"),
  period = c("early", "mid", "late"),
  landuse = c("fixed", "bau"),
  seed = seeds,
  stringsAsFactors = FALSE
) %>%
  mutate(
    job_id = row_number(),
    workflow = "unconstrained",
    scenario_name = paste(landuse, gcm, climate, period, sep = "_"),
    popsize = popsize,
    generations = generations,
    run_tag = run_tag
  ) %>%
  select(job_id, workflow, everything())

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write_csv(grid, out_file)
message("Wrote ", nrow(grid), " jobs to: ", out_file)
