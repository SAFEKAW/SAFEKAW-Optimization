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
parse_values <- function(x) strsplit(x, ",", fixed = TRUE)[[1]]
parse_ints <- function(x) {
  values <- as.integer(parse_values(x))
  if (!length(values) || anyNA(values)) stop("Invalid integer list: ", x)
  values
}

seeds <- parse_ints(get_arg("--seeds", paste(1:20, collapse = ",")))
phases <- parse_values(get_arg(
  "--phases", "crop_bounds,crop_fert,crop_fert_irrigfrac"
))
irrigation <- parse_values(get_arg("--irrigation", "current,efficient"))
popsize <- as.integer(get_arg("--popsize", "8"))
generations <- as.integer(get_arg("--generations", "3"))
run_tag_prefix <- get_arg("--run-tag-prefix", "constrained")
out_file <- get_arg(
  "--output",
  here("hpc_opt", "outputs", "optimization_grid_constrained.csv")
)

supported_phases <- c("crop_bounds", "crop_fert", "crop_fert_irrigfrac")
if (!all(phases %in% supported_phases)) stop("Unsupported constrained phase.")
if (!all(irrigation %in% c("current", "efficient"))) {
  stop("Irrigation scenarios must be current and/or efficient.")
}
if (popsize < 4 || popsize %% 4 != 0) stop("popsize must be a multiple of 4.")
if (generations < 1) stop("generations must be positive.")
if (!grepl("^[A-Za-z0-9._-]+$", run_tag_prefix)) stop("Invalid run-tag prefix.")

grid <- expand.grid(
  gcm = "ensemble",
  climate = c("rcp45", "rcp85"),
  period = c("early", "mid", "late"),
  landuse = c("fixed", "bau"),
  irrigation = irrigation,
  phase = phases,
  seed = seeds,
  stringsAsFactors = FALSE
) %>%
  mutate(
    job_id = row_number(),
    workflow = "constrained",
    scenario_name = paste(
      landuse, irrigation, gcm, climate, period, sep = "_"
    ),
    popsize = popsize,
    generations = generations,
    run_tag = paste(run_tag_prefix, phase, sep = "_")
  ) %>%
  select(job_id, workflow, everything())

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write_csv(grid, out_file)
message(
  "Wrote ", nrow(grid), " constrained jobs across ",
  n_distinct(grid$scenario_name), " scenario contexts to: ", out_file
)
