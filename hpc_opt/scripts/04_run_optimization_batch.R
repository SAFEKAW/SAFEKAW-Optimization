# hpc_opt/scripts/05_run_optimization_batch.R

suppressPackageStartupMessages({
  library(here)
})

gcm_name <- "ensemble"
climates <- c("rcp45", "rcp85")
periods <- c("early", "mid", "late")
landuses <- c("fixed", "bau")
seeds <- 1

worker <- here("hpc_opt", "scripts", "04_run_optimization.R")

grid <- expand.grid(
  climate = climates,
  period = periods,
  landuse = landuses,
  seed = seeds,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(grid))) {
  cmd <- sprintf(
    'Rscript "%s" --gcm %s --climate %s --period %s --landuse %s --seed %s',
    worker,
    gcm_name,
    grid$climate[i],
    grid$period[i],
    grid$landuse[i],
    grid$seed[i]
  )
  
  message("\nRunning: ", cmd)
  status <- system(cmd)
  
  if (status != 0) {
    stop("Optimization failed for row ", i, ": ", paste(grid[i, ], collapse = " | "))
  }
}