# hpc_opt/R/11_models_load.R
suppressPackageStartupMessages({
  library(here)
})

# Convention:
# model_dir/
#   irr_lm.rds
#   yield_kg_<Crop>.rds
#   yield_kcal_<Crop>.rds
#   wq_lm.rds   (optional)
#
load_models <- function(model_dir) {
  stopifnot(dir.exists(model_dir))
  
  # helper to load if exists
  load_if_exists <- function(path) if (file.exists(path)) readRDS(path) else NULL
  
  models <- list(
    irrigation_lm = load_if_exists(file.path(model_dir, "irr_lm.rds")),
    wq_lm         = load_if_exists(file.path(model_dir, "wq_lm.rds")),
    yield_kg      = list(),
    yield_kcal    = list()
  )
  
  # load any crop-specific yield models present
  kg_files   <- list.files(model_dir, pattern = "^yield_kg_.*\\.rds$", full.names = TRUE)
  kcal_files <- list.files(model_dir, pattern = "^yield_kcal_.*\\.rds$", full.names = TRUE)
  
  for (f in kg_files) {
    crop <- sub("^yield_kg_(.*)\\.rds$", "\\1", basename(f))
    models$yield_kg[[crop]] <- readRDS(f)
  }
  for (f in kcal_files) {
    crop <- sub("^yield_kcal_(.*)\\.rds$", "\\1", basename(f))
    models$yield_kcal[[crop]] <- readRDS(f)
  }
  
  models
}
