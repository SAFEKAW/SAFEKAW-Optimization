suppressPackageStartupMessages({
  library(here)
})

run_stage <- function(script, envir = new.env(parent = globalenv())) {
  message("\n------------------------------")
  message("Running: ", script)
  message("------------------------------")
  sys.source(here("hpc_opt", "scripts", script), envir = envir)
  invisible(TRUE)
}

gcm_names <- c("ensemble")
climate_pathways <- c("rcp45", "rcp85")

landuse_files <- c(
  here("hpc_opt", "config", "landuse", "fixed.yaml"),
  here("hpc_opt", "config", "landuse", "bau.yaml")
  # here("hpc_opt", "config", "landuse", "crp.yaml")
)

irrigation_files <- c(
  here("hpc_opt", "config", "irrigation", "current.yaml"),
  here("hpc_opt", "config", "irrigation", "efficient.yaml")
)

fertilizer_files <- c(
  here("hpc_opt", "config", "fertilizer", "current.yaml"),
  here("hpc_opt", "config", "fertilizer", "efficient.yaml")
)

for (gcm in gcm_names) {
  for (pathway in climate_pathways) {
    for (lu in landuse_files) {
      for (irr in irrigation_files) {
        for (fert in fertilizer_files) {
          
          env <- new.env(parent = globalenv())
          
          env$gcm_name <- gcm
          env$climate_pathway <- pathway
          env$landuse_file <- lu
          env$irrigation_file <- irr
          env$fertilizer_file <- fert
          
          message("\nRunning combo:")
          message("  gcm      = ", gcm)
          message("  pathway  = ", pathway)
          message("  landuse  = ", basename(lu))
          message("  irrig    = ", basename(irr))
          message("  fert     = ", basename(fert))
          
          run_stage("04_run_deterministic_factorial.R", envir = env)
        }
      }
    }
  }
}

message("All deterministic factorial runs complete.")