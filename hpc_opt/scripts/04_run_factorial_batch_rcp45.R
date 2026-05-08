suppressPackageStartupMessages({
  library(here)
})

landuse_files <- c(
  here("hpc_opt","config","landuse","fixed.yaml"),
  here("hpc_opt","config","landuse","bau.yaml")#,
  # here("hpc_opt","config","landuse","crp.yaml")
)

irrigation_files <- c(
  here("hpc_opt","config","irrigation","current.yaml"),
  here("hpc_opt","config","irrigation","efficient.yaml")
)

fertilizer_files <- c(
  here("hpc_opt","config","fertilizer","current.yaml"),
  here("hpc_opt","config","fertilizer","efficient.yaml")
)


for (landuse_file in landuse_files) {
  for (irrigation_file in irrigation_files) {
    for (fertilizer_file in fertilizer_files) {
      
      message("\n==============================")
      message("Running combo:")
      message(" landuse    = ", basename(landuse_file))
      message(" irrigation = ", basename(irrigation_file))
      message(" fertilizer = ", basename(fertilizer_file))
      message(" climate    = ", climate_pathway)
      message("==============================\n")
      
      source(here("hpc_opt","scripts","03_run_deterministic_factorial.R"), local = new.env())
      
    }
  }
}