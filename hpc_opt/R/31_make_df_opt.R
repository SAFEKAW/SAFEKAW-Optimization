# hpc_opt/R/31_make_df_opt.R
suppressPackageStartupMessages({
  library(dplyr)
})

make_df_opt_from_integration <- function(crop_basin_perha, crop_params) {
  # crop_basin_perha must include: Year, Crop, Yield_kgHa, Irrigation_m
  stopifnot(all(c("Year","Crop","Yield_kgHa","Irrigation_m") %in% names(crop_basin_perha)))
  
  # fert_kgha comes from crop_params (or you can override later with policy)
  df_opt <- crop_basin_perha %>%
    select(Year, Crop, Yield_kgHa, Irrigation_m) %>%
    left_join(crop_params %>% select(Crop, fert_kgha = fert_kgHa), by = "Crop")
  
  df_opt
}