suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
})

gcms <- c(
  "ccsm4",
  "bcc_csm1_1",
  "gfdl_esm2m",
  "canesm2",
  "cnrm_cm5"
)

scenario_tags <- c(
  "rcp45_early", "rcp45_mid", "rcp45_late",
  "rcp85_early", "rcp85_mid", "rcp85_late"
)

make_climate_ensemble <- function(scenario_tag) {
  
  message("Building ensemble climate files for: ", scenario_tag)
  
  # ---- daily county climate ----
  climate_files <- here(
    "data",
    paste0("ClimateData_County_", gcms, "_", scenario_tag, ".csv")
  )
  
  if (!all(file.exists(climate_files))) {
    missing <- climate_files[!file.exists(climate_files)]
    stop(
      "Missing climate files for ", scenario_tag, ":\n",
      paste(missing, collapse = "\n")
    )
  }
  
  climate_all <- map2_dfr(
    climate_files,
    gcms,
    ~ read_csv(.x, show_col_types = FALSE) %>%
      mutate(gcm = .y)
  )
  
  climate_ensemble <- climate_all %>%
    group_by(FIPS, name, Date, Year, Month, Day) %>%
    summarise(
      precip_mm_gridmet_mean = mean(precip_mm_gridmet, na.rm = TRUE),
      precip_mm_gridmet_sd   = sd(precip_mm_gridmet, na.rm = TRUE),
      
      tmmn_C_gridmet_mean = mean(tmmn_C_gridmet, na.rm = TRUE),
      tmmn_C_gridmet_sd   = sd(tmmn_C_gridmet, na.rm = TRUE),
      
      tmmx_C_gridmet_mean = mean(tmmx_C_gridmet, na.rm = TRUE),
      tmmx_C_gridmet_sd   = sd(tmmx_C_gridmet, na.rm = TRUE),
      
      n_gcms = n_distinct(gcm),
      .groups = "drop"
    ) %>%
    rename(
      precip_mm_gridmet = precip_mm_gridmet_mean,
      tmmn_C_gridmet    = tmmn_C_gridmet_mean,
      tmmx_C_gridmet    = tmmx_C_gridmet_mean
    ) %>%
    arrange(FIPS, Date)
  
  out_climate <- here(
    "data",
    paste0("ClimateData_County_ensemble_", scenario_tag, ".csv")
  )
  
  write_csv(climate_ensemble, out_climate)
  message("Wrote: ", out_climate)
  
  # ---- crop growing-season climate + GDD ----
  crop_files <- here(
    "data",
    paste0("crop_climate_gs_", gcms, "_", scenario_tag, ".csv")
  )
  
  if (!all(file.exists(crop_files))) {
    missing <- crop_files[!file.exists(crop_files)]
    stop(
      "Missing crop climate files for ", scenario_tag, ":\n",
      paste(missing, collapse = "\n")
    )
  }
  
  crop_all <- map2_dfr(
    crop_files,
    gcms,
    ~ read_csv(.x, show_col_types = FALSE) %>%
      mutate(gcm = .y)
  )
  
  crop_ensemble <- crop_all %>%
    group_by(FIPS, Year, crop) %>%
    summarise(
      GDD_mean = mean(GDD, na.rm = TRUE),
      GDD_sd   = sd(GDD, na.rm = TRUE),
      
      n_days_mean = mean(n_days, na.rm = TRUE),
      n_days_sd   = sd(n_days, na.rm = TRUE),
      
      precip_gs_mm_mean = mean(precip_gs_mm, na.rm = TRUE),
      precip_gs_mm_sd   = sd(precip_gs_mm, na.rm = TRUE),
      
      ppt_n_days_mean = mean(ppt_n_days, na.rm = TRUE),
      ppt_n_days_sd   = sd(ppt_n_days, na.rm = TRUE),
      
      n_gcms = n_distinct(gcm),
      .groups = "drop"
    ) %>%
    rename(
      GDD          = GDD_mean,
      n_days       = n_days_mean,
      precip_gs_mm = precip_gs_mm_mean,
      ppt_n_days   = ppt_n_days_mean
    ) %>%
    arrange(crop, FIPS, Year)
  
  out_crop <- here(
    "data",
    paste0("crop_climate_gs_ensemble_", scenario_tag, ".csv")
  )
  
  write_csv(crop_ensemble, out_crop)
  message("Wrote: ", out_crop)
  
  # ---- optional legacy GDD-only file ----
  gdd_ensemble <- crop_ensemble %>%
    select(
      FIPS, Year, crop,
      GDD, GDD_sd,
      n_days, n_days_sd,
      n_gcms
    )
  
  out_gdd <- here(
    "data",
    paste0("gdd_all_gs_ensemble_", scenario_tag, ".csv")
  )
  
  write_csv(gdd_ensemble, out_gdd)
  message("Wrote: ", out_gdd)
  
  invisible(
    list(
      climate = climate_ensemble,
      crop_climate = crop_ensemble,
      gdd = gdd_ensemble
    )
  )
}

walk(scenario_tags, make_climate_ensemble)
