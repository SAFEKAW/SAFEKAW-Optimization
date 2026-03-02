# hpc_opt/R/20_integrate_historical.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

# sources: your new HPC wrappers
# - predict_irrigation_depth()
# - predict_yields()
# - build_wq_input_from_basin()
# - predict_wq_flux()
# - compute_net_returns()
integrate_historical <- function(df_combined_county,
                                 common_input_basin,
                                 df_yield_obs,
                                 df_nitrate_obs,
                                 models,
                                 crop_params,
                                 universal_costs,
                                 yrs_common = 2006:2023) {
  
  # ---- subset years ----
  df_combined_county <- df_combined_county %>% filter(Year %in% yrs_common)
  common_input_basin <- common_input_basin %>% filter(Year %in% yrs_common)
  df_yield_obs       <- df_yield_obs %>% filter(Year %in% yrs_common)
  df_nitrate_obs     <- df_nitrate_obs %>% filter(Year %in% yrs_common)
  
  df_combined_county <- df_combined_county %>%
    dplyr::mutate(FIPS = as.character(FIPS))
  
  df_yield_obs <- df_yield_obs %>%
    dplyr::mutate(FIPS = as.character(FIPS))
  
  # ---- 1) Irrigation predictions at county/crop rows ----
  stopifnot(!is.null(models$irrigation_lm))
  irr_aug <- predict_irrigation_depth(
    df_all = df_combined_county,
    irrigation_lm = models$irrigation_lm,
    irrigated_crops = c("Corn","Soybeans"),
    require_irrigated_mgmt = TRUE
  )
  
  # ---- 2) Build crop modeling df: join yield obs + totalWater ----
  df_crop <- irr_aug %>%
    filter(!is.na(Crop), Year %in% yrs_common) %>%
    mutate(
      # If WaterManagement exists, force rainfed/non-irrigated to 0; otherwise keep predicted irrigation.
      irrigation_WaterUse_m = if ("WaterManagement" %in% names(.)) {
        dplyr::if_else(WaterManagement %in% c("Rainfed","Non-Irrigated"), 0, dplyr::coalesce(irrigation_WaterUse_m, 0))
      } else {
        dplyr::coalesce(irrigation_WaterUse_m, 0)
      },
      totalWater_m = precip_m + irrigation_WaterUse_m
    ) %>%
    left_join(
      df_yield_obs %>% mutate(FIPS = as.character(FIPS)),
      by = c("Year","FIPS","Crop")
    ) %>%
    filter(is.finite(totalWater_m))
  
  # optional area threshold (kept)
  if ("area_ha" %in% names(df_crop)) {
    area_ha_thres <- 64.75 * 10
    df_crop <- df_crop %>% filter(area_ha > area_ha_thres)
  }
  
  # ---- 3) Yield predictions (NO fitting; fixed effects only recommended) ----
  df_crop_pred <- predict_yields(
    df = df_crop,
    yield_kg_models   = models$yield_kg,
    yield_kcal_models = models$yield_kcal,
    fixed_only = TRUE
  )
  
  # For compatibility with your economics wrapper
  df_crop_pred <- df_crop_pred %>%
    rename(
      yield_kgHa_detrended_fit = yield_kgHa_pred
    )
  
  # ---- 4) Water-quality input + predictions (NO fitting) ----
  # IMPORTANT: common_input_basin must use LandCover_m2_all_cult (no space)
  stopifnot(!is.null(models$wq_lm))
  wq_input <- build_wq_input_from_basin(common_input_basin, irr_aug = irr_aug, use_modeled_irr = TRUE) %>%
    left_join(df_nitrate_obs %>% select(Year, NitrateFlux_kg), by = "Year")
  
  wq_pred <- predict_wq_flux(wq_input, wq_lm = models$wq_lm)
  
  # ---- 5) Net returns (per row) ----
  df_combo_NR <- compute_net_returns(df_crop_pred, crop_params, universal_costs)
  
  # ---- 6) Crop-basin per-ha summaries (matches your script) ----
  crop_basin_perha <- df_combo_NR %>%
    group_by(Year, Crop) %>%
    summarise(
      Crop_area_m2 = sum(area_m2, na.rm = TRUE),
      Crop_area_ha = sum(dplyr::coalesce(area_ha, area_m2/10000), na.rm = TRUE),
      
      Yield_kgHa   = weighted.mean(yield_kgHa_detrended_fit, w = area_m2, na.rm = TRUE),
      Irrigation_m = weighted.mean(irr_depth_m_pred,         w = area_m2, na.rm = TRUE),
      
      Irrigation_m3 = sum(Irrigation_m3_total, na.rm = TRUE),
      fert_kgcrop   = sum(fertilizer_kgCrop, na.rm = TRUE),
      
      NetReturn_ha  = weighted.mean(NetReturn_dollar_ha, w = area_m2, na.rm = TRUE),
      
      Crop_prc = mean(area_prc, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(wq_pred %>% select(Year, NitrateFluxPredicted_kg), by = "Year")
  
  # ---- 7) Basin totals per year ----
  basin_totals <- crop_basin_perha %>%
    mutate(
      Yield_total_kg      = Crop_area_ha * Yield_kgHa,
      NetReturn_total_usd = Crop_area_ha * NetReturn_ha
    ) %>%
    group_by(Year) %>%
    summarise(
      Yield_total_kg      = sum(Yield_total_kg,      na.rm = TRUE),
      NetReturn_total_usd = sum(NetReturn_total_usd, na.rm = TRUE),
      Irrigation_total_m3 = sum(Irrigation_m3,       na.rm = TRUE),
      Area_m2             = sum(Crop_area_m2,        na.rm = TRUE),
      Area_ha             = sum(Crop_area_ha,        na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(wq_pred %>% select(Year, NitrateFluxPredicted_kg), by = "Year")
  
  # ---- 8) Irrigated fraction by year (kept) ----
  irr_frac_YR <- NULL
  if ("WaterManagement" %in% names(df_combo_NR)) {
    irr_frac_YR <- df_combo_NR %>%
      group_by(Year, WaterManagement) %>%
      summarise(area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop_last") %>%
      mutate(total_area_allcrops = sum(area_m2, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(frac_of_basin_cultivated = area_m2 / total_area_allcrops)
  }
  
  list(
    irr_aug = irr_aug,
    df_crop_pred = df_crop_pred,
    wq_pred = wq_pred,
    df_combo_NR = df_combo_NR,
    crop_basin_perha = crop_basin_perha,
    basin_totals = basin_totals,
    irr_frac_YR = irr_frac_YR
  )
}
