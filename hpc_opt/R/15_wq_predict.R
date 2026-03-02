# hpc_opt/R/15_wq_predict.R
suppressPackageStartupMessages({
  library(dplyr)
})

# fit once (prep step)
fit_wq_lm <- function(df) {
  df <- add_wq_features(df)
  stats::lm(
    NitrateFlux_kg ~ cult_frac + Climate_precip_m + Climate_Tmean_C + precip_percentileChange,
    data = df
  )
}

# predict only
predict_wq_flux <- function(df, wq_lm) {
  stopifnot(!is.null(wq_lm))
  df <- add_wq_features(df)
  df$NitrateFluxPredicted_kg <- stats::predict(wq_lm, newdata = df)
  df
}

# build the basin-year WQ input from basin table + (optional) county irrigation augmented
build_wq_input_from_basin <- function(common_input_basin, irr_aug = NULL, use_modeled_irr = TRUE) {
  
  wq_in <- common_input_basin %>%
    select(Year, starts_with("Climate_"), starts_with("LandCover_m2_"), any_of("Management_FertilizerUse_kg")) %>%
    mutate(
      Climate_Tmean_C = coalesce(Climate_Tmean_C, (Climate_Tmin_C + Climate_Tmax_C)/2),
      BasinArea_m2 = LandCover_m2_all_cult + LandCover_m2_grassPastureHay + LandCover_m2_developed,
      Management_FertilizerUse_kgm2 = if ("Management_FertilizerUse_kg" %in% names(.)) {
        Management_FertilizerUse_kg / BasinArea_m2
      } else NA_real_
    ) %>%
    arrange(Year)
  
  # irrigation depth (either from common_input_basin or recompute from irr_aug)
  if (use_modeled_irr && !is.null(irr_aug)) {
    stopifnot(all(c("Year","irrigation_WaterUse_m","area_m2") %in% names(irr_aug)))
    irr_depth_basin <- irr_aug %>%
      group_by(Year) %>%
      summarise(Management_irrigation_m = wmean(irrigation_WaterUse_m, area_m2), .groups = "drop")
    
    wq_in <- wq_in %>% left_join(irr_depth_basin, by = "Year")
  } else {
    if (!("Management_irrigation_m" %in% names(wq_in))) wq_in$Management_irrigation_m <- NA_real_
  }
  
  # precip percentile and change
  ec <- stats::ecdf(wq_in$Climate_precip_m)
  wq_in <- wq_in %>%
    mutate(
      precip_percentile       = ec(Climate_precip_m),
      precip_percentileChange = tidyr::replace_na(precip_percentile - dplyr::lag(precip_percentile), 0)
    )
  
  add_wq_features(wq_in)
}
