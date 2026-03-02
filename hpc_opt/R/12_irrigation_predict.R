# hpc_opt/R/12_irrigation_predict.R
suppressPackageStartupMessages({
  library(dplyr)
})

# Fit once (prep step), not inside optimization
fit_irrigation_lm <- function(df_all, irrigated_crops = c("Corn", "Soybeans")) {
  df_fit <- df_all %>%
    filter(Crop %in% irrigated_crops, n_pdiv > 5) %>%
    tidyr::drop_na(precip_m, irrigation_WaterUse_m, tmax_C_avg)
  
  stats::lm(irrigation_WaterUse_m ~ precip_m + tmax_C_avg, data = df_fit)
}

# Predict only; does NOT refit
predict_irrigation_depth <- function(df_all,
                                     irrigation_lm,
                                     irrigated_crops = c("Corn","Soybeans"),
                                     require_irrigated_mgmt = TRUE) {
  
  stopifnot(!is.null(irrigation_lm))
  
  df_out <- df_all
  df_out$irr_depth_m_pred     <- 0
  df_out$Irrigation_m3Ha_pred <- 0
  df_out$Irrigation_m3_total  <- 0
  
  eligible <- df_out$Crop %in% irrigated_crops
  if (require_irrigated_mgmt && "WaterManagement" %in% names(df_out)) {
    eligible <- eligible & df_out$WaterManagement != "Non-Irrigated"
  }
  
  if (any(eligible, na.rm = TRUE)) {
    preds <- stats::predict(irrigation_lm, newdata = df_out[eligible, , drop = FALSE])
    preds <- pmax(0, preds)
    
    df_out$irr_depth_m_pred[eligible]     <- preds
    df_out$Irrigation_m3Ha_pred[eligible] <- preds * 10000
    if ("area_m2" %in% names(df_out)) {
      df_out$Irrigation_m3_total[eligible] <- preds * df_out$area_m2[eligible]
    }
  }
  
  # downstream expects this column name
  df_out$irrigation_WaterUse_m <- df_out$irr_depth_m_pred
  df_out
}
