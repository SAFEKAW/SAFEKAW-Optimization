# ===========================
# model_wrappers.R
# SAFEKAW submodel wrappers
# ===========================

    # this will be the file to change/improve models if we decide to do that later!

# ---- Small helpers ----
mae  <- function(a, b, na.rm = TRUE) mean(abs(a - b), na.rm = na.rm)
rmse <- function(a, b, na.rm = TRUE) sqrt(mean((a - b)^2, na.rm = na.rm))

normalize_crop_params <- function(cp) {
  # Accepts tibble/data.frame or list(Crop=list(profit_perkg=, seed_cost_ha=))
  if (inherits(cp, "data.frame")) out <- cp else if (is.list(cp)) {
    out <- tibble(
      Crop = names(cp),
      profit_perkg = vapply(cp, function(x) x[["profit_perkg"]], numeric(1)),
      seed_cost_ha = vapply(cp, function(x) x[["seed_cost_ha"]] %||% x[["seed_cost"]], numeric(1))
    )
  } else stop("crop_params must be a list or data.frame/tibble.")
  
  if ("price_per_kg" %in% names(out) && !"profit_perkg" %in% names(out))
    out <- rename(out, profit_perkg = price_per_kg)
  if ("seed_cost" %in% names(out) && !"seed_cost_ha" %in% names(out))
    out <- rename(out, seed_cost_ha = seed_cost)
  
  out %>% dplyr::select(Crop, profit_perkg, seed_cost_ha) %>%
    mutate(
      Crop = as.character(Crop),
      profit_perkg = as.numeric(profit_perkg),
      seed_cost_ha = as.numeric(seed_cost_ha)
    )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b 

# =========================================================
# 1) IRRIGATION MODELS
# =========================================================

# 1a) Validation wrapper (fit + diagnostics on training subset)
run_irrigation_stat_existing <- function(df_all) {
  df_fit <- df_all |>
    filter(Crop %in% c("Corn", "Soybeans"), n_pdiv > 5) |>
    drop_na(precip_m, irrigation_WaterUse_m, area_prc)
  
  fit_irr <- lm(irrigation_WaterUse_m ~ precip_m + tmax_C_avg, data = df_fit) #probs want to add temps in here too!!
  
  
  df_fit$irr_pred_m <- predict(fit_irr, newdata = df_fit)
  df_fit$irr_pred_m <- pmax(0, df_fit$irr_pred_m)
  
  diagnostics <- list(
    MAE_m  = mae(df_fit$irr_pred_m, df_fit$irrigation_WaterUse_m),
    RMSE_m = rmse(df_fit$irr_pred_m, df_fit$irrigation_WaterUse_m),
    R2     = summary(lm(df_fit$irr_pred_m ~ df_fit$irrigation_WaterUse_m))$r.squared,
    n      = nrow(df_fit)
  )
  
  list(model = fit_irr, data = df_fit, diagnostics = diagnostics)
}

# 1b) Predict only for eligible crops (others forced to 0); returns augmented df
run_irrigation_predict_eligible <- function(
    df_all,
    irrigated_crops = c("Corn","Soybeans"),
    require_irrigated_mgmt = TRUE   # if TRUE, "Non-Irrigated" rows forced to 0
) {
  df_fit <- df_all |>
    filter(Crop %in% irrigated_crops, n_pdiv > 5) |>
    drop_na(precip_m, irrigation_WaterUse_m)
  
  fit_irr <- lm(irrigation_WaterUse_m ~ precip_m + tmax_C_avg, data = df_fit) #probs want to add temps in here too!!
  
  df_out <- df_all
  df_out$irr_depth_m_pred     <- 0
  df_out$Irrigation_m3Ha_pred <- 0
  df_out$Irrigation_m3_total  <- 0
  
  eligible <- df_out$Crop %in% irrigated_crops
  if (require_irrigated_mgmt && "WaterManagement" %in% names(df_out)) {
    eligible <- eligible & df_out$WaterManagement != "Non-Irrigated"
  }
  
  if (any(eligible, na.rm = TRUE)) {
    preds <- predict(fit_irr, newdata = df_out[eligible, , drop = FALSE])
    preds <- pmax(0, preds)
    df_out$irr_depth_m_pred[eligible]     <- preds
    df_out$Irrigation_m3Ha_pred[eligible] <- preds * 10000
    if ("area_m2" %in% names(df_out)) {
      df_out$Irrigation_m3_total[eligible] <- preds * df_out$area_m2[eligible]
    }
  }
  
  #  depth column used downstream
  df_out$irrigation_WaterUse_m <- df_out$irr_depth_m_pred
  
  list(model = fit_irr, data_all_aug = df_out)
}

# =========================================================
# 2) CROP YIELD MODELS
# =========================================================

# Fit both kcal and kg models for a single crop, return augmented data + fits
fit_yield_models <- function(data, crop) {
  dfc <- subset(data, Crop == crop)
  
  # You can customize formulas by crop here
  if (crop == "Sorghum") {
    formula_kcal <- yield_kcalHa_detrended ~ poly(totalWater_m, 2) + tmin_C_avg + tmax_C_avg  #add GDD when I get it here
    formula_kg   <- yield_kgHa_detrended   ~ poly(totalWater_m, 2) + tmin_C_avg + tmax_C_avg
  } else {
    formula_kcal <- yield_kcalHa_detrended ~ poly(totalWater_m, 2) + tmin_C_avg + tmax_C_avg
    formula_kg   <- yield_kgHa_detrended   ~ poly(totalWater_m, 2) + tmin_C_avg + tmax_C_avg
  }
  
  fit_kcal <- lm(formula_kcal, data = dfc)
  fit_kg   <- lm(formula_kg,   data = dfc)
  
  dfc$yield_kcalHa_detrended_fit <- fitted(fit_kcal)
  dfc$yield_kgHa_detrended_fit   <- fitted(fit_kg)
  
  list(
    crop = crop,
    fit_kcal = fit_kcal,
    fit_kg   = fit_kg,
    R2 = c(
      kcalHa = summary(fit_kcal)$adj.r.squared,
      kgHa   = summary(fit_kg)$adj.r.squared
    ),
    data = dfc
  )
}

# Run per-crop fits across all present crops
run_crop_models <- function(df) {
  crops <- sort(unique(df$Crop))
  results <- lapply(crops, function(cr) fit_yield_models(df, cr))
  
  df_combo_aug <- do.call(rbind, lapply(results, `[[`, "data"))
  df_fit <- data.frame(
    Crop = sapply(results, `[[`, "crop"),
    fit_R2_kcalHa = sapply(results, function(x) x$R2["kcalHa"]),
    fit_R2_kgHa   = sapply(results, function(x) x$R2["kgHa"])
  )
  
  list(data = df_combo_aug, fit_stats = df_fit, models = results)
}

# =========================================================
# 3) WATER QUALITY MODEL
# =========================================================

# Fits MLR and returns data with predictions + performance
# Expects columns: NitrateFlux_kg, LandCover_m2_, Climate_, precip_percentileChange,
#                  Management_irrigation_m (depth), Management_FertilizerUse_kg (total)
run_waterquality_model <- function(df) {
  mlr_nitrate <- stats::lm(
    NitrateFlux_kg ~ 
      LandCover_m2_developed +
      LandCover_m2_grassPastureHay +
      `LandCover_m2_all cult` +
      Climate_precip_m +
      Climate_Tmean_C +
      precip_percentileChange +
      Management_irrigation_m +
      Management_FertilizerUse_kgm2, 
    #  Management_FertilizerUse_kg, #add spei here if we feel it's neccessary - need to calc outside of this file 
    data = df
  )
  
  df$NitrateFluxPredicted_kg <- predict(mlr_nitrate, newdata = df)
  
  mae_val  <- mae(df$NitrateFluxPredicted_kg, df$NitrateFlux_kg, na.rm = TRUE)
  nmae_val <- mae_val / (max(df$NitrateFluxPredicted_kg, na.rm = TRUE) -
                           min(df$NitrateFluxPredicted_kg, na.rm = TRUE))
  pred <- df$NitrateFluxPredicted_kg
  obs  <- df$NitrateFlux_kg
  R2_val <- if (all(is.finite(pred)) && all(is.finite(obs))) cor(pred, obs)^2 else NA_real_
  
  list(data = df, model = mlr_nitrate, performance = list(MAE = mae_val, NMAE = nmae_val, R2 = R2_val))
}

# Helper: build basin WQ input by joining modeled irrigation depth (area-weighted)
# common_input_basin: Year + Climate_* + LandCover_m2_* + Management_FertilizerUse_kg (historical)
# irr_aug: county rows with irrigation_WaterUse_m and area_m2

  # assemble base: climate + landcover groups + fertilizer totals
  build_wq_input_from_basin <- function(common_input_basin, irr_aug,
                                        nitrate_df = NULL,
                                        nitrate_year_col = "Year",
                                        nitrate_value_col = "NitrateFlux_kg") {
    
    # 1) Area-weighted modeled irrigation depth (m) over counties
    irr_depth_basin <- irr_aug %>%
      group_by(Year) %>%
      summarise(
        Management_irrigation_m = stats::weighted.mean(irrigation_WaterUse_m, w = area_m2, na.rm = TRUE),
        .groups = "drop"
      )
    
    # 2) Start from basin inputs; compute basin area (same definition used later)
    wq_in <- common_input_basin %>%
      dplyr::select(
        Year,
        starts_with("Climate_"),
        starts_with("LandCover_m2_"),
        any_of("Management_FertilizerUse_kg")  # basin TOTAL fertilizer 
      ) %>%
      mutate(
        Climate_Tmean_C = coalesce(Climate_Tmean_C, (Climate_Tmin_C + Climate_Tmax_C)/2),
        BasinArea_m2 = `LandCover_m2_all cult` + LandCover_m2_grassPastureHay + LandCover_m2_developed
      )
    
    # 3) If a basin TOTAL fertilizer column exists, convert to kg per m² BASIN
    if ("Management_FertilizerUse_kg" %in% names(wq_in)) {
      wq_in <- wq_in %>%
        mutate(
          Management_FertilizerUse_kgm2 = Management_FertilizerUse_kg / BasinArea_m2
        )
    } else {
      wq_in <- wq_in %>%
        mutate(Management_FertilizerUse_kgm2 = NA_real_)
    }
    
    # 4) Join modeled irrigation depth and sort
    wq_in <- wq_in %>%
      left_join(irr_depth_basin, by = "Year") %>%
      arrange(Year)
    
    # 5) Optionally join observed nitrate
    if (!is.null(nitrate_df)) {
      stopifnot(nitrate_year_col %in% names(nitrate_df),
                nitrate_value_col %in% names(nitrate_df))
      nitrate_small <- nitrate_df %>%
        dplyr::select(
          !!rlang::sym(nitrate_year_col),
          !!rlang::sym(nitrate_value_col)
        )
      names(nitrate_small) <- c("Year", "NitrateFlux_kg")
      wq_in <- left_join(wq_in, nitrate_small, by = "Year")
    }
    
    # 6) Precip percentile + change (on basin precip)
    stopifnot("Climate_precip_m" %in% names(wq_in))
    ec <- ecdf(wq_in$Climate_precip_m)
    wq_in <- wq_in %>%
      mutate(
        precip_percentile       = ec(Climate_precip_m),
        precip_percentileChange = replace_na(precip_percentile - lag(precip_percentile), 0)
      )
    
    wq_in
  }
  





# =========================================================
# 4) NET RETURN (per-row, given modeled irrigation + fitted yields)
# =========================================================

# Adds NetReturn_dollar_ha to rows that have:
# yield_kgHa_detrended_fit, Irrigation_m3Ha_pred, Crop (to join crop_params)
# crop_params: tibble/data.frame with Crop, profit_perkg(or price_per_kg), seed_cost_ha
# universal_costs: list(fert_cost_ha=..., irr_cost_per_m3=...)
compute_net_returns <- function(df_rows, crop_params, universal_costs) {
  cp <- normalize_crop_params(crop_params)
  
  df_rows |>
    left_join(cp, by = "Crop") |>
    mutate(
      Irrigation_m3     = coalesce(Irrigation_m3_total, 0),
      profit_perkg             = replace_na(profit_perkg, 0),
      seed_cost_ha             = replace_na(seed_cost_ha, 0),
      yield_kgHa_detrended_fit = coalesce(yield_kgHa_detrended_fit, 0),
      fert_cost_ha             = universal_costs$fert_cost_ha,
      irr_cost_ha              = Irrigation_m3Ha_pred * universal_costs$irr_cost_per_m3,
      profit_ha                = yield_kgHa_detrended_fit * profit_perkg,
      NetReturn_dollar_ha      = profit_ha - (fert_cost_ha + seed_cost_ha + irr_cost_ha)
    )
}
