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
  
  # Fixed-effects part: same as before
  if (crop == "Sorghum") {
    fixed_kcal <- yield_kcalHa_detrended ~ poly(totalWater_m, 2) + tmin_C_avg + tmax_C_avg
    fixed_kg   <- yield_kgHa_detrended   ~ poly(totalWater_m, 2) + tmin_C_avg + tmax_C_avg
  } else {
    fixed_kcal <- yield_kcalHa_detrended ~ poly(totalWater_m, 2) + tmin_C_avg + tmax_C_avg
    fixed_kg   <- yield_kgHa_detrended   ~ poly(totalWater_m, 2) + tmin_C_avg + tmax_C_avg
  }
  
  # Random effects: county-level FIPS
  formula_kcal <- update(fixed_kcal, . ~ . + (1 | FIPS) + (1 | Year))
  formula_kg   <- update(fixed_kg,   . ~ . + (1 | FIPS) + (1 | Year))
  
  fit_kcal <- lme4::lmer(formula_kcal, data = dfc, REML = TRUE)
  fit_kg   <- lme4::lmer(formula_kg,   data = dfc, REML = TRUE)
  
  dfc$yield_kcalHa_detrended_fit <- fitted(fit_kcal)
  dfc$yield_kgHa_detrended_fit   <- fitted(fit_kg)
  
  # Simple predictive R² (obs vs fitted), one per response
  r2_kcal <- with(dfc,
                  cor(yield_kcalHa_detrended, yield_kcalHa_detrended_fit,
                      use = "complete.obs")^2)
  r2_kg   <- with(dfc,
                  cor(yield_kgHa_detrended, yield_kgHa_detrended_fit,
                      use = "complete.obs")^2)
  
  list(
    crop    = crop,
    fit_kcal = fit_kcal,
    fit_kg   = fit_kg,
    R2 = c(
      kcalHa = r2_kcal,
      kgHa   = r2_kg
    ),
    data = dfc
  )
}

run_crop_models <- function(df) {
  crops   <- sort(unique(df$Crop))
  results <- lapply(crops, function(cr) fit_yield_models(df, cr))
  
  # Combine augmented data
  df_combo_aug <- do.call(rbind, lapply(results, `[[`, "data"))
  
  # Extract R² (note the ["kcalHa"] and ["kgHa"])
  df_fit <- data.frame(
    Crop          = sapply(results, `[[`, "crop"),
    fit_R2_kcalHa = sapply(results, function(x) unname(x$R2["kcalHa"])),
    fit_R2_kgHa   = sapply(results, function(x) unname(x$R2["kgHa"]))
  )
  
  list(
    data      = df_combo_aug,
    fit_stats = df_fit,
    models    = results
  )
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
  

  # 4) NET RETURN (per-row, given modeled irrigation + fitted yields)
  # =========================================================
  #
  # df_rows needs (at least):
  #   Crop
  #   yield_kgHa_detrended_fit   # predicted yield (kg/ha)
  #   Irrigation_m3Ha_pred       # irrigation volume per ha (m3/ha; 0 for NonIrr)
  #
  # df_rows may also have:
  #   fert_kgHa or fertilizer_kgHa  # fertilizer rate (kg/ha, ideally as N)
  #
  # crop_params needs:
  #   Crop
  #   income_per_kg
  #   direct_cost_per_kg
  #   fixed_cost_per_kg
  #   total_cost_per_kg   # direct + fixed, ideally excluding explicit irrigation cost
  #   (optional) net_return_per_kg
  #
  # crop_params may also have:
  #   fert_kgHa            # fertilizer rate per ha (kg/ha as N, or total fert)
  #
  # universal_costs:
  #   list(irr_cost_per_m3 = ...)
  
  compute_net_returns <- function(df_rows, crop_params, universal_costs) {
    `%||%` <- function(a, b) if (is.null(a)) b else a
    
    # --- 0) Harmonize column names in df_rows if needed ----------------
    # Old fertilizer name -> new
    if (!"fert_kgHa" %in% names(df_rows) && "fertilizer_kgHa" %in% names(df_rows)) {
      df_rows <- dplyr::rename(df_rows,  fert_kgHa = fertilizer_kgHa)
    }
    
    # Old irrigation name -> new
    if (!"Irrigation_m3Ha_pred" %in% names(df_rows) && "irr_depth_m_pred" %in% names(df_rows)) {
      df_rows <- dplyr::rename(df_rows, Irrigation_m3Ha_pred = irr_depth_m_pred)
    }
    
    # --- 1) Normalize crop_params --------------------------------------
    cp <- crop_params %>%
      dplyr::mutate(
        income_per_kg      = dplyr::coalesce(income_per_kg, 0),
        direct_cost_per_kg = dplyr::coalesce(direct_cost_per_kg, 0),
        fixed_cost_per_kg  = dplyr::coalesce(fixed_cost_per_kg, 0),
        total_cost_per_kg  = dplyr::coalesce(
          total_cost_per_kg,
          direct_cost_per_kg + fixed_cost_per_kg
        ),
        net_return_per_kg  = dplyr::coalesce(
          net_return_per_kg,
          income_per_kg - total_cost_per_kg
        ),
        # Fertilizer rate per ha (optional, used for N model, not for $)
        fert_kgHa          = dplyr::coalesce(fert_kgHa, NA_real_)
      ) %>%
      # avoid .x/.y suffixes after join if df_rows has fert_kgHa
      dplyr::rename(fert_kgHa_cp = fert_kgHa)
    
    # --- 2) Join and compute per-ha economics --------------------------
    df_rows %>%
      dplyr::left_join(cp, by = "Crop") %>%
      dplyr::mutate(
        # make sure these are non-NA
        yield_kgHa_detrended_fit = dplyr::coalesce(yield_kgHa_detrended_fit, 0),
        Irrigation_m3Ha_pred     = dplyr::coalesce(Irrigation_m3Ha_pred, 0),
        
        # --- Fertilizer (for N model) ----------------------------------
        # Prefer per-row fert_kgHa if present; fall back to crop_params; else 0
        fert_kgHa_eff = dplyr::coalesce(.data$fert_kgHa, .data$fert_kgHa_cp, 0),
        
        # If you want an explicit N input column (optional, comment out if not needed):
        # If fert_kgHa_eff is already kg N/ha, this is just a copy.
        # Otherwise you could multiply by an N fraction (e.g., 0.46) here.
        # fertN_kgHa = fert_kgHa_eff * (universal_costs$fert_N_frac %||% 1),
        
        # --- Revenue and cost side (per ha) ----------------------------
        # Revenue per ha
        revenue_ha = yield_kgHa_detrended_fit * income_per_kg,
        
        # Base costs per ha from KSU budgets (direct + fixed, per kg * yield)
        direct_costs_ha    = yield_kgHa_detrended_fit * direct_cost_per_kg,
        fixed_costs_ha     = yield_kgHa_detrended_fit * fixed_cost_per_kg,
        base_total_costs_ha = yield_kgHa_detrended_fit * total_cost_per_kg,
        
        # Explicit irrigation cost per ha (so we can change scenarios)
        irr_cost_ha = Irrigation_m3Ha_pred * (universal_costs$irr_cost_per_m3 %||% 0),
        
        # Total costs and net returns
        total_costs_ha      = base_total_costs_ha + irr_cost_ha,
        NetReturn_dollar_ha = revenue_ha - total_costs_ha
      )
  }
  
  
