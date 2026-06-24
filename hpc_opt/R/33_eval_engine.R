# hpc_opt/R/33_eval_engine.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

eval_candidate_year <- function(
    x, Y,
    df_county,
    irrigation_lm,
    yield_kg_models,
    yield_kcal_models,
    wq_model_mlr,
    wq_base_by_year,
    lu_baseline,
    universal_costs,
    crop_params,
    baseline_irrig_frac,
    fert_ref_by_year,
    policy = NULL,
    hist_mix = NULL,
    shares_override = NULL
) {
  PEN <- 1e12
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ---- decision variables / crop shares ----
  if (!is.null(shares_override)) {
    req_names <- c("Corn", "Soybeans", "Sorghum", "Wheat")
    if (!all(req_names %in% names(shares_override))) {
      stop("shares_override must be a named vector with: Corn, Soybeans, Sorghum, Wheat")
    }
    shares <- shares_override[req_names]
    
    if (any(!is.finite(shares))) return(c(PEN, PEN, PEN))
    if (any(shares < 0) || any(shares > 1)) return(c(PEN, PEN, PEN))
    if (abs(sum(shares) - 1) > 1e-8) return(c(PEN, PEN, PEN))
  } else {
    x <- unname(x)
    corn <- x[1]; soy <- x[2]; sor <- x[3]
    wheat <- 1 - corn - soy - sor
    
    if (any(c(corn, soy, sor, wheat) < 0) || any(c(corn, soy, sor, wheat) > 1)) {
      return(c(PEN, PEN, PEN))
    }
    
    shares <- c(Corn = corn, Soybeans = soy, Sorghum = sor, Wheat = wheat)
    
    if (isTRUE(policy$debug_crop_alloc)) {
      message(
        "Year ", Y, " | optimizer shares: ",
        paste(names(shares), round(shares, 3), collapse = " | ")
      )
    }
  }
  
  # ---- baseline land cover for year ----
  lu_y <- lu_baseline[lu_baseline$Year == Y, ]
  if (nrow(lu_y) != 1L) return(c(PEN, PEN, PEN))
  
  policy <- policy %||% list()
  cult_area_factor <- policy$cult_area_factor %||% 1
  
  LC_cult_base <- lu_y$LC_cult_base
  LC_dev_base  <- lu_y$LC_dev_base
  LC_total     <- lu_y$LC_total
  
  avail_nondeveloped_m2 <- LC_total - LC_dev_base
  Cult_m2 <- LC_cult_base * cult_area_factor
  Cult_m2 <- max(0, min(avail_nondeveloped_m2, Cult_m2))
  Grass_m2 <- avail_nondeveloped_m2 - Cult_m2
  
  # ---- irrigated fraction policy ----
  irrig_frac_factor <- policy$irrig_frac_factor %||% 1
  theta_irrig_policy <- baseline_irrig_frac * irrig_frac_factor
  theta_irrig_policy <- max(0, min(1, theta_irrig_policy))
  
  # ---- irrigation efficiency (affects withdrawals/cost only, not yield) ----
  irr_eff <- policy$irr_eff %||% 1
  irr_eff <- max(1, irr_eff)
  
  # ---- fertilizer policy ----
  fert_factor <- policy$fert_factor %||% 1
  fert_share_direct <- universal_costs$fert_share_direct %||% 0.2
  
  # ---- subset county/crop rows for this year ----
  crops4 <- c("Corn", "Soybeans", "Sorghum", "Wheat")
  
  df_crop_y <- df_county %>%
    filter(Year == Y, Crop %in% crops4) %>%
    mutate(FIPS = as.character(FIPS))
  
  if (nrow(df_crop_y) == 0) return(c(PEN, PEN, PEN))
  
  # keep only needed columns from county table; join economics params from crop table
  df_crop_y <- df_crop_y %>%
    select(any_of(c(
      "Year", "FIPS", "Crop", "area_m2", "area_ha", "area_prc",
      "n_pdiv", "GDD", "precip_m", "tmax_C_avg", "tmin_C_avg",
      "fertilizer_kgHa", "fertilizer_kgCrop"
    ))) %>%
    left_join(
      crop_params %>%
        rename(fert_kgha = any_of("fert_kgha")),
      by = "Crop"
    )
  
  # ---- reallocate total cultivated area across county x crop rows ----
  # preserve each crop's county distribution from the baseline climate table,
  # but scale total cultivated area and crop mix to the scenario.
  area_by_crop <- df_crop_y %>%
    group_by(Crop) %>%
    summarise(base_crop_area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop")
  
  if (any(!is.finite(area_by_crop$base_crop_area_m2)) || sum(area_by_crop$base_crop_area_m2) <= 0) {
    return(c(PEN, PEN, PEN))
  }
  
  crop_targets <- tibble(
    Crop = names(shares),
    share = as.numeric(shares),
    target_crop_area_m2 = Cult_m2 * share
  )
  
  df_crop_y <- df_crop_y %>%
    left_join(area_by_crop, by = "Crop") %>%
    left_join(crop_targets, by = "Crop") %>%
    mutate(
      within_crop_wt = if_else(base_crop_area_m2 > 0, area_m2 / base_crop_area_m2, 0),
      area_m2 = target_crop_area_m2 * within_crop_wt,
      area_ha = area_m2 / 1e4
    )
  
  if (isTRUE(policy$debug_crop_alloc)) {
    df_crop_y %>%
      group_by(Crop) %>%
      summarise(
        allocated_area_m2 = sum(area_m2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        allocated_share = allocated_area_m2 / sum(allocated_area_m2, na.rm = TRUE)
      ) %>%
      print(n = Inf)
  }
  
  # ---- allocate irrigated vs rainfed area by crop (same logic as before) ----
  alloc <- allocate_irrigation(
    crop_shares = shares,
    theta_irrig = theta_irrig_policy,
    hist_mix    = hist_mix,
    mode        = "preserve_hist_mix"
  )
  
  irrig_share_by_crop <- tibble(
    Crop = names(alloc$irrig),
    irrig_share_crop = as.numeric(alloc$irrig / shares[names(alloc$irrig)])
  ) %>%
    mutate(
      irrig_share_crop = if_else(is.finite(irrig_share_crop), irrig_share_crop, 0),
      irrig_share_crop = pmax(0, pmin(1, irrig_share_crop))
    )
  
  rain_share_by_crop <- tibble(
    Crop = names(alloc$rainfed),
    rain_share_crop = as.numeric(alloc$rainfed / shares[names(alloc$rainfed)])
  ) %>%
    mutate(
      rain_share_crop = if_else(is.finite(rain_share_crop), rain_share_crop, 0),
      rain_share_crop = pmax(0, pmin(1, rain_share_crop))
    )
  
  df_crop_y <- df_crop_y %>%
    left_join(irrig_share_by_crop, by = "Crop") %>%
    left_join(rain_share_by_crop, by = "Crop") %>%
    mutate(
      irrig_share_crop = coalesce(irrig_share_crop, 0),
      rain_share_crop  = coalesce(rain_share_crop, 1 - irrig_share_crop)
    )
  
  df_irrig <- df_crop_y %>%
    mutate(
      Management = "irrigated",
      area_m2 = area_m2 * irrig_share_crop,
      area_ha = area_m2 / 1e4
    ) %>%
    filter(area_m2 > 0)
  
  # ---- hard irrigated area cap ----
  irrig_area_cap_m2 <- policy$irrig_area_cap_m2 %||% Inf
  irrig_area_cap_m2 <- as.numeric(irrig_area_cap_m2)
  
  irrig_area_m2 <- sum(df_irrig$area_m2, na.rm = TRUE)
  
  if (isTRUE(policy$debug)) {
    message("Year = ", Y)
    message("baseline_irrig_frac = ", baseline_irrig_frac)
    message("irrig_frac_factor = ", irrig_frac_factor)
    message("theta_irrig_policy = ", theta_irrig_policy)
    message("irrig_area_m2 = ", irrig_area_m2)
    message("irrig_area_km2 = ", irrig_area_m2 / 1e6)
    message("irrig_area_cap_m2 = ", irrig_area_cap_m2)
    message("irrig_area_cap_km2 = ", irrig_area_cap_m2 / 1e6)
  }
  
  if (length(irrig_area_cap_m2) != 1L ||
      is.na(irrig_area_cap_m2) ||
      irrig_area_cap_m2 < 0) {
    stop("policy$irrig_area_cap_m2 must be a non-negative number or Inf.")
  }
  
  if (irrig_area_m2 > irrig_area_cap_m2) {
    return(c(PEN, PEN, PEN))
  }

  
  df_rain <- df_crop_y %>%
    mutate(
      Management = "rainfed",
      area_m2 = area_m2 * rain_share_crop,
      area_ha = area_m2 / 1e4
    ) %>%
    filter(area_m2 > 0)
  
  df_rows <- bind_rows(df_irrig, df_rain)
  if (nrow(df_rows) == 0) return(c(PEN, PEN, PEN))
  
  
  
  
  # ---- predict irrigation depth (county x crop rows) ----
  # use crop eligibility only; no WaterManagement column required
  stopifnot(!is.null(irrigation_lm))
  df_rows <- predict_irrigation_depth(
    df_all = df_rows,
    irrigation_lm = irrigation_lm,
    irrigated_crops = c("Corn", "Soybeans"),
    require_irrigated_mgmt = FALSE
  )
  
  # ---- irrigation depth floor for irrigated rows ----
  # Based on historical alluvial irrigation depth distribution:
  # p5 ≈ 0.033 m, p10 ≈ 0.054 m, p25 ≈ 0.098 m
  irr_depth_floor_m <- policy$irr_depth_floor_m %||% 0.033
  
  # force rainfed rows to zero irrigation;
  # keep irrigated rows at or above historical low-end supplemental irrigation depth
  df_rows <- df_rows %>%
    mutate(
      irr_depth_m_pred = if_else(
        Management == "rainfed",
        0,
        pmax(coalesce(irr_depth_m_pred, 0), irr_depth_floor_m)
      ),
      irrigation_WaterUse_m = irr_depth_m_pred,
      Irrigation_m3Ha_pred = irr_depth_m_pred * 10000,
      Irrigation_m3_total = irr_depth_m_pred * area_m2
    )
  
  
  if (isTRUE(policy$debug)) {
    df_rows %>%
      group_by(Year, Management, Crop) %>%
      summarise(
        area_m2 = sum(area_m2, na.rm = TRUE),
        mean_irr_depth = mean(irr_depth_m_pred, na.rm = TRUE),
        total_irr_m3 = sum(Irrigation_m3_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      print(n = 50)
  }
  
  # ---- predict yield using total water (efficiency does NOT affect yield) ----
  df_rows <- df_rows %>%
    mutate(
      totalWater_m = precip_m + irrigation_WaterUse_m
    )
  
  df_rows <- predict_yields(
    df = df_rows,
    yield_kg_models   = yield_kg_models,
    yield_kcal_models = yield_kcal_models,
    fixed_only = FALSE
  )
  
  df_rows <- df_rows %>%
    rename(yield_kgHa_detrended_fit = yield_kgHa_pred) %>%
    mutate(
      yield_kgHa_detrended_fit = pmax(0, yield_kgHa_detrended_fit)
    )
  
  if (all(is.na(df_rows$yield_kgHa_detrended_fit))) return(c(PEN, PEN, PEN))
  
  # ---- effective withdrawals and fertilizer policy ----
  df_rows <- df_rows %>%
    mutate(
      Irrigation_m_eff = if_else(Management == "irrigated", irr_depth_m_pred / irr_eff, 0),
      Irrigation_m3Ha_eff = Irrigation_m_eff * 10000,
      Irrigation_m3_total_eff = Irrigation_m_eff * area_m2,
      
      fert_kgHa = coalesce(fert_kgha, fertilizer_kgHa, 0) * fert_factor,
      fertilizer_kgCrop = fert_kgHa * area_ha
    )
  
  # ---- economics ----
  df_rows <- df_rows %>%
    mutate(
      direct_cost_per_kg_eff =
        direct_cost_per_kg * (1 - fert_share_direct + fert_share_direct * fert_factor),
      total_cost_per_kg_eff =
        direct_cost_per_kg_eff + fixed_cost_per_kg,
      
      revenue_total   = income_per_kg * coalesce(yield_kgHa_detrended_fit, 0) * area_ha,
      base_cost_total = total_cost_per_kg_eff * coalesce(yield_kgHa_detrended_fit, 0) * area_ha,
      irr_cost_total  = Irrigation_m3_total_eff * (universal_costs$irr_cost_per_m3 %||% 0)
    )
  
  Fert_kg_total <- sum(df_rows$fertilizer_kgCrop, na.rm = TRUE)
  Irr_m3_total  <- sum(df_rows$Irrigation_m3_total_eff, na.rm = TRUE)
  
  #later for irr vol cap  
  #  if (Irr_m3_total > policy$irrig_volume_cap_m3) {
  #    return(c(PEN, PEN, PEN))
  #  }
  
  
  Revenue_total  <- sum(df_rows$revenue_total, na.rm = TRUE)
  BaseCost_total <- sum(df_rows$base_cost_total, na.rm = TRUE)
  Irr_totalCost  <- sum(df_rows$irr_cost_total, na.rm = TRUE)
  
  NR_total <- Revenue_total - (BaseCost_total + Irr_totalCost)
  
  # ---- water quality stays at basin/year scale ----
  BasinArea_m2 <- Cult_m2 + Grass_m2 + LC_dev_base
  
  pred_row <- tibble(
    Year = Y,
    BasinArea_m2 = BasinArea_m2,
    dev_frac  = ifelse(BasinArea_m2 > 0, LC_dev_base / BasinArea_m2, NA_real_),
    cult_frac = ifelse(BasinArea_m2 > 0, Cult_m2 / BasinArea_m2, NA_real_)
  ) %>%
    left_join(
      wq_base_by_year %>%
        select(Year, Climate_precip_m, Climate_Tmean_C, precip_percentileChange) %>%
        distinct(Year, .keep_all = TRUE),
      by = "Year"
    )
  
  if (anyNA(pred_row)) return(c(PEN, PEN, PEN))
  
  N0 <- pmax(0, as.numeric(predict(wq_model_mlr, newdata = pred_row)))
  
  # ---- fertilizer multiplier ----
  gamma <- policy$fert_gamma %||% 0.1
  Fert_policy_kg <- Fert_kg_total
  
  Fert_ref_kg <- fert_ref_by_year$Fert_ref_kg[fert_ref_by_year$Year == Y]
  if (length(Fert_ref_kg) != 1L || is.na(Fert_ref_kg) || Fert_ref_kg <= 0) {
    return(c(PEN, PEN, PEN))
  }
  
  mult <- fert_multiplier(Fert_policy_kg, Fert_ref_kg, gamma = gamma)
  mult <- pmin(mult, 5)  # prevent explosion
  nitrate_kgyr <- N0 * mult
  
  c(nitrate_kgyr, -NR_total, Irr_m3_total)
}

eval_candidate_all_years <- function(
    x, years_vec,
    df_county,
    irrigation_lm,
    yield_kg_models,
    yield_kcal_models,
    wq_model_mlr,
    wq_base_by_year,
    lu_baseline, universal_costs, crop_params,
    baseline_irrig_frac,
    hist_mix = NULL,
    fert_ref_by_year,
    policy = NULL,
    weights = NULL,
    na_penalty = 1e9
) {
  stopifnot(length(years_vec) > 0)
  
  vals <- vapply(
    years_vec,
    \(Y) eval_candidate_year(
      x = x, Y = Y,
      df_county = df_county,
      irrigation_lm = irrigation_lm,
      yield_kg_models = yield_kg_models,
      yield_kcal_models = yield_kcal_models,
      wq_model_mlr = wq_model_mlr,
      wq_base_by_year = wq_base_by_year,
      lu_baseline = lu_baseline,
      universal_costs = universal_costs,
      crop_params = crop_params,
      baseline_irrig_frac = baseline_irrig_frac,
      hist_mix = hist_mix,
      fert_ref_by_year = fert_ref_by_year,
      policy = policy
    ),
    numeric(3)
  )
  
  vals <- t(vals)
  colnames(vals) <- c("n_flux_kgyr", "neg_profit_usdyr", "irr_m3_yr")
  
  if (is.null(weights)) weights <- rep(1, nrow(vals))
  if (length(weights) != nrow(vals)) stop("weights must match length(years_vec).")
  
  wmean <- function(v, w) {
    keep <- is.finite(v) & is.finite(w) & w > 0
    if (!any(keep)) return(NA_real_)
    wk <- w[keep] / sum(w[keep])
    sum(v[keep] * wk)
  }
  
  out <- c(
    mean_n_flux_kgyr      = wmean(vals[, "n_flux_kgyr"], weights),
    mean_neg_profit_usdyr = wmean(vals[, "neg_profit_usdyr"], weights),
    mean_irr_m3_yr        = wmean(vals[, "irr_m3_yr"], weights)
  )
  
  if (any(!is.finite(out))) out[] <- na_penalty
  out
}