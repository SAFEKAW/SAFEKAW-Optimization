eval_candidate <- function(
    x,
    Y,
    df_opt,
    wq_model_log1p,
    wq_context,         # this is your per-year climate/features table
    lu_baseline,
    universal_costs,
    crop_params,
    baseline_irrig_frac,
    hist_mix = NULL
){
  PEN <- 1e12
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # ---- 0) decision vector -> crop shares (+ leftover wheat) ---------
  x <- unname(x)
  
  corn <- x[1]; soy <- x[2]; sor <- x[3]
  wheat <- 1 - corn - soy - sor
  
  if (any(c(corn, soy, sor, wheat) < 0) || any(c(corn, soy, sor, wheat) > 1)) {
    return(c(PEN, PEN, PEN))
  }
  
  shares <- c(Corn = corn, Soybeans = soy, Sorghum = sor, Wheat = wheat)
  
  # ---- 1) baseline land cover for this year -------------------------
  lu_y <- lu_baseline[lu_baseline$Year == Y, ]
  if (nrow(lu_y) != 1L) return(c(PEN, PEN, PEN))
  
  LC_cult_base  <- lu_y$LC_cult_base
  LC_grass_base <- lu_y$LC_grass_base
  LC_dev_base   <- lu_y$LC_dev_base
  LC_total      <- lu_y$LC_total
  
  Cult_m2  <- LC_cult_base
  Grass_m2 <- LC_total - LC_dev_base - Cult_m2
  
  # ---- 2) per-ha rows (4 crops) ------------------------------------
  crops4 <- c("Corn","Soybeans","Sorghum","Wheat")
  
  perha <- df_opt %>%
    dplyr::filter(Year == Y, Crop %in% crops4) %>%
    tidyr::complete(
      Year = Y, Crop = crops4,
      fill = list(Yield_kgHa = 0, Irrigation_m = 0)
    ) %>%
    dplyr::select(-dplyr::any_of(names(crop_params)[names(crop_params) != "Crop"])) %>%
    dplyr::left_join(crop_params, by = "Crop")
  
  # ---- 3) irrigation allocation at baseline irrig fraction ---------
  alloc <- allocate_irrigation(
    crop_shares = shares,
    theta_irrig = baseline_irrig_frac,
    hist_mix    = hist_mix,
    mode        = "preserve_hist_mix"
  )
  
  area_irrig_m2 <- Cult_m2 * alloc$irrig
  area_rain_m2  <- Cult_m2 * alloc$rainfed
  area_irrig_ha <- area_irrig_m2 / 1e4
  area_rain_ha  <- area_rain_m2  / 1e4
  
  # ---- 4) build irrigated vs rainfed slices ------------------------
  perha_irrig <- perha %>%
    dplyr::mutate(
      area_m2 = as.numeric(area_irrig_m2[match(Crop, names(area_irrig_m2))]),
      area_ha = as.numeric(area_irrig_ha[match(Crop, names(area_irrig_ha))]),
      Irrigation_m_eff = Irrigation_m
    )
  
  perha_rain <- perha %>%
    dplyr::mutate(
      area_m2 = as.numeric(area_rain_m2[match(Crop, names(area_rain_m2))]),
      area_ha = as.numeric(area_rain_ha[match(Crop, names(area_rain_ha))]),
      Irrigation_m_eff = 0
    )
  
  perha2 <- dplyr::bind_rows(
    perha_irrig %>% dplyr::mutate(Management = "irrigated"),
    perha_rain  %>% dplyr::mutate(Management = "rainfed")
  )
  
  # ---- 5) per-crop totals using per-kg econ ------------------------
  perha2 <- perha2 %>%
    dplyr::mutate(
      revenue_total   = income_per_kg     * Yield_kgHa * area_ha,
      base_cost_total = total_cost_per_kg * Yield_kgHa * area_ha,
      irr_cost_total  = (Irrigation_m_eff * area_m2) *
        (universal_costs$irr_cost_per_m3 %||% 0)
    )
  
  Fert_kg_total <- sum(perha2$fert_kgha * perha2$area_ha,         na.rm = TRUE)
  Irr_m3_total  <- sum(perha2$Irrigation_m_eff * perha2$area_m2,  na.rm = TRUE)
  
  Revenue_total  <- sum(perha2$revenue_total,   na.rm = TRUE)
  BaseCost_total <- sum(perha2$base_cost_total, na.rm = TRUE)
  Irr_totalCost  <- sum(perha2$irr_cost_total,  na.rm = TRUE)
  
  NR_total <- Revenue_total - (BaseCost_total + Irr_totalCost)
  
  # ---- 6) WQ prediction row ----------------------------------------
  pred_row <- tibble::tibble(
    Year = Y,
    `LandCover_m2_all cult`      = Cult_m2,
    LandCover_m2_grassPastureHay = Grass_m2,
    LandCover_m2_developed       = LC_dev_base
  ) %>%
    dplyr::left_join(
      wq_context %>%
        dplyr::select(Year, Climate_precip_m, Climate_Tmean_C, precip_percentileChange) %>%
        dplyr::distinct(Year, .keep_all = TRUE),
      by = "Year"
    ) %>%
    dplyr::mutate(
      Management_irrigation_m       = ifelse(Cult_m2 > 0, Irr_m3_total  / Cult_m2, 0),
      Management_FertilizerUse_kgm2 = ifelse(Cult_m2 > 0, Fert_kg_total / Cult_m2, 0)
    )
  
  if (anyNA(pred_row)) return(c(PEN, PEN, PEN))
  
  pred_log1p   <- as.numeric(stats::predict(wq_model_log1p, newdata = pred_row))
  nitrate_kgyr <- max(0, exp(pred_log1p) - 1)
  
  c(nitrate_kgyr, -NR_total, Irr_m3_total)
}

