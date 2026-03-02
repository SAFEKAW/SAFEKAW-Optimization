# hpc_opt/R/33_eval_engine.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

eval_candidate_year <- function(
    x, Y,
    df_opt,
    wq_model_mlr,
    wq_base_by_year,
    lu_baseline,
    universal_costs,
    crop_params,
    baseline_irrig_frac,
    fert_ref_by_year,
    policy = NULL,
    hist_mix = NULL
) {
  PEN <- 1e12
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  x <- unname(x)
  corn <- x[1]; soy <- x[2]; sor <- x[3]
  wheat <- 1 - corn - soy - sor
  
  if (any(c(corn, soy, sor, wheat) < 0) || any(c(corn, soy, sor, wheat) > 1)) {
    return(c(PEN, PEN, PEN))
  }
  
  shares <- c(Corn = corn, Soybeans = soy, Sorghum = sor, Wheat = wheat)
  
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
  
  # ---- per-ha rows (must exist in df_opt) ----
  crops4 <- c("Corn","Soybeans","Sorghum","Wheat")
  
  perha <- df_opt %>%
    filter(Year == Y, Crop %in% crops4) %>%
    complete(Year = Y, Crop = crops4, fill = list(Yield_kgHa = 0, Irrigation_m = 0)) %>%
    select(-any_of(names(crop_params)[names(crop_params) != "Crop"])) %>%
    left_join(crop_params, by = "Crop")
  
  # ---- policy irrigated fraction ----
  irrig_frac_factor <- policy$irrig_frac_factor %||% 1
  theta_irrig_policy <- baseline_irrig_frac * irrig_frac_factor
  theta_irrig_policy <- max(0, min(1, theta_irrig_policy))
  
  # ---- irrigation efficiency ----
  irr_eff <- policy$irr_eff %||% 1
  irr_eff <- max(1, irr_eff)
  
  # ---- allocate irrigated vs rainfed area by crop ----
  alloc <- allocate_irrigation(
    crop_shares = shares,
    theta_irrig = theta_irrig_policy,
    hist_mix    = hist_mix,
    mode        = "preserve_hist_mix"
  )
  
  area_irrig_m2 <- Cult_m2 * alloc$irrig
  area_rain_m2  <- Cult_m2 * alloc$rainfed
  area_irrig_ha <- area_irrig_m2 / 1e4
  area_rain_ha  <- area_rain_m2  / 1e4
  
  perha_irrig <- perha %>%
    mutate(
      area_m2 = as.numeric(area_irrig_m2[match(Crop, names(area_irrig_m2))]),
      area_ha = as.numeric(area_irrig_ha[match(Crop, names(area_irrig_ha))]),
      Irrigation_m_eff = Irrigation_m / irr_eff
    )
  
  perha_rain <- perha %>%
    mutate(
      area_m2 = as.numeric(area_rain_m2[match(Crop, names(area_rain_m2))]),
      area_ha = as.numeric(area_rain_ha[match(Crop, names(area_rain_ha))]),
      Irrigation_m_eff = 0
    )
  
  perha2 <- bind_rows(
    perha_irrig %>% mutate(Management = "irrigated"),
    perha_rain  %>% mutate(Management = "rainfed")
  )
  
  # ---- economics (fert affects only fert share of direct costs) ----
  fert_factor <- policy$fert_factor %||% 1
  fert_share_direct <- universal_costs$fert_share_direct %||% 0.2
  
  perha2 <- perha2 %>%
    mutate(
      direct_cost_per_kg_eff =
        direct_cost_per_kg * (1 - fert_share_direct + fert_share_direct * fert_factor),
      total_cost_per_kg_eff =
        direct_cost_per_kg_eff + fixed_cost_per_kg,
      
      revenue_total   = income_per_kg * Yield_kgHa * area_ha,
      base_cost_total = total_cost_per_kg_eff * Yield_kgHa * area_ha,
      irr_cost_total  = (Irrigation_m_eff * area_m2) * (universal_costs$irr_cost_per_m3 %||% 0)
    )
  
  Fert_kg_total <- sum(perha2$fert_kgha * perha2$area_ha,        na.rm = TRUE)
  Irr_m3_total  <- sum(perha2$Irrigation_m_eff * perha2$area_m2, na.rm = TRUE)
  
  Revenue_total  <- sum(perha2$revenue_total,   na.rm = TRUE)
  BaseCost_total <- sum(perha2$base_cost_total, na.rm = TRUE)
  Irr_totalCost  <- sum(perha2$irr_cost_total,  na.rm = TRUE)
  
  NR_total <- Revenue_total - (BaseCost_total + Irr_totalCost)
  
  # ---- WQ baseline prediction (no fert in model) ----
  BasinArea_m2 <- Cult_m2 + Grass_m2 + LC_dev_base
  
  pred_row <- tibble(
    Year = Y,
    BasinArea_m2 = BasinArea_m2,
    dev_frac  = ifelse(BasinArea_m2 > 0, LC_dev_base / BasinArea_m2, NA_real_),
    cult_frac = ifelse(BasinArea_m2 > 0, Cult_m2      / BasinArea_m2, NA_real_)
  ) %>%
    left_join(
      wq_base_by_year %>%
        select(Year, Climate_precip_m, Climate_Tmean_C, precip_percentileChange) %>%
        distinct(Year, .keep_all = TRUE),
      by = "Year"
    )
  
  if (anyNA(pred_row)) return(c(PEN, PEN, PEN))
  
  N0 <- pmax(0, as.numeric(predict(wq_model_mlr, newdata = pred_row)))
  
  # ---- Fert multiplier ----
  gamma <- policy$fert_gamma %||% 0.1
  Fert_policy_kg <- Fert_kg_total * fert_factor
  
  Fert_ref_kg <- fert_ref_by_year$Fert_ref_kg[fert_ref_by_year$Year == Y]
  if (length(Fert_ref_kg) != 1L || is.na(Fert_ref_kg) || Fert_ref_kg <= 0) return(c(PEN, PEN, PEN))
  
  mult <- fert_multiplier(Fert_policy_kg, Fert_ref_kg, gamma = gamma)
  nitrate_kgyr <- N0 * mult
  
  c(nitrate_kgyr, -NR_total, Irr_m3_total)
}

eval_candidate_all_years <- function(
    x, years_vec,
    df_opt,
    wq_model_mlr, wq_base_by_year,
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
      df_opt = df_opt,
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
    mean_n_flux_kgyr      = wmean(vals[, "n_flux_kgyr"],      weights),
    mean_neg_profit_usdyr = wmean(vals[, "neg_profit_usdyr"], weights),
    mean_irr_m3_yr        = wmean(vals[, "irr_m3_yr"],        weights)
  )
  
  if (any(!is.finite(out))) out[] <- na_penalty
  out
}