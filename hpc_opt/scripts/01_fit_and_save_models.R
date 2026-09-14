#01_fit_and_save_models
suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lme4)
  library(broom)
  library(broom.mixed)
  library(performance)
})

source(here("hpc_opt","R","10_helpers.R"))
source(here("hpc_opt","R","12_irrigation_predict.R"))
source(here("hpc_opt","R","13_yield_predict.R"))
source(here("hpc_opt","R","14_wq_features.R"))
source(here("hpc_opt","R","15_wq_predict.R"))

yrs_common <- 2006:2023

dir.create(here("hpc_opt","models"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("hpc_opt","outputs","model_checks"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("hpc_opt","outputs","model_checks","figures"), recursive = TRUE, showWarnings = FALSE)

# Avoid aborting a refit when Excel or OneDrive temporarily locks a diagnostic
# CSV. Prefer the canonical path and otherwise write a dated sibling.
write_model_check_csv <- function(x, path) {
  tryCatch(
    readr::write_csv(x, path),
    error = function(e) {
      fallback <- sub("\\.csv$", "_refresh_20260807.csv", path)
      warning("Diagnostic CSV locked; writing updated results to: ", fallback)
      readr::write_csv(x, fallback)
    }
  )
}

# -----------------------------
# load historical common inputs
# -----------------------------
df_combined_county <- read_csv(
  here("hpc_opt", "outputs", "common_inputs_county_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% yrs_common) %>%
  mutate(FIPS = as.character(FIPS))

common_input_basin <- read_csv(
  here("hpc_opt", "outputs", "common_inputs_basin_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% yrs_common)

if ("LandCover_m2_all cult" %in% names(common_input_basin) &&
    !("LandCover_m2_all_cult" %in% names(common_input_basin))) {
  common_input_basin <- common_input_basin %>%
    rename(LandCover_m2_all_cult = `LandCover_m2_all cult`)
}

df_yield_obs <- read_csv(
  here("data","CropYieldData_County.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% yrs_common) %>%
  mutate(FIPS = as.character(FIPS))

df_nitrate_obs <- read_csv(
  here("data","RiverNitrateData_EKSRB.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% yrs_common)

df_crop_climate <- read_csv(
  here("data","crop_climate_gs_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    FIPS = as.character(FIPS),
    Crop = recode(crop,
                  "Corn"     = "Corn",
                  "Soybeans" = "Soybeans",
                  "Sorghum"  = "Sorghum",
                  "Wheat"    = "Wheat"
    ),
    precip_gs_m = precip_gs_mm / 1000
  ) %>%
  select(FIPS, Year, Crop, GDD, precip_gs_m)

df_wateruse_by_crop <- read_csv(
  here("data", "WaterUseByCrop_EKSRB.csv"),
  show_col_types = FALSE
)

df_combined_county2 <- split_historical_water_management(
  df_county = df_combined_county %>% mutate(FIPS = as.character(FIPS)),
  wateruse_by_crop = df_wateruse_by_crop,
  irrigated_crops = c("Corn", "Soybeans")
)

# -----------------------------
# 1. Irrigation model
# -----------------------------
irr_train <- df_combined_county %>%
  filter(Crop %in% c("Corn", "Soybeans")) %>%
  filter(!is.na(irrigation_WaterUse_m), is.finite(precip_m), is.finite(tmax_C_avg))

irr_lm <- lm(
  irrigation_WaterUse_m ~ precip_m + tmax_C_avg,
  data = irr_train
)

irr_train <- irr_train %>%
  mutate(
    irr_pred_m = predict(irr_lm, newdata = .),
    resid_m = irrigation_WaterUse_m - irr_pred_m
  )

irr_metrics <- tibble(
  RMSE = rmse(irr_train$irr_pred_m, irr_train$irrigation_WaterUse_m),
  MAE  = mae(irr_train$irr_pred_m, irr_train$irrigation_WaterUse_m),
  R2   = cor(irr_train$irr_pred_m, irr_train$irrigation_WaterUse_m, use = "complete.obs")^2
)

saveRDS(irr_lm, here("hpc_opt","models","irr_lm.rds"))
write_model_check_csv(irr_metrics, here("hpc_opt","outputs","model_checks","irrigation_metrics.csv"))

r2_df_irr <- round(irr_metrics$R2, 2)


p_irr_obs_pred <- ggplot(irr_train, aes(x = irrigation_WaterUse_m, y = irr_pred_m, color = Crop)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  scale_color_manual( values =c("#EEC900", "#548B54")) +
  geom_point(alpha = 0.6) +
  labs(x = "Observed irrigation depth (m)",
       y = "Predicted irrigation depth (m)",
       title = "Observed vs Predicted Irrigation") +
  geom_text(data = as.data.frame(r2_df_irr), aes(x = Inf, y = Inf, label =  paste0("R² = ",r2_df_irr)),
            inherit.aes = FALSE, hjust = 4.5, vjust = 2.5, size = 5) +
  theme_test(base_size = 15) + theme(legend.position = "none")

p_irr_obs_pred #slightly worse R2..? why??

ggsave(
  here("hpc_opt","outputs","model_checks","figures","irrigation_obs_pred.png"),
  p_irr_obs_pred, width = 6, height = 5, dpi = 300)

p_irr_resid <- ggplot(irr_train, aes(x = irr_pred_m, y = resid_m, color = Crop)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual( values =c("#EEC900", "#548B54")) +
  labs(x = "Year", y = "Residual (obs - pred)",
       title = "Irrigation residuals vs prediction") +
  theme_test(base_size = 15) + theme(legend.position = "bottom")

p_irr_resid

ggsave(
  here("hpc_opt","outputs","model_checks","figures","irrigation_residuals.png"),
  p_irr_resid, width = 6, height = 5, dpi = 300)

# -----------------------------
# 2. Build historical crop table
# -----------------------------
irr_aug <- predict_irrigation_depth(
  df_all = df_combined_county2,
  irrigation_lm = irr_lm,
  irrigated_crops = c("Corn","Soybeans"),
  require_irrigated_mgmt = TRUE
)

df_crop <- irr_aug %>%
  filter(!is.na(Crop), Year %in% yrs_common) %>%
  mutate(
    FIPS = as.character(FIPS),
    irrigation_WaterUse_m = if_else(
      WaterManagement == "Non-Irrigated",
      0,
      coalesce(irrigation_WaterUse_m, 0)
    )
  ) %>%
  left_join(
    df_yield_obs %>% mutate(FIPS = as.character(FIPS)),
    by = c("Year", "FIPS", "Crop", "WaterManagement")
  ) %>%
  left_join(
    df_crop_climate,
    by = c("Year", "FIPS", "Crop")
  ) %>%
  mutate(
    GDD = coalesce(GDD.y, GDD.x)
  ) %>%
  mutate(
    totalWater_m = precip_gs_m + irrigation_WaterUse_m
  ) %>%
  filter(
    is.finite(totalWater_m),
    is.finite(GDD)
  )


if ("area_ha" %in% names(df_crop)) {
  area_ha_thres <- 64.75 * 10
  df_crop <- df_crop %>%
    filter(coalesce(crop_area_ha_unsplit, area_ha) > area_ha_thres)
}

# -----------------------------
# 3. Yield model comparison
# -----------------------------
calc_metrics <- function(obs, pred) {
  tibble(
    RMSE = rmse(pred, obs),
    MAE  = mae(pred, obs),
    R2   = cor(obs, pred, use = "complete.obs")^2
  )
}

fit_compare_one <- function(dat, response) {
  dat <- dat %>% filter(is.finite(.data[[response]]))
  
  f_lm <- as.formula(
    paste0(response, " ~ poly(totalWater_m, 2, raw = TRUE) + GDD") #+ tmin_C_avg + tmax_C_avg + GDD
  )
  
  f_lmer <- as.formula(
    paste0(response, " ~ poly(totalWater_m, 2, raw = TRUE) + GDD + (1|FIPS) ") #+ (1|Year)
  )
  
  m_lm <- lm(f_lm, data = dat)
  m_lmer <- lmer(f_lmer, data = dat, REML = FALSE)
  
  dat_pred <- dat %>%
    mutate(
      pred_lm         = predict(m_lm, newdata = dat),
      pred_lmer_full  = predict(m_lmer, newdata = dat, re.form = NULL),
      pred_lmer_fixed = predict(m_lmer, newdata = dat, re.form = NA)
    )
  
  metrics <- bind_rows(
    calc_metrics(dat_pred[[response]], dat_pred$pred_lm) %>% mutate(model = "lm"),
    calc_metrics(dat_pred[[response]], dat_pred$pred_lmer_full) %>% mutate(model = "lmer_full"),
    calc_metrics(dat_pred[[response]], dat_pred$pred_lmer_fixed) %>% mutate(model = "lmer_fixed")
  )
  
  list(
    lm_model = m_lm,
    lmer_model = m_lmer,
    pred = dat_pred,
    metrics = metrics
  )
}

responses <- c("yield_kgHa_detrended", "yield_kcalHa_detrended")
crops <- sort(unique(df_crop$Crop))
results <- list()

for (cr in crops) {
  dat_cr <- df_crop %>% filter(Crop == cr)
  for (resp in responses) {
    key <- paste(cr, resp, sep = "__")
    results[[key]] <- fit_compare_one(dat_cr, resp)
  }
}

metrics_all <- bind_rows(
  lapply(names(results), function(nm) {
    parts <- strsplit(nm, "__")[[1]]
    results[[nm]]$metrics %>%
      mutate(Crop = parts[1], Response = parts[2])
  })
)

write_model_check_csv(
  metrics_all,
  here("hpc_opt","outputs","model_checks","yield_model_comparison.csv")
)

# coefficient comparison
coef_compare <- bind_rows(
  lapply(names(results), function(nm) {
    parts <- strsplit(nm, "__")[[1]]
    crop <- parts[1]
    response <- parts[2]
    
    lm_tab <- broom::tidy(results[[nm]]$lm_model) %>%
      transmute(Crop = crop, Response = response, model = "lm", term, estimate, std.error)
    
    lmer_tab <- broom.mixed::tidy(results[[nm]]$lmer_model, effects = "fixed") %>%
      transmute(Crop = crop, Response = response, model = "lmer", term, estimate, std.error)
    
    bind_rows(lm_tab, lmer_tab)
  })
)

coef_compare_wide <- coef_compare %>%
  pivot_wider(
    names_from = model,
    values_from = c(estimate, std.error)
  ) %>%
  mutate(
    estimate_diff = estimate_lmer - estimate_lm,
    abs_estimate_diff = abs(estimate_diff)
  )

write_model_check_csv(
  coef_compare_wide,
  here("hpc_opt","outputs","model_checks","yield_model_coefficients_wide.csv")
)

# -----------------------------
# 4. Fit final yield models
# -----------------------------
yield_kg_models <- list()
yield_kcal_models <- list()

for (cr in crops) {
  dat_cr <- df_crop %>%
    filter(Crop == cr) %>%
    filter(
      is.finite(yield_kgHa_detrended),
      is.finite(yield_kcalHa_detrended),
      is.finite(totalWater_m),
      is.finite(precip_gs_m),
      is.finite(irrigation_WaterUse_m),
      is.finite(GDD)
    )
  
  if (cr == "Wheat") {
    wheat_scaling <- list(
      water_center = mean(dat_cr$totalWater_m),
      water_scale = sd(dat_cr$totalWater_m),
      gdd_center = mean(dat_cr$GDD),
      gdd_scale = sd(dat_cr$GDD),
      year_center = 2005
    )

    if (!is.finite(wheat_scaling$water_scale) || wheat_scaling$water_scale <= 0 ||
        !is.finite(wheat_scaling$gdd_scale) || wheat_scaling$gdd_scale <= 0) {
      stop("Cannot fit wheat model: predictor standard deviations must be positive.")
    }

    dat_cr <- dat_cr %>%
      mutate(
        ZTotalWater = (totalWater_m - wheat_scaling$water_center) /
          wheat_scaling$water_scale,
        ZGDD = (GDD - wheat_scaling$gdd_center) / wheat_scaling$gdd_scale,
        Year_centered = Year - wheat_scaling$year_center
      )

    wheat_formula_kg <- yield_kgHa_detrended ~
      ZTotalWater + I(ZTotalWater^2) + I(ZTotalWater^3) +
      ZGDD + ZTotalWater:ZGDD +
      (1 | FIPS)

    wheat_formula_kcal <- update(
      wheat_formula_kg,
      yield_kcalHa_detrended ~ .
    )

    m_kg <- lmer(wheat_formula_kg, data = dat_cr, REML = TRUE)
    m_kcal <- lmer(wheat_formula_kcal, data = dat_cr, REML = TRUE)

    attr(m_kg, "yield_scaling") <- wheat_scaling
    attr(m_kcal, "yield_scaling") <- wheat_scaling
  }
  
  else if (cr %in% c("Corn", "Soybeans")) {
    # Fit one crop-specific response across both management classes. Rainfall
    # and irrigation enter separately because the diagnostic audit showed that
    # treating them as interchangeable total water obscures the yield response.
    # Rainfed observations have irrigation_WaterUse_m == 0.
    m_kg <- lmer(
      yield_kgHa_detrended ~ poly(precip_gs_m, 2, raw = TRUE) +
        irrigation_WaterUse_m + GDD + (1 | FIPS),
      data = dat_cr,
      REML = TRUE
    )

    m_kcal <- lmer(
      yield_kcalHa_detrended ~ poly(precip_gs_m, 2, raw = TRUE) +
        irrigation_WaterUse_m + GDD + (1 | FIPS),
      data = dat_cr,
      REML = TRUE
    )
  }

  else {
    m_kg <- lmer(
      yield_kgHa_detrended ~ poly(totalWater_m, 2, raw = TRUE) + GDD + (1 | FIPS),
      data = dat_cr,
      REML = TRUE
    )
    
    m_kcal <- lmer(
      yield_kcalHa_detrended ~ poly(totalWater_m, 2, raw = TRUE) + GDD + (1 | FIPS),
      data = dat_cr,
      REML = TRUE
    )
  }
 
  
  yield_kg_models[[cr]] <- m_kg
  yield_kcal_models[[cr]] <- m_kcal
  
  saveRDS(m_kg, here("hpc_opt","models", paste0("yield_kg_", cr, ".rds")))
  saveRDS(m_kcal, here("hpc_opt","models", paste0("yield_kcal_", cr, ".rds")))
}

# Wheat extrapolation diagnostic. Hold GDD at its historical median and total
# water at representative historical dry/median/wet conditions, then confirm
# that the climate-only production model has no artificial calendar-year trend.
wheat_train <- df_crop %>%
  filter(
    Crop == "Wheat",
    is.finite(totalWater_m),
    is.finite(GDD)
  )

wheat_water_levels <- setNames(
  as.numeric(
    quantile(
      wheat_train$totalWater_m,
      probs = c(0.10, 0.50, 0.90),
      na.rm = TRUE
    )
  ),
  c("dry", "median", "wet")
)

wheat_projection_diagnostic <- tidyr::crossing(
  Year = seq(min(yrs_common), 2100L),
  water_condition = names(wheat_water_levels)
) %>%
  mutate(
    totalWater_m = unname(wheat_water_levels[water_condition]),
    GDD = median(wheat_train$GDD, na.rm = TRUE),
    FIPS = as.character(wheat_train$FIPS[[1]]),
    Crop = "Wheat"
  )

wheat_projection_diagnostic$yield_kgHa_pred_fixed <-
  predict_one_yield_model(
    yield_kg_models[["Wheat"]],
    newdata = wheat_projection_diagnostic,
    fixed_only = TRUE
  )

write_model_check_csv(
  wheat_projection_diagnostic,
  here(
    "hpc_opt", "outputs", "model_checks",
    "wheat_yield_year_extrapolation.csv"
  )
)

p_wheat_year_extrapolation <- ggplot(
  wheat_projection_diagnostic,
  aes(
    x = Year,
    y = yield_kgHa_pred_fixed,
    color = water_condition
  )
) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_vline(
    xintercept = max(yrs_common),
    linetype = "dashed",
    color = "grey40"
  ) +
  scale_color_brewer(
    palette = "Dark2",
    breaks = c("dry", "median", "wet")
  ) +
  labs(
    x = "Year",
    y = "Predicted detrended wheat yield (kg/ha)",
    color = "Total-water condition",
    title = "Climate-only wheat yield extrapolation check",
    subtitle = paste0(
      "GDD fixed at historical median; water fixed at historical ",
      "10th, 50th, and 90th percentiles"
    )
  ) +
  theme_bw()

ggsave(
  here(
    "hpc_opt", "outputs", "model_checks", "figures",
    "wheat_yield_year_extrapolation.png"
  ),
  p_wheat_year_extrapolation,
  width = 8,
  height = 5,
  dpi = 300
)

# Diagnostics from the final saved model specifications.
required_prediction_fields <- c(
  "precip_gs_m", "irrigation_WaterUse_m", "totalWater_m", "GDD"
)
missing_prediction_fields <- setdiff(required_prediction_fields, names(df_crop))
if (length(missing_prediction_fields) > 0) {
  stop(
    "Yield diagnostic data are missing: ",
    paste(missing_prediction_fields, collapse = ", ")
  )
}
df_crop_pred <- predict_yields(
    df = df_crop,
    yield_kg_models = yield_kg_models,
    yield_kcal_models = yield_kcal_models,
    fixed_only = FALSE
  )

#lapply(yield_kg_models, lme4::isSingular, tol = 1e-5)
#lapply(yield_kg_models, lme4::VarCorr)

final_yield_metrics <- df_crop_pred %>%
  group_by(Crop) %>%
  summarise(
    n = sum(complete.cases(yield_kgHa_detrended, yield_kgHa_pred)),
    RMSE = rmse(yield_kgHa_pred, yield_kgHa_detrended),
    MAE = mae(yield_kgHa_pred, yield_kgHa_detrended),
    R2 = cor(
      yield_kgHa_detrended,
      yield_kgHa_pred,
      use = "complete.obs"
    )^2,
    .groups = "drop"
  )

write_model_check_csv(
  final_yield_metrics,
  here("hpc_opt", "outputs", "model_checks", "yield_final_model_metrics.csv")
)

r2_df <- final_yield_metrics %>%
  mutate(label = paste0("R2 = ", round(R2, 2)))


p_yield_obs_pred <- ggplot(
  df_crop_pred,
  aes(x = yield_kgHa_pred, y = yield_kgHa_detrended, color = WaterManagement)
) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Crop, scales = "free") +
  scale_color_manual(values = c(
    "Irrigated" = "lightblue", "Non-Irrigated" = "orange"
  )) +
  labs(x = "Predicted Yield (kg/ha)", y = "Observed Yield (kg/ha)",
       title = "Predicted vs Observed Yield") +
  geom_text(data = r2_df, aes(x = Inf, y = Inf, label = label),
            inherit.aes = FALSE, hjust = 1.15, vjust = 1.2, size = 3.5) +
  theme_test(base_size = 14) + 
  theme(legend.position = "none")

p_yield_obs_pred

ggsave(
  here("hpc_opt","outputs","model_checks","figures","yield_obs_pred_kg.png"),
  p_yield_obs_pred, width = 8, height = 6, dpi = 300
)

p_yield_resid <- df_crop_pred %>%
  mutate(resid_kg = yield_kgHa_detrended - yield_kgHa_pred) %>%
  ggplot(aes(x = totalWater_m, y = resid_kg, color = WaterManagement)) + #FIPS
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c(
    "Irrigated" = "lightblue", "Non-Irrigated" = "orange"
  )) +
  facet_wrap(~ Crop, scales = "free") +
  labs(x = "Total growing-season water (m)",
       y = "Residual: observed - predicted (kg/ha)",
       title = "Residuals vs Total Water") +
  #geom_line(aes(y = yield_kgHa_detrended_fit), color = "grey40", linewidth = 0.7, alpha = 0.6) +
  theme_test(base_size = 12) + theme(legend.position = "bottom")
p_yield_resid

ggsave(
  here("hpc_opt","outputs","model_checks","figures","yield_residuals_kg.png"),
  p_yield_resid, width = 8, height = 6, dpi = 300
)

p_wp <- ggplot(df_crop_pred, aes(x = totalWater_m, y = yield_kgHa_detrended)) +
  geom_point(aes(color = WaterManagement), alpha = 0.6) +
  facet_wrap(~ Crop, scales = "free_y") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", se = FALSE) +
  labs(
    x = "Precipitation + Irrigation (m)",
    y = "Observed yield (kg/ha)",
    title = "Water–productivity relationships by crop"
  ) +
  theme_test(base_size = 12) +
  theme(legend.position = "bottom")

p_wp


#coeff for write up
fit_std_one <- function(dat, response, use_gdd = TRUE) {
  dat2 <- dat %>%
    filter(is.finite(.data[[response]]), is.finite(totalWater_m)) %>%
    mutate(
      z_y = as.numeric(scale(.data[[response]])),
      z_water = as.numeric(scale(totalWater_m))
    )
  
  if (use_gdd) {
    dat2 <- dat2 %>% mutate(z_gdd = as.numeric(scale(GDD)))
    
    m_std <- lm(
      z_y ~ poly(z_water, 2, raw = TRUE) + z_gdd,
      data = dat2
    )
  } else {
    dat2 <- dat2 %>%
      mutate(
        z_tmin = as.numeric(scale(tmin_C_avg)),
        z_tmax = as.numeric(scale(tmax_C_avg))
      )
    
    m_std <- lm(
      z_y ~ poly(z_water, 2, raw = TRUE) + z_tmin + z_tmax,
      data = dat2
    )
  }
  
  broom::tidy(m_std)
}

std_coef_all <- bind_rows(
  lapply(sort(unique(df_crop$Crop)), function(cr) {
    fit_std_one(
      dat = df_crop %>% filter(Crop == cr),
      response = "yield_kgHa_detrended",
      use_gdd = TRUE
    ) %>%
      mutate(Crop = cr, Response = "yield_kgHa_detrended")
  })
)

std_coef_all


coef_yield_all <- bind_rows(
  lapply(sort(unique(df_crop$Crop)), function(cr) {
    dat_cr <- df_crop %>% filter(Crop == cr)
    
    m <- lm(
      yield_kgHa_detrended ~ poly(totalWater_m, 2, raw = TRUE) + GDD,
      data = dat_cr
    )
    
    broom::tidy(m) %>%
      mutate(Crop = cr)
  })
)

coef_yield_all

water_effect_df <- df_crop %>%
  group_by(Crop) %>%
  summarise(
    water_min = min(totalWater_m, na.rm = TRUE),
    water_max = max(totalWater_m, na.rm = TRUE),
    GDD = mean(GDD, na.rm = TRUE),
    Year = as.integer(round(mean(Year, na.rm = TRUE))),
    FIPS = as.character(first(FIPS)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  do({
    tibble(
      Crop = .$Crop,
      totalWater_m = seq(.$water_min, .$water_max, length.out = 100),
      precip_gs_m = seq(.$water_min, .$water_max, length.out = 100),
      irrigation_WaterUse_m = 0,
      GDD = .$GDD,
      Year = .$Year,
      FIPS = .$FIPS
    )
  }) %>%
  ungroup()

water_effect_pred <- water_effect_df %>%
  group_split(Crop) %>%
  lapply(function(dat) {
    cr <- unique(dat$Crop)
    mod <- yield_kg_models[[cr]]
    
    dat$pred_yield <- predict_one_yield_model(mod, newdata = dat, fixed_only = TRUE)
    dat
  }) %>%
  bind_rows()

# -----------------------------
# 5. Water quality model
# -----------------------------
wq_train <- build_wq_input_from_basin(
  common_input_basin = common_input_basin,
  irr_aug = irr_aug,
  use_modeled_irr = TRUE
) %>%
  left_join(df_nitrate_obs %>% select(Year, NitrateFlux_kg), by = "Year") %>%
  filter(
    is.finite(NitrateFlux_kg),
    is.finite(cult_frac),
    is.finite(Climate_precip_m),
    is.finite(Climate_Tmean_C),
    is.finite(precip_percentileChange)
  )

wq_lm <- fit_wq_lm(wq_train)

wq_pred <- predict_wq_flux(wq_train, wq_lm) %>%
  mutate(resid_kg = NitrateFlux_kg - NitrateFluxPredicted_kg)

wq_metrics <- tibble(
  RMSE = rmse(wq_pred$NitrateFluxPredicted_kg, wq_pred$NitrateFlux_kg),
  MAE  = mae(wq_pred$NitrateFluxPredicted_kg, wq_pred$NitrateFlux_kg),
  R2   = cor(wq_pred$NitrateFluxPredicted_kg, wq_pred$NitrateFlux_kg, use = "complete.obs")^2
)

r2_df_wq <- round(wq_metrics$R2, 2)


saveRDS(wq_lm, here("hpc_opt","models","wq_lm.rds"))
write_model_check_csv(wq_metrics, here("hpc_opt","outputs","model_checks","wq_metrics.csv"))


p_wq_obs_pred <- ggplot(wq_pred, aes(x = NitrateFlux_kg, y = NitrateFluxPredicted_kg)) +
  geom_point(alpha = 0.7, color = "steelblue", size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Predicted flux (N kg/yr)", y = "Observed flux (N kg/yr)",
       title = "Predicted vs Observed N flux") +
  geom_text(data = as.data.frame(r2_df_wq), aes(x = Inf, y = Inf, label =  paste0("R² = ",r2_df_wq)),            inherit.aes = FALSE, hjust = 4.1, vjust = 1.2, size = 3.5) +
  theme_test(base_size = 14) + 
  theme(legend.position = "none")

p_wq_obs_pred

ggsave(
  here("hpc_opt","outputs","model_checks","figures","wq_obs_pred.png"),
  p_wq_obs_pred, width = 6, height = 5, dpi = 300
)

p_wq_resid <- ggplot(wq_pred, aes(x = Climate_precip_m, y = resid_kg)) +
  geom_point(alpha = 0.7, color = "steelblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Climate_precip_m", y = "Residual (obs - pred)",
       title = "Residuals vs prediction") +
  theme_test(base_size = 14)

p_wq_resid

ggsave(
  here("hpc_opt","outputs","model_checks","figures","wq_residuals.png"),
  p_wq_resid, width = 6, height = 5, dpi = 300
)

coef_wq <- broom::tidy(wq_lm)
coef_wq

coef_wq %>%
  mutate(
    term = recode(term,
                  "cult_frac" = "Cultivated fraction",
                  "Climate_precip_m" = "Precipitation",
                  "Climate_Tmean_C" = "Mean temperature",
                  "precip_percentileChange" = "Precipitation anomaly"
    )
  )


