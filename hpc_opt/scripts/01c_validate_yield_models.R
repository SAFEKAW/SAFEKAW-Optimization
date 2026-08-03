# 01c_validate_yield_models.R
#
# Forward year-block validation for the current crop-yield models, plus a
# targeted comparison of the wheat model with and without the
# ZTotalWater:Year_centered interaction.

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(lme4)
})

source(here("hpc_opt", "R", "10_helpers.R"))
source(here("hpc_opt", "R", "12_irrigation_predict.R"))
source(here("hpc_opt", "R", "13_yield_predict.R"))
source(here("hpc_opt", "R", "11_models_load.R"))

years_hist <- 2006:2023
out_dir <- here("hpc_opt", "outputs", "model_checks")
fig_dir <- here("hpc_opt", "outputs", "model_checks", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# Reconstruct the same historical yield-modeling table used for production.
# -------------------------------------------------------------------------

county_hist <- read_csv(
  here("hpc_opt", "outputs", "common_inputs_county_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% years_hist) %>%
  mutate(
    FIPS = as.character(FIPS),
    is_irrig_hist = build_irrig_flags(pick(everything())),
    WaterManagement = if_else(
      is_irrig_hist,
      "Irrigated",
      "Non-Irrigated"
    )
  )

yield_obs <- read_csv(
  here("data", "CropYieldData_County.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% years_hist) %>%
  mutate(FIPS = as.character(FIPS))

crop_climate <- read_csv(
  here("data", "crop_climate_gs_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    FIPS = as.character(FIPS),
    Year,
    Crop = crop,
    GDD,
    precip_gs_m = precip_gs_mm / 1000
  )

irrigation_model <- readRDS(here("hpc_opt", "models", "irr_lm.rds"))

yield_data <- predict_irrigation_depth(
  df_all = county_hist,
  irrigation_lm = irrigation_model,
  irrigated_crops = c("Corn", "Soybeans"),
  require_irrigated_mgmt = TRUE
) %>%
  filter(!is.na(Crop), Year %in% years_hist) %>%
  mutate(
    FIPS = as.character(FIPS),
    irrigation_WaterUse_m = if_else(
      WaterManagement == "Non-Irrigated",
      0,
      coalesce(irrigation_WaterUse_m, 0)
    )
  ) %>%
  left_join(
    yield_obs,
    by = c("Year", "FIPS", "Crop", "WaterManagement")
  ) %>%
  left_join(
    crop_climate,
    by = c("Year", "FIPS", "Crop"),
    suffix = c("", "_climate")
  ) %>%
  mutate(
    GDD = coalesce(GDD_climate, GDD),
    totalWater_m = precip_gs_m + irrigation_WaterUse_m
  ) %>%
  filter(
    is.finite(yield_kgHa_detrended),
    is.finite(totalWater_m),
    is.finite(GDD)
  )

if ("area_ha" %in% names(yield_data)) {
  yield_data <- yield_data %>% filter(area_ha > 64.75 * 10)
}

# Forward-chaining blocks preserve the direction of prediction: each
# validation block is later than every year used to fit it.
validation_blocks <- tribble(
  ~fold, ~train_end, ~test_start, ~test_end,
  "2015-2017", 2014L, 2015L, 2017L,
  "2018-2020", 2017L, 2018L, 2020L,
  "2021-2023", 2020L, 2021L, 2023L
)

current_formula <- yield_kgHa_detrended ~
  poly(totalWater_m, 2, raw = TRUE) + GDD + (1 | FIPS)

wheat_formula_climate_only <- yield_kgHa_detrended ~
  ZTotalWater + I(ZTotalWater^2) + I(ZTotalWater^3) +
  ZGDD +
  ZTotalWater:ZGDD +
  (1 | FIPS)

wheat_formula_no_water_year <- update(
  wheat_formula_climate_only,
  . ~ . + Year_centered
)

wheat_formula_with_water_year <- update(
  wheat_formula_no_water_year,
  . ~ . + ZTotalWater:Year_centered
)

make_scaling <- function(dat) {
  scaling <- list(
    water_center = mean(dat$totalWater_m),
    water_scale = sd(dat$totalWater_m),
    gdd_center = mean(dat$GDD),
    gdd_scale = sd(dat$GDD),
    year_center = 2005
  )

  if (!is.finite(scaling$water_scale) || scaling$water_scale <= 0 ||
      !is.finite(scaling$gdd_scale) || scaling$gdd_scale <= 0) {
    stop("Training-fold predictor standard deviations must be positive.")
  }
  scaling
}

add_scaled_predictors <- function(dat, scaling) {
  dat %>%
    mutate(
      ZTotalWater = (totalWater_m - scaling$water_center) /
        scaling$water_scale,
      ZGDD = (GDD - scaling$gdd_center) / scaling$gdd_scale,
      Year_centered = Year - scaling$year_center
    )
}

score_predictions <- function(dat, model_name, crop, fold) {
  complete <- complete.cases(
    dat$yield_kgHa_detrended,
    dat$prediction
  )
  obs <- dat$yield_kgHa_detrended[complete]
  pred <- dat$prediction[complete]

  tibble(
    Crop = crop,
    model = model_name,
    fold = fold,
    n = length(obs),
    RMSE = sqrt(mean((pred - obs)^2)),
    MAE = mean(abs(pred - obs)),
    R2 = if (length(obs) > 1 && sd(obs) > 0 && sd(pred) > 0) {
      cor(obs, pred)^2
    } else {
      NA_real_
    },
    bias = mean(pred - obs)
  )
}

fit_and_score_fold <- function(crop, model_name, fold_row) {
  dat_crop <- yield_data %>% filter(Crop == crop)
  train <- dat_crop %>% filter(Year <= fold_row$train_end)
  test <- dat_crop %>%
    filter(Year >= fold_row$test_start, Year <= fold_row$test_end)

  if (nrow(train) == 0 || nrow(test) == 0) {
    return(list(metrics = tibble(), predictions = tibble()))
  }

  if (crop == "Wheat") {
    scaling <- make_scaling(train)
    train <- add_scaled_predictors(train, scaling)
    formula <- switch(
      model_name,
      wheat_climate_only = wheat_formula_climate_only,
      wheat_no_water_year = wheat_formula_no_water_year,
      wheat_with_water_year = wheat_formula_with_water_year,
      stop("Unknown wheat validation model: ", model_name)
    )
    model <- lmer(formula, data = train, REML = TRUE)
    attr(model, "yield_scaling") <- scaling
  } else {
    model <- lmer(current_formula, data = train, REML = TRUE)
  }

  test$prediction <- predict_one_yield_model(
    model,
    newdata = test,
    fixed_only = FALSE
  )

  predictions <- test %>%
    transmute(
      Crop = crop,
      model = model_name,
      fold = fold_row$fold,
      Year,
      FIPS,
      observed = yield_kgHa_detrended,
      predicted = prediction,
      residual = observed - predicted
    )

  list(
    metrics = score_predictions(
      test,
      model_name = model_name,
      crop = crop,
      fold = fold_row$fold
    ),
    predictions = predictions
  )
}

model_grid <- bind_rows(
  tibble(
    Crop = c("Corn", "Soybeans", "Sorghum"),
    model = "current"
  ),
  tibble(
    Crop = "Wheat",
    model = c(
      "wheat_climate_only",
      "wheat_no_water_year",
      "wheat_with_water_year"
    )
  )
)

validation_results <- pmap(
  tidyr::crossing(
    model_grid,
    fold_index = seq_len(nrow(validation_blocks))
  ),
  function(Crop, model, fold_index) {
    fit_and_score_fold(
      crop = Crop,
      model_name = model,
      fold_row = validation_blocks[fold_index, ]
    )
  }
)

validation_metrics <- map_dfr(validation_results, "metrics")
validation_predictions <- map_dfr(validation_results, "predictions")

validation_summary <- validation_predictions %>%
  group_by(Crop, model) %>%
  summarise(
    n = sum(complete.cases(observed, predicted)),
    RMSE = sqrt(mean((predicted - observed)^2, na.rm = TRUE)),
    MAE = mean(abs(predicted - observed), na.rm = TRUE),
    R2 = cor(observed, predicted, use = "complete.obs")^2,
    bias = mean(predicted - observed, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  validation_metrics,
  file.path(out_dir, "yield_year_block_validation_by_fold.csv")
)
write_csv(
  validation_summary,
  file.path(out_dir, "yield_year_block_validation_summary.csv")
)
write_csv(
  validation_predictions,
  file.path(out_dir, "yield_year_block_validation_predictions.csv")
)

p_validation <- ggplot(
  validation_predictions %>%
    mutate(
      panel = case_when(
        model == "wheat_climate_only" ~ "Wheat: climate only",
        model == "wheat_no_water_year" ~ "Wheat: no water × year",
        model == "wheat_with_water_year" ~ "Wheat: with water × year",
        TRUE ~ Crop
      )
    ),
  aes(x = observed, y = predicted, color = fold)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.65) +
  facet_wrap(~panel, scales = "free") +
  labs(
    x = "Observed detrended yield (kg/ha)",
    y = "Forward-block prediction (kg/ha)",
    color = "Held-out years",
    title = "Yield-model forward year-block validation"
  ) +
  theme_bw()

ggsave(
  file.path(fig_dir, "yield_year_block_validation.png"),
  p_validation,
  width = 12,
  height = 8,
  dpi = 300
)

# -------------------------------------------------------------------------
# 2006-2100 fixed-climate extrapolation for every production crop model and
# for the wheat alternative without the water-by-year interaction.
# -------------------------------------------------------------------------

models <- load_models(here("hpc_opt", "models"))
wheat_full <- models$yield_kg[["Wheat"]]
wheat_scaling <- attr(wheat_full, "yield_scaling", exact = TRUE)
wheat_all <- yield_data %>% filter(Crop == "Wheat")
wheat_all_scaled <- add_scaled_predictors(wheat_all, wheat_scaling)
wheat_no_water_year <- lmer(
  wheat_formula_no_water_year,
  data = wheat_all_scaled,
  REML = TRUE
)
attr(wheat_no_water_year, "yield_scaling") <- wheat_scaling

wheat_climate_only <- lmer(
  wheat_formula_climate_only,
  data = wheat_all_scaled,
  REML = TRUE
)
attr(wheat_climate_only, "yield_scaling") <- wheat_scaling

wheat_in_sample_comparison <- imap_dfr(
  list(
    wheat_climate_only = wheat_climate_only,
    wheat_no_water_year = wheat_no_water_year,
    wheat_with_water_year = wheat_full
  ),
  function(model_object, model_name) {
    pred <- predict_one_yield_model(
      model_object,
      newdata = wheat_all,
      fixed_only = FALSE
    )
    obs <- wheat_all$yield_kgHa_detrended
    tibble(
      model = model_name,
      in_sample_n = sum(complete.cases(obs, pred)),
      in_sample_RMSE = sqrt(mean((pred - obs)^2, na.rm = TRUE)),
      in_sample_MAE = mean(abs(pred - obs), na.rm = TRUE),
      in_sample_R2 = cor(obs, pred, use = "complete.obs")^2,
      AIC = AIC(model_object)
    )
  }
) %>%
  left_join(
    validation_summary %>%
      filter(Crop == "Wheat") %>%
      select(
        model,
        blocked_n = n,
        blocked_RMSE = RMSE,
        blocked_MAE = MAE,
        blocked_R2 = R2,
        blocked_bias = bias
      ),
    by = "model"
  )

write_csv(
  wheat_in_sample_comparison,
  file.path(out_dir, "wheat_three_model_comparison.csv")
)

plot_models <- c(
  models$yield_kg,
  Wheat_climate_only = list(wheat_climate_only),
  Wheat_no_water_year = list(wheat_no_water_year)
)

extrapolation <- imap_dfr(plot_models, function(model, model_key) {
  crop <- if (model_key %in% c(
    "Wheat_climate_only",
    "Wheat_no_water_year"
  )) {
    "Wheat"
  } else {
    model_key
  }
  dat_crop <- yield_data %>% filter(Crop == crop)
  water_levels <- setNames(
    as.numeric(
      quantile(
        dat_crop$totalWater_m,
        probs = c(0.10, 0.50, 0.90),
        na.rm = TRUE
      )
    ),
    c("dry", "median", "wet")
  )

  grid <- crossing(
    Year = 2006:2100,
    water_condition = names(water_levels)
  ) %>%
    mutate(
      Crop = crop,
      model = if (model_key == "Wheat_climate_only") {
        "wheat_climate_only"
      } else if (model_key == "Wheat_no_water_year") {
        "wheat_no_water_year"
      } else if (crop == "Wheat") {
        "wheat_with_water_year"
      } else {
        "current"
      },
      totalWater_m = unname(water_levels[water_condition]),
      GDD = median(dat_crop$GDD, na.rm = TRUE),
      FIPS = as.character(dat_crop$FIPS[[1]])
    )

  grid$predicted_yield <- predict_one_yield_model(
    model,
    newdata = grid,
    fixed_only = TRUE
  )
  grid
})

write_csv(
  extrapolation,
  file.path(out_dir, "yield_2006_2100_extrapolation.csv")
)

p_extrapolation <- ggplot(
  extrapolation %>%
    mutate(
      panel = case_when(
        model == "wheat_climate_only" ~ "Wheat: climate only",
        model == "wheat_no_water_year" ~ "Wheat: no water × year",
        model == "wheat_with_water_year" ~ "Wheat: with water × year",
        TRUE ~ Crop
      )
    ),
  aes(
    x = Year,
    y = predicted_yield,
    color = water_condition
  )
) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.4) +
  geom_vline(
    xintercept = max(years_hist),
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~panel, scales = "free_y") +
  scale_color_brewer(
    palette = "Dark2",
    breaks = c("dry", "median", "wet")
  ) +
  labs(
    x = "Year",
    y = "Predicted detrended yield (kg/ha)",
    color = "Total-water condition",
    title = "Yield-model extrapolation under fixed climate conditions",
    subtitle = paste0(
      "GDD fixed at each crop's historical median; water fixed at ",
      "historical 10th, 50th, and 90th percentiles"
    )
  ) +
  theme_bw()

ggsave(
  file.path(fig_dir, "yield_2006_2100_extrapolation.png"),
  p_extrapolation,
  width = 12,
  height = 9,
  dpi = 300
)

message("Wrote validation and extrapolation results to: ", out_dir)
