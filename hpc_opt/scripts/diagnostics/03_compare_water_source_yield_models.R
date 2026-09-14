suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(lme4)
  library(purrr)
  library(readr)
  library(tidyr)
})

source(here("hpc_opt", "R", "10_helpers.R"))
source(here("hpc_opt", "R", "12_irrigation_predict.R"))

years_hist <- 2006:2023
crops_test <- c("Corn", "Soybeans")
out_dir <- here(
  "hpc_opt", "outputs", "historical_irrigation_audit",
  "water_source_specifications"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

county <- read_csv(
  here("hpc_opt", "outputs", "common_inputs_county_hist_baseline.csv"),
  show_col_types = FALSE, col_types = cols(FIPS = col_character())
) %>% filter(Year %in% years_hist)
wateruse <- read_csv(
  here("data", "WaterUseByCrop_EKSRB.csv"),
  show_col_types = FALSE
) %>% filter(Year %in% years_hist)
yield_obs <- read_csv(
  here("data", "CropYieldData_County.csv"),
  show_col_types = FALSE, col_types = cols(FIPS = col_character())
) %>% filter(Year %in% years_hist)
crop_climate <- read_csv(
  here("data", "crop_climate_gs_hist_baseline.csv"),
  show_col_types = FALSE, col_types = cols(FIPS = col_character())
) %>% transmute(
  FIPS, Year, Crop = crop, GDD,
  precip_gs_m = precip_gs_mm / 1000
)

model_data <- split_historical_water_management(
  county, wateruse, irrigated_crops = crops_test
) %>%
  predict_irrigation_depth(
    irrigation_lm = readRDS(here("hpc_opt", "models", "irr_lm.rds")),
    irrigated_crops = crops_test,
    require_irrigated_mgmt = TRUE
  ) %>%
  filter(Crop %in% crops_test) %>%
  mutate(irrigation_WaterUse_m = if_else(
    WaterManagement == "Non-Irrigated", 0, irrigation_WaterUse_m
  )) %>%
  left_join(yield_obs, by = c("Year", "FIPS", "Crop", "WaterManagement")) %>%
  left_join(crop_climate, by = c("Year", "FIPS", "Crop"), suffix = c("", "_climate")) %>%
  mutate(
    GDD = coalesce(GDD_climate, GDD),
    GDD_1000 = GDD / 1000,
    totalWater_m = precip_gs_m + irrigation_WaterUse_m,
    WaterManagement = factor(
      WaterManagement, levels = c("Non-Irrigated", "Irrigated")
    )
  ) %>%
  filter(
    crop_area_ha_unsplit > 64.75 * 10,
    is.finite(yield_kgHa_detrended),
    is.finite(precip_gs_m),
    is.finite(irrigation_WaterUse_m),
    is.finite(GDD_1000)
  )

formulas <- list(
  total_water_curve = yield_kgHa_detrended ~
    poly(totalWater_m, 2, raw = TRUE) + GDD_1000 + (1 | FIPS),
  total_water_plus_management = yield_kgHa_detrended ~
    poly(totalWater_m, 2, raw = TRUE) + GDD_1000 + WaterManagement + (1 | FIPS),
  separate_sources_linear_irrigation = yield_kgHa_detrended ~
    poly(precip_gs_m, 2, raw = TRUE) + irrigation_WaterUse_m + GDD_1000 + (1 | FIPS),
  separate_sources_curved = yield_kgHa_detrended ~
    poly(precip_gs_m, 2, raw = TRUE) +
    poly(irrigation_WaterUse_m, 2, raw = TRUE) + GDD_1000 + (1 | FIPS)
)

fit_model <- function(formula, data) {
  lmer(
    formula, data = data, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
  )
}

metrics <- function(obs, pred) {
  tibble(
    n = length(obs),
    RMSE = sqrt(mean((pred - obs)^2)),
    MAE = mean(abs(pred - obs)),
    R2 = cor(obs, pred)^2,
    bias = mean(pred - obs)
  )
}

year_folds <- tribble(
  ~fold, ~train_end, ~test_start, ~test_end,
  "2012_2015", 2011L, 2012L, 2015L,
  "2016_2019", 2015L, 2016L, 2019L,
  "2020_2023", 2019L, 2020L, 2023L
)

fit_rows <- list()
metric_rows <- list()
coefficient_rows <- list()

for (crop_name in crops_test) {
  dat <- filter(model_data, Crop == crop_name)
  county_folds <- tibble(
    FIPS = sort(unique(dat$FIPS)),
    county_fold = rep(1:5, length.out = n_distinct(dat$FIPS))
  )

  for (model_name in names(formulas)) {
    form <- formulas[[model_name]]
    full_fit <- fit_model(form, dat)
    fit_rows[[length(fit_rows) + 1L]] <- tibble(
      Crop = crop_name, model = model_name, n = nrow(dat),
      AIC = AIC(full_fit), BIC = BIC(full_fit),
      singular = isSingular(full_fit, tol = 1e-5)
    )
    coefficient_rows[[length(coefficient_rows) + 1L]] <- tibble(
      Crop = crop_name, model = model_name,
      term = names(fixef(full_fit)), estimate = unname(fixef(full_fit))
    )

    for (i in seq_len(nrow(year_folds))) {
      fd <- year_folds[i, ]
      train <- filter(dat, Year <= fd$train_end)
      test <- filter(dat, Year >= fd$test_start, Year <= fd$test_end)
      fit <- fit_model(form, train)
      pred <- predict(fit, newdata = test, allow.new.levels = TRUE)
      metric_rows[[length(metric_rows) + 1L]] <- metrics(
        test$yield_kgHa_detrended, pred
      ) %>% mutate(
        Crop = crop_name, model = model_name,
        validation = "forward_year_block", fold = fd$fold,
        .before = 1
      )
    }

    dat_fold <- left_join(dat, county_folds, by = "FIPS")
    for (fold_id in 1:5) {
      train <- filter(dat_fold, county_fold != fold_id)
      test <- filter(dat_fold, county_fold == fold_id)
      fit <- fit_model(form, train)
      pred <- predict(fit, newdata = test, re.form = NA, allow.new.levels = TRUE)
      metric_rows[[length(metric_rows) + 1L]] <- metrics(
        test$yield_kgHa_detrended, pred
      ) %>% mutate(
        Crop = crop_name, model = model_name,
        validation = "held_out_county", fold = as.character(fold_id),
        .before = 1
      )
    }
  }
}

fit_information <- bind_rows(fit_rows)
fold_metrics <- bind_rows(metric_rows)
validation_summary <- fold_metrics %>%
  group_by(Crop, model, validation) %>%
  summarise(
    folds = dplyr::n(),
    RMSE = weighted.mean(RMSE, n),
    MAE = weighted.mean(MAE, n),
    R2 = weighted.mean(R2, n),
    bias = weighted.mean(bias, n),
    n = sum(n),
    .groups = "drop"
  )
coefficients <- bind_rows(coefficient_rows)

write_csv(fit_information, file.path(out_dir, "fit_information_criteria.csv"))
write_csv(fold_metrics, file.path(out_dir, "fold_metrics.csv"))
write_csv(validation_summary, file.path(out_dir, "validation_summary.csv"))
write_csv(coefficients, file.path(out_dir, "fixed_effect_coefficients.csv"))

comparison_plot <- validation_summary %>%
  filter(model != "separate_sources_curved") %>%
  mutate(model = recode(
    model,
    total_water_curve = "Combined total water",
    total_water_plus_management = "Total water + management intercept",
    separate_sources_linear_irrigation = "Selected: rain + irrigation"
  ), model = factor(model, levels = c(
    "Combined total water", "Total water + management intercept",
    "Selected: rain + irrigation"
  ))) %>%
  ggplot(aes(model, RMSE, fill = validation)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  facet_wrap(~Crop, scales = "free_y") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(
    x = NULL, y = "Cross-validation RMSE (kg/ha)",
    fill = "Validation",
    title = "Yield-model comparison by water representation"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 25, hjust = 1)
  )
ggsave(
  file.path(out_dir, "water_source_model_validation.png"),
  comparison_plot, width = 10, height = 5.5, dpi = 220
)

response_plot_data <- map_dfr(crops_test, function(crop_name) {
  dat <- filter(model_data, Crop == crop_name)
  fit <- fit_model(formulas$separate_sources_linear_irrigation, dat)
  rain <- seq(
    quantile(dat$precip_gs_m, 0.05), quantile(dat$precip_gs_m, 0.95),
    length.out = 150
  )
  positive_irrigation <- dat$irrigation_WaterUse_m[
    dat$irrigation_WaterUse_m > 0
  ]
  typical_irrigation <- median(positive_irrigation)
  irrigation <- seq(0, quantile(positive_irrigation, 0.95), length.out = 150)
  rain_grid <- crossing(
    precip_gs_m = rain,
    line = c("Rainfed: no irrigation", "Irrigated: typical depth")
  ) %>%
    mutate(
      irrigation_WaterUse_m = if_else(
        line == "Rainfed: no irrigation", 0, typical_irrigation
      ),
      panel = "Response across rainfall",
      water_m = precip_gs_m
    )
  irrigation_grid <- tibble(
    precip_gs_m = median(dat$precip_gs_m),
    irrigation_WaterUse_m = irrigation,
    line = "Rainfall held at median",
    panel = "Response to irrigation",
    water_m = irrigation
  )
  grids <- bind_rows(rain_grid, irrigation_grid) %>%
    mutate(
      Crop = crop_name,
      GDD_1000 = median(dat$GDD_1000),
      FIPS = first(dat$FIPS)
    )
  grids$predicted_yield_kgHa <- predict(
    fit, newdata = grids, re.form = NA, allow.new.levels = TRUE
  )
  grids
})

response_plot <- ggplot(
  response_plot_data, aes(water_m, predicted_yield_kgHa, color = line)
) +
  geom_line(linewidth = 1) +
  facet_grid(panel ~ Crop, scales = "free") +
  labs(
    x = "Water depth for the predictor shown (m)",
    y = "Predicted detrended yield (kg/ha)", color = NULL,
    title = "How the selected model represents rainfall and irrigation",
    subtitle = "GDD held at its historical median; county random effects omitted"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(
  file.path(out_dir, "selected_water_source_responses.png"),
  response_plot, width = 9, height = 7, dpi = 220
)

print(validation_summary, n = Inf)
print(fit_information, n = Inf)
