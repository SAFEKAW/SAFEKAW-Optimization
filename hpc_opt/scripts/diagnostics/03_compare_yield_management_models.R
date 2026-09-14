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
out_dir <- here("hpc_opt", "outputs", "historical_irrigation_audit", "yield_specifications")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

county <- read_csv(
  here("hpc_opt", "outputs", "common_inputs_county_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% years_hist) %>%
  mutate(FIPS = as.character(FIPS))

wateruse <- read_csv(
  here("data", "WaterUseByCrop_EKSRB.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% years_hist)

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
    FIPS = as.character(FIPS), Year, Crop = crop,
    GDD, precip_gs_m = precip_gs_mm / 1000
  )

irrigation_model <- readRDS(here("hpc_opt", "models", "irr_lm.rds"))

model_data <- split_historical_water_management(
  county,
  wateruse,
  irrigated_crops = crops_test
) %>%
  predict_irrigation_depth(
    irrigation_lm = irrigation_model,
    irrigated_crops = crops_test,
    require_irrigated_mgmt = TRUE
  ) %>%
  filter(Crop %in% crops_test) %>%
  mutate(
    irrigation_WaterUse_m = if_else(
      WaterManagement == "Non-Irrigated", 0, irrigation_WaterUse_m
    )
  ) %>%
  left_join(yield_obs, by = c("Year", "FIPS", "Crop", "WaterManagement")) %>%
  left_join(crop_climate, by = c("Year", "FIPS", "Crop"), suffix = c("", "_climate")) %>%
  mutate(
    GDD = coalesce(GDD_climate, GDD),
    totalWater_m = precip_gs_m + irrigation_WaterUse_m
  ) %>%
  filter(
    crop_area_ha_unsplit > 64.75 * 10,
    is.finite(yield_kgHa_detrended),
    is.finite(totalWater_m),
    is.finite(GDD)
  ) %>%
  mutate(WaterManagement = factor(
    WaterManagement,
    levels = c("Non-Irrigated", "Irrigated")
  ))

formulas <- list(
  shared_curve = yield_kgHa_detrended ~
    poly(totalWater_m, 2, raw = TRUE) + GDD + (1 | FIPS),
  management_intercept = yield_kgHa_detrended ~
    poly(totalWater_m, 2, raw = TRUE) + GDD + WaterManagement + (1 | FIPS),
  management_curve = yield_kgHa_detrended ~
    poly(totalWater_m, 2, raw = TRUE) * WaterManagement + GDD + (1 | FIPS)
)

metric_rows <- function(obs, pred, crop, model, validation, fold) {
  tibble(
    Crop = crop,
    model = model,
    validation = validation,
    fold = as.character(fold),
    n = sum(complete.cases(obs, pred)),
    RMSE = sqrt(mean((pred - obs)^2, na.rm = TRUE)),
    MAE = mean(abs(pred - obs), na.rm = TRUE),
    R2 = if (sum(complete.cases(obs, pred)) > 2L) {
      cor(obs, pred, use = "complete.obs")^2
    } else {
      NA_real_
    },
    bias = mean(pred - obs, na.rm = TRUE)
  )
}

fit_model <- function(formula, data) {
  lmer(
    formula,
    data = data,
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa")
  )
}

year_folds <- tribble(
  ~fold, ~train_end, ~test_start, ~test_end,
  "2012_2015", 2011L, 2012L, 2015L,
  "2016_2019", 2015L, 2016L, 2019L,
  "2020_2023", 2019L, 2020L, 2023L
)

all_metrics <- list()
all_predictions <- list()
fit_summary <- list()
curve_predictions <- list()
full_fits <- list()
support_ranges <- list()

for (crop_name in crops_test) {
  dat <- model_data %>% filter(Crop == crop_name)
  county_levels <- sort(unique(dat$FIPS))
  county_fold_lookup <- tibble(
    FIPS = county_levels,
    county_fold = rep(1:5, length.out = length(county_levels))
  )

  overlap <- dat %>%
    group_by(WaterManagement) %>%
    summarise(
      n = n(),
      water_min = min(totalWater_m),
      water_p10 = quantile(totalWater_m, 0.10),
      water_median = median(totalWater_m),
      water_p90 = quantile(totalWater_m, 0.90),
      water_max = max(totalWater_m),
      .groups = "drop"
    ) %>%
    mutate(Crop = crop_name, .before = 1)
  write_csv(overlap, file.path(out_dir, paste0(tolower(crop_name), "_water_overlap.csv")))
  support_ranges[[crop_name]] <- overlap %>%
    select(Crop, WaterManagement, water_min, water_max)

  for (model_name in names(formulas)) {
    form <- formulas[[model_name]]
    full_fit <- fit_model(form, dat)
    full_fits[[paste(crop_name, model_name, sep = "__")]] <- full_fit
    in_pred <- predict(full_fit, newdata = dat, allow.new.levels = TRUE)

    all_metrics[[length(all_metrics) + 1L]] <- metric_rows(
      dat$yield_kgHa_detrended, in_pred, crop_name, model_name, "in_sample", "all"
    )
    all_metrics[[length(all_metrics) + 1L]] <- dat %>%
      mutate(.pred = in_pred) %>%
      group_by(WaterManagement) %>%
      group_modify(~ metric_rows(
        .x$yield_kgHa_detrended, .x$.pred, crop_name, model_name,
        paste0("in_sample_", as.character(.y$WaterManagement)), "all"
      )) %>%
      ungroup()

    fit_summary[[length(fit_summary) + 1L]] <- tibble(
      Crop = crop_name,
      model = model_name,
      n = nrow(dat),
      AIC = AIC(full_fit),
      BIC = BIC(full_fit),
      singular = isSingular(full_fit, tol = 1e-5)
    )

    for (i in seq_len(nrow(year_folds))) {
      fold_def <- year_folds[i, ]
      train <- dat %>% filter(Year <= fold_def$train_end)
      test <- dat %>% filter(Year >= fold_def$test_start, Year <= fold_def$test_end)
      fit <- fit_model(form, train)
      pred <- predict(fit, newdata = test, allow.new.levels = TRUE)
      all_metrics[[length(all_metrics) + 1L]] <- metric_rows(
        test$yield_kgHa_detrended, pred, crop_name, model_name,
        "forward_year_block", fold_def$fold
      )
      all_predictions[[length(all_predictions) + 1L]] <- test %>%
        transmute(
          Crop, Year, FIPS, WaterManagement, totalWater_m, GDD,
          observed = yield_kgHa_detrended,
          predicted = pred,
          model = model_name,
          validation = "forward_year_block",
          fold = fold_def$fold
        )
    }

    dat_with_fold <- dat %>% left_join(county_fold_lookup, by = "FIPS")
    for (fold_id in 1:5) {
      train <- dat_with_fold %>% filter(county_fold != fold_id)
      test <- dat_with_fold %>% filter(county_fold == fold_id)
      fit <- fit_model(form, train)
      pred <- predict(fit, newdata = test, re.form = NA, allow.new.levels = TRUE)
      all_metrics[[length(all_metrics) + 1L]] <- metric_rows(
        test$yield_kgHa_detrended, pred, crop_name, model_name,
        "held_out_county", fold_id
      )
      all_predictions[[length(all_predictions) + 1L]] <- test %>%
        transmute(
          Crop, Year, FIPS, WaterManagement, totalWater_m, GDD,
          observed = yield_kgHa_detrended,
          predicted = pred,
          model = model_name,
          validation = "held_out_county",
          fold = as.character(fold_id)
        )
    }

    water_grid <- crossing(
      totalWater_m = seq(min(dat$totalWater_m), max(dat$totalWater_m), length.out = 200),
      WaterManagement = levels(dat$WaterManagement)
    ) %>%
      mutate(
        GDD = median(dat$GDD),
        FIPS = first(dat$FIPS),
        Crop = crop_name,
        model = model_name
      )
    water_grid$predicted_yield <- predict(
      full_fit, newdata = water_grid, re.form = NA, allow.new.levels = TRUE
    )
    curve_predictions[[length(curve_predictions) + 1L]] <- water_grid
  }
}

metrics <- bind_rows(all_metrics)
predictions <- bind_rows(all_predictions)
fits <- bind_rows(fit_summary)
curves <- bind_rows(curve_predictions)
support <- bind_rows(support_ranges)

likelihood_ratio_tests <- map_dfr(crops_test, function(crop_name) {
  shared <- full_fits[[paste(crop_name, "shared_curve", sep = "__")]]
  intercept <- full_fits[[paste(crop_name, "management_intercept", sep = "__")]]
  separate <- full_fits[[paste(crop_name, "management_curve", sep = "__")]]

  bind_rows(
    as.data.frame(anova(shared, intercept)) %>%
      slice(2) %>%
      transmute(
        Crop = crop_name,
        comparison = "shared_curve_vs_management_intercept",
        added_parameters = Df,
        chi_square = Chisq,
        p_value = `Pr(>Chisq)`
      ),
    as.data.frame(anova(intercept, separate)) %>%
      slice(2) %>%
      transmute(
        Crop = crop_name,
        comparison = "management_intercept_vs_management_curve",
        added_parameters = Df,
        chi_square = Chisq,
        p_value = `Pr(>Chisq)`
      )
  )
})

validation_summary <- metrics %>%
  filter(validation %in% c("forward_year_block", "held_out_county")) %>%
  group_by(Crop, model, validation) %>%
  summarise(
    folds = n(),
    RMSE = weighted.mean(RMSE, n),
    MAE = weighted.mean(MAE, n),
    R2 = weighted.mean(R2, n, na.rm = TRUE),
    bias = weighted.mean(bias, n),
    n = sum(n),
    .groups = "drop"
  )

curve_checks <- curves %>%
  group_by(Crop, model, WaterManagement) %>%
  summarise(
    min_predicted_yield = min(predicted_yield),
    max_predicted_yield = max(predicted_yield),
    fraction_negative = mean(predicted_yield < 0),
    .groups = "drop"
  )

write_csv(metrics, file.path(out_dir, "fold_metrics.csv"))
write_csv(validation_summary, file.path(out_dir, "validation_summary.csv"))
write_csv(predictions, file.path(out_dir, "validation_predictions.csv"))
write_csv(fits, file.path(out_dir, "fit_information_criteria.csv"))
write_csv(curves, file.path(out_dir, "water_curve_predictions.csv"))
write_csv(curve_checks, file.path(out_dir, "water_curve_checks.csv"))
write_csv(likelihood_ratio_tests, file.path(out_dir, "likelihood_ratio_tests.csv"))

p <- ggplot(
  curves %>%
    left_join(support, by = c("Crop", "WaterManagement")) %>%
    filter(totalWater_m >= water_min, totalWater_m <= water_max),
  aes(totalWater_m, predicted_yield, color = WaterManagement)
) +
  geom_line(linewidth = 0.9) +
  facet_grid(Crop ~ model, scales = "free") +
  labs(
    x = "Total growing-season water (m)",
    y = "Predicted detrended yield (kg/ha)",
    title = "Candidate historical water-yield specifications"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "candidate_water_yield_curves.png"), p,
       width = 12, height = 7, dpi = 200)

print(validation_summary, n = Inf)
print(fits, n = Inf)
print(as_tibble(likelihood_ratio_tests), n = Inf)
print(curve_checks, n = Inf)
message("Wrote yield-management model comparison to: ", out_dir)
