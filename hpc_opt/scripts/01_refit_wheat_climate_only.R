# Targeted production refit for the selected climate-only wheat model.
suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(lme4)
})

source(here("hpc_opt", "R", "10_helpers.R"))
source(here("hpc_opt", "R", "12_irrigation_predict.R"))
source(here("hpc_opt", "R", "13_yield_predict.R"))

years_hist <- 2006:2023

county_hist <- read_csv(
  here("hpc_opt", "outputs", "common_inputs_county_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% years_hist) %>%
  mutate(
    FIPS = as.character(FIPS),
    is_irrig_hist = build_irrig_flags(pick(everything())),
    WaterManagement = if_else(is_irrig_hist, "Irrigated", "Non-Irrigated")
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

wheat_data <- predict_irrigation_depth(
  df_all = county_hist,
  irrigation_lm = irrigation_model,
  irrigated_crops = c("Corn", "Soybeans"),
  require_irrigated_mgmt = TRUE
) %>%
  filter(Crop == "Wheat", Year %in% years_hist) %>%
  mutate(
    FIPS = as.character(FIPS),
    irrigation_WaterUse_m = 0
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
    area_ha > 64.75 * 10,
    is.finite(yield_kgHa_detrended),
    is.finite(yield_kcalHa_detrended),
    is.finite(totalWater_m),
    is.finite(GDD)
  )

wheat_scaling <- list(
  water_center = mean(wheat_data$totalWater_m),
  water_scale = sd(wheat_data$totalWater_m),
  gdd_center = mean(wheat_data$GDD),
  gdd_scale = sd(wheat_data$GDD),
  year_center = 2005
)

wheat_scaled <- wheat_data %>%
  mutate(
    ZTotalWater = (totalWater_m - wheat_scaling$water_center) /
      wheat_scaling$water_scale,
    ZGDD = (GDD - wheat_scaling$gdd_center) / wheat_scaling$gdd_scale
  )

wheat_formula_kg <- yield_kgHa_detrended ~
  ZTotalWater + I(ZTotalWater^2) + I(ZTotalWater^3) +
  ZGDD + ZTotalWater:ZGDD + (1 | FIPS)

wheat_formula_kcal <- update(
  wheat_formula_kg,
  yield_kcalHa_detrended ~ .
)

wheat_kg <- lmer(wheat_formula_kg, data = wheat_scaled, REML = TRUE)
wheat_kcal <- lmer(wheat_formula_kcal, data = wheat_scaled, REML = TRUE)
attr(wheat_kg, "yield_scaling") <- wheat_scaling
attr(wheat_kcal, "yield_scaling") <- wheat_scaling

out_kg <- here("hpc_opt", "models", "yield_kg_Wheat_climate_only_20260807.rds")
out_kcal <- here("hpc_opt", "models", "yield_kcal_Wheat_climate_only_20260807.rds")
saveRDS(wheat_kg, out_kg)
saveRDS(wheat_kcal, out_kcal)

check_kg <- readRDS(out_kg)
check_kcal <- readRDS(out_kcal)
for (model in list(check_kg, check_kcal)) {
  terms_text <- paste(deparse(formula(model)), collapse = " ")
  if (grepl("Year_centered", terms_text, fixed = TRUE)) {
    stop("Climate-only wheat refit unexpectedly contains Year_centered.")
  }
  if (nobs(model) != nrow(wheat_data)) {
    stop("Saved wheat model row count does not match the training table.")
  }
}

message("Verified climate-only wheat models (n = ", nrow(wheat_data), ").")
message("Temporary kg model: ", out_kg)
message("Temporary kcal model: ", out_kcal)
