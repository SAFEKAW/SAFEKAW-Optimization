# hpc_opt/scripts/01_fit_and_save_models.R
# Fit submodels ONCE (irrigation, yields, water-quality) and save to hpc_opt/models/
# No plots, no knitting, HPC/batch friendly.

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lme4)
})

# --- source minimal helpers + wrappers (fit functions + feature builders) ---
source(here("hpc_opt", "R", "10_helpers.R"))
source(here("hpc_opt", "R", "12_irrigation_predict.R"))  # fit_irrigation_lm()
source(here("hpc_opt", "R", "14_wq_features.R"))
source(here("hpc_opt", "R", "15_wq_predict.R"))          # fit_wq_lm(), build_wq_input_from_basin()

# ---- config ----
yrs_common <- 2006:2023

# Inputs (using the new hpc_opt outputs created by 01_common_inputs_make.R)
in_county <- here("hpc_opt", "outputs", "common_inputs_county.csv")
in_basin  <- here("hpc_opt", "outputs", "common_inputs_basin.csv")

# Observations used for fitting yield + WQ models
in_yield_obs   <- here("data", "CropYieldData_County.csv")
in_nitrate_obs <- here("data", "RiverNitrateData_EKSRB.csv")

# Model output dir
model_dir <- here("hpc_opt", "models")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# ---- quick file checks ----
stopifnot(file.exists(in_county), file.exists(in_basin),
          file.exists(in_yield_obs), file.exists(in_nitrate_obs))

# ---- load data ----
df_combined <- read_csv(in_county, show_col_types = FALSE) %>%
  filter(Year %in% yrs_common) %>%
  mutate(FIPS = as.character(FIPS))

common_input_basin <- read_csv(in_basin, show_col_types = FALSE) %>%
  filter(Year %in% yrs_common)

df_yield <- read_csv(in_yield_obs, show_col_types = FALSE) %>%
  filter(Year %in% yrs_common) %>%
  mutate(FIPS = as.character(FIPS))

df_nitrate <- read_csv(in_nitrate_obs, show_col_types = FALSE) %>%
  filter(Year %in% yrs_common)

# ---- 1) Fit irrigation model and save ----
message("Fitting irrigation lm...")
irr_lm <- fit_irrigation_lm(df_combined, irrigated_crops = c("Corn","Soybeans"))
saveRDS(irr_lm, file.path(model_dir, "irr_lm.rds"))
message("Saved: ", file.path(model_dir, "irr_lm.rds"))
message("Irrigation lm summary:")
print(summary(irr_lm))

# ---- 2) Build modeled irrigation on all rows (needed for crop totalWater) ----
message("Predicting irrigation (for crop model inputs)...")
irr_aug <- predict_irrigation_depth(
  df_all = df_combined,
  irrigation_lm = irr_lm,
  irrigated_crops = c("Corn","Soybeans"),
  require_irrigated_mgmt = TRUE
)

# ---- 3) Fit yield models per crop and save ----
# Your original fixed effects:
#   yield_*_detrended ~ poly(totalWater_m, 2) + GDD
# with random effects:
#   (1|FIPS) + (1|Year)

message("Building crop modeling table...")
df_crop <- irr_aug %>%
  filter(!is.na(Crop), Year %in% yrs_common) %>%
  mutate(
    # irrigation_WaterUse_m is already 0 for ineligible crops from predict_irrigation_depth()
    totalWater_m = precip_m + dplyr::coalesce(irrigation_WaterUse_m, 0)
  ) %>%
  left_join(df_yield, by = c("Year","FIPS","Crop")) %>%
  filter(is.finite(totalWater_m))


# Optional area threshold (kept from your script)
if ("area_ha" %in% names(df_crop)) {
  area_ha_thres <- 64.75 * 10
  df_crop <- df_crop %>% filter(area_ha > area_ha_thres)
}

# Ensure grouping vars are factors (as in your wrapper)
df_crop <- df_crop %>%
  mutate(
    FIPS = as.factor(FIPS),
    Year_f = as.factor(Year)  # keep Year numeric too; use Year_f in random effects
  )

fit_yield_one_crop <- function(dfc, crop) {
  dfc <- dfc %>% filter(Crop == crop)
  
  # Require these columns
  req <- c("yield_kgHa_detrended", "yield_kcalHa_detrended", "totalWater_m", "GDD", "FIPS", "Year_f")
  miss <- setdiff(req, names(dfc))
  if (length(miss) > 0) stop("Missing required cols for yield fit: ", paste(miss, collapse = ", "))
  
  # Formulas (match your intent; Year random effect included)
  f_kg   <- stats::as.formula("yield_kgHa_detrended ~ poly(totalWater_m, 2) + GDD + (1|FIPS) + (1|Year_f)")
  f_kcal <- stats::as.formula("yield_kcalHa_detrended ~ poly(totalWater_m, 2) + GDD + (1|FIPS) + (1|Year_f)")
  
  m_kg   <- lme4::lmer(f_kg,   data = dfc, REML = TRUE)
  m_kcal <- lme4::lmer(f_kcal, data = dfc, REML = TRUE)
  
  list(kg = m_kg, kcal = m_kcal, n = nrow(dfc))
}

message("Fitting yield models per crop...")
yield_crops <- sort(unique(df_crop$Crop))
yield_crops <- intersect(yield_crops, c("Corn","Soybeans","Sorghum","Wheat"))

yield_fit_stats <- tibble::tibble(
  Crop = character(),
  n = integer(),
  R2_kg_fixed = numeric(),
  R2_kcal_fixed = numeric(),
  RMSE_kg_fixed = numeric(),
  RMSE_kcal_fixed = numeric()
)

for (cr in yield_crops) {
  message("  - ", cr)
  fits <- fit_yield_one_crop(df_crop, cr)
  
  # Save models
  saveRDS(fits$kg,   file.path(model_dir, paste0("yield_kg_",   cr, ".rds")))
  saveRDS(fits$kcal, file.path(model_dir, paste0("yield_kcal_", cr, ".rds")))
  
  # Quick fixed-effect-only metrics on training data (for console sanity)
  dfc <- df_crop %>% filter(Crop == cr)
  pred_kg_fixed   <- as.numeric(predict(fits$kg,   newdata = dfc, re.form = NA, allow.new.levels = TRUE))
  pred_kcal_fixed <- as.numeric(predict(fits$kcal, newdata = dfc, re.form = NA, allow.new.levels = TRUE))
  
  obs_kg   <- dfc$yield_kgHa_detrended
  obs_kcal <- dfc$yield_kcalHa_detrended
  
  ok_kg   <- is.finite(obs_kg) & is.finite(pred_kg_fixed)
  ok_kcal <- is.finite(obs_kcal) & is.finite(pred_kcal_fixed)
  
  r2_kg   <- if (sum(ok_kg)   > 5) stats::cor(obs_kg[ok_kg],   pred_kg_fixed[ok_kg])^2 else NA_real_
  r2_kcal <- if (sum(ok_kcal) > 5) stats::cor(obs_kcal[ok_kcal], pred_kcal_fixed[ok_kcal])^2 else NA_real_
  
  rmse_kg   <- if (sum(ok_kg)   > 5) rmse(obs_kg[ok_kg],   pred_kg_fixed[ok_kg]) else NA_real_
  rmse_kcal <- if (sum(ok_kcal) > 5) rmse(obs_kcal[ok_kcal], pred_kcal_fixed[ok_kcal]) else NA_real_
  
  yield_fit_stats <- bind_rows(
    yield_fit_stats,
    tibble::tibble(
      Crop = cr, n = fits$n,
      R2_kg_fixed = r2_kg,
      R2_kcal_fixed = r2_kcal,
      RMSE_kg_fixed = rmse_kg,
      RMSE_kcal_fixed = rmse_kcal
    )
  )
}

message("Saved yield models to: ", model_dir)
message("Yield fixed-effects sanity metrics:")
print(yield_fit_stats)

write_csv(yield_fit_stats, file.path(model_dir, "yield_fit_stats_fixed.csv"))

# ---- 4) Fit WQ model and save ----
# We build basin-year WQ inputs using modeled irrigation and join observed NitrateFlux_kg.
# NOTE: common_input_basin must include LandCover_m2_all_cult etc (HPC-safe names).
message("Building WQ training table...")
wq_in <- build_wq_input_from_basin(common_input_basin, irr_aug = irr_aug, use_modeled_irr = TRUE) %>%
  left_join(df_nitrate %>% select(Year, NitrateFlux_kg), by = "Year")

required <- c("NitrateFlux_kg", "precip_percentileChange",
              "LandCover_m2_grassPastureHay", "LandCover_m2_all_cult",
              "Climate_precip_m", "Climate_Tmean_C")
missing <- setdiff(required, names(wq_in))
if (length(missing) > 0) stop("Missing required cols for WQ fit: ", paste(missing, collapse = ", "))

wq_train <- wq_in %>% tidyr::drop_na(dplyr::all_of(required))

message("Fitting WQ lm...")
wq_lm <- fit_wq_lm(wq_train)
saveRDS(wq_lm, file.path(model_dir, "wq_lm.rds"))
message("Saved: ", file.path(model_dir, "wq_lm.rds"))
message("WQ lm summary:")
print(summary(wq_lm))

# quick WQ metrics on training rows
wq_pred <- predict(wq_lm, newdata = wq_train)
wq_perf <- tibble::tibble(
  n = nrow(wq_train),
  MAE = mae(wq_pred, wq_train$NitrateFlux_kg),
  RMSE = rmse(wq_pred, wq_train$NitrateFlux_kg),
  R2 = if (nrow(wq_train) > 5) stats::cor(wq_pred, wq_train$NitrateFlux_kg)^2 else NA_real_
)
write_csv(wq_perf, file.path(model_dir, "wq_fit_stats.csv"))
message("WQ training performance:")
print(wq_perf)

message("DONE. Models written to: ", model_dir)
