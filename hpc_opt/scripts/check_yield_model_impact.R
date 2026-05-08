suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

source(here("hpc_opt","R","10_helpers.R"))
source(here("hpc_opt","R","11_models_load.R"))
source(here("hpc_opt","R","12_irrigation_predict.R"))
source(here("hpc_opt","R","13_yield_predict.R"))

yrs_common <- 2006:2023

# -----------------------------
# load inputs
# -----------------------------
df_combined_county <- read_csv(
  here("data","common_inputs_county.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% yrs_common) %>%
  mutate(
    FIPS = as.character(FIPS),
    is_irrig_hist = build_irrig_flags(cur_data_all()),
    WaterManagement = if_else(is_irrig_hist, "Irrigated", "Non-Irrigated")
  )

df_yield_obs <- read_csv(
  here("data","CropYieldData_County.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% yrs_common) %>%
  mutate(FIPS = as.character(FIPS))

df_gdd <- read_csv(
  here("data","gdd_all_gs_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    FIPS = as.character(FIPS),
    Crop = recode(crop,
                  "Corn" = "Corn",
                  "Soybeans" = "Soybeans",
                  "Sorghum" = "Sorghum",
                  "Wheat" = "Wheat"
    )
  ) %>%
  select(FIPS, Year, Crop, GDD)

# -----------------------------
# load old saved models
# -----------------------------
old_models <- load_models(here("hpc_opt","models"))
irr_lm_old <- old_models$irrigation_lm

# -----------------------------
# rebuild corrected df_crop
# -----------------------------
irr_aug <- predict_irrigation_depth(
  df_all = df_combined_county,
  irrigation_lm = irr_lm_old,
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
    ),
    totalWater_m = precip_m + irrigation_WaterUse_m
  ) %>%
  left_join(
    df_yield_obs,
    by = c("Year","FIPS","Crop","WaterManagement")
  ) %>%
  left_join(df_gdd, by = c("Year","FIPS","Crop")) %>%
  filter(
    is.finite(totalWater_m),
    is.finite(yield_kgHa_detrended),
    is.finite(yield_kcalHa_detrended)
  ) %>% rename(GDD = GDD.x) 

if ("area_ha" %in% names(df_crop)) {
  area_ha_thres <- 64.75 * 10
  df_crop <- df_crop %>% filter(area_ha > area_ha_thres)
}

# -----------------------------
# fit corrected temporary models
# -----------------------------
crops <- sort(unique(df_crop$Crop))

new_yield_kg <- list()
new_yield_kcal <- list()

for (cr in crops) {
  dat_cr <- df_crop %>% filter(Crop == cr)
  
  new_yield_kg[[cr]] <- lm(
    yield_kgHa_detrended ~ poly(totalWater_m, 2, raw = TRUE) + tmin_C_avg + tmax_C_avg,
    data = dat_cr
  )
  
  new_yield_kcal[[cr]] <- lm(
    yield_kcalHa_detrended ~ poly(totalWater_m, 2, raw = TRUE) + tmin_C_avg + tmax_C_avg,
    data = dat_cr
  )
}

# -----------------------------
# predict old vs new on SAME corrected df_crop
# -----------------------------
pred_old <- predict_yields(
  df = df_crop,
  yield_kg_models = old_models$yield_kg,
  yield_kcal_models = old_models$yield_kcal,
  fixed_only = TRUE
) %>%
  transmute(
    Year, FIPS, Crop, WaterManagement,
    old_pred_kg = yield_kgHa_pred,
    old_pred_kcal = yield_kcalHa_pred
  )

pred_new <- predict_yields(
  df = df_crop,
  yield_kg_models = new_yield_kg,
  yield_kcal_models = new_yield_kcal,
  fixed_only = TRUE
) %>%
  transmute(
    Year, FIPS, Crop, WaterManagement,
    new_pred_kg = yield_kgHa_pred,
    new_pred_kcal = yield_kcalHa_pred
  )

pred_compare <- df_crop %>%
  select(Year, FIPS, Crop, WaterManagement, yield_kgHa_detrended, yield_kcalHa_detrended) %>%
  left_join(pred_old, by = c("Year","FIPS","Crop","WaterManagement")) %>%
  left_join(pred_new, by = c("Year","FIPS","Crop","WaterManagement")) %>%
  mutate(
    diff_kg = new_pred_kg - old_pred_kg,
    pct_diff_kg = 100 * diff_kg / old_pred_kg,
    diff_kcal = new_pred_kcal - old_pred_kcal,
    pct_diff_kcal = 100 * diff_kcal / old_pred_kcal
  )

# -----------------------------
# summarize differences
# -----------------------------
impact_summary <- pred_compare %>%
  group_by(Crop) %>%
  summarise(
    n = n(),
    cor_old_new_kg = cor(old_pred_kg, new_pred_kg, use = "complete.obs"),
    rmse_old_new_kg = sqrt(mean((new_pred_kg - old_pred_kg)^2, na.rm = TRUE)),
    mean_old_kg = mean(old_pred_kg, na.rm = TRUE),
    mean_new_kg = mean(new_pred_kg, na.rm = TRUE),
    mean_abs_pct_diff_kg = mean(abs(pct_diff_kg), na.rm = TRUE),
    
    cor_old_new_kcal = cor(old_pred_kcal, new_pred_kcal, use = "complete.obs"),
    rmse_old_new_kcal = sqrt(mean((new_pred_kcal - old_pred_kcal)^2, na.rm = TRUE)),
    mean_old_kcal = mean(old_pred_kcal, na.rm = TRUE),
    mean_new_kcal = mean(new_pred_kcal, na.rm = TRUE),
    mean_abs_pct_diff_kcal = mean(abs(pct_diff_kcal), na.rm = TRUE),
    .groups = "drop"
  )

print(impact_summary)

dir.create(here("hpc_opt","outputs","model_checks"), recursive = TRUE, showWarnings = FALSE)

write_csv(pred_compare, here("hpc_opt","outputs","model_checks","yield_old_vs_new_predictions.csv"))
write_csv(impact_summary, here("hpc_opt","outputs","model_checks","yield_old_vs_new_summary.csv"))

# -----------------------------
# optional scatterplot old vs new
# -----------------------------
p_compare <- ggplot(pred_compare, aes(x = old_pred_kg, y = new_pred_kg, color = WaterManagement)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ Crop, scales = "free") +
  labs(
    x = "Old saved model prediction (kg/ha)",
    y = "New corrected model prediction (kg/ha)",
    title = "Old vs corrected yield model predictions"
  ) +
  theme_bw()
p_compare

ggsave(
  here("hpc_opt","outputs","model_checks","yield_old_vs_new_scatter.png"),
  p_compare,
  width = 8,
  height = 6,
  dpi = 300
)