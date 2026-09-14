suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
})

source(here("hpc_opt", "R", "10_helpers.R"))

audit_dir <- here("hpc_opt", "outputs", "historical_irrigation_audit")
before_dir <- file.path(audit_dir, "before")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

required_before <- c(
  "int_basin_annual.csv",
  "yield_final_model_metrics.csv"
)
missing_before <- required_before[!file.exists(file.path(before_dir, required_before))]
if (length(missing_before) > 0L) {
  stop("Missing preserved pre-correction audit inputs: ",
       paste(missing_before, collapse = ", "))
}

county <- read_csv(
  here("hpc_opt", "outputs", "common_inputs_county_hist_baseline.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% 2006:2023, Crop %in% c("Corn", "Soybeans")) %>%
  mutate(binary_irrigation_flag = build_irrig_flags(pick(everything())))

wateruse <- read_csv(
  here("data", "WaterUseByCrop_EKSRB.csv"),
  show_col_types = FALSE
) %>%
  filter(Year %in% 2006:2023, Crop %in% c("Corn", "Soybeans"))

classification_comparison <- county %>%
  group_by(Year, Crop) %>%
  summarise(
    total_crop_area_ha = sum(area_ha, na.rm = TRUE),
    binary_flagged_area_ha = sum(area_ha[binary_irrigation_flag], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    wateruse %>% select(Year, Crop, observed_irrigated_area_ha = irrArea_ha),
    by = c("Year", "Crop")
  ) %>%
  mutate(
    observed_irrigated_fraction = observed_irrigated_area_ha / total_crop_area_ha,
    binary_flagged_fraction = binary_flagged_area_ha / total_crop_area_ha,
    binary_to_observed_area_ratio =
      binary_flagged_area_ha / observed_irrigated_area_ha
  )

old_basin <- read_csv(
  file.path(before_dir, "int_basin_annual.csv"),
  show_col_types = FALSE
)
new_basin <- read_csv(
  here("hpc_opt", "outputs", "integration", "int_basin_annual.csv"),
  show_col_types = FALSE
)

observed_withdrawal <- wateruse %>%
  group_by(Year) %>%
  summarise(
    observed_irrigation_m3 = sum(waterUse_m3, na.rm = TRUE),
    observed_irrigated_area_ha = sum(irrArea_ha, na.rm = TRUE),
    .groups = "drop"
  )

annual_comparison <- old_basin %>%
  select(
    Year,
    old_yield_kg = Yield_total_kg,
    old_profit_usd = NetReturn_total_usd,
    old_irrigation_m3 = Irrigation_total_m3,
    old_nitrate_kg = NitrateFluxPredicted_kg
  ) %>%
  left_join(
    new_basin %>% select(
      Year,
      new_yield_kg = Yield_total_kg,
      new_profit_usd = NetReturn_total_usd,
      new_irrigation_m3 = Irrigation_total_m3,
      new_nitrate_kg = NitrateFluxPredicted_kg
    ),
    by = "Year"
  ) %>%
  left_join(observed_withdrawal, by = "Year") %>%
  mutate(
    yield_change_percent = 100 * (new_yield_kg / old_yield_kg - 1),
    profit_change_percent = 100 * (new_profit_usd / old_profit_usd - 1),
    nitrate_change_percent = 100 * (new_nitrate_kg / old_nitrate_kg - 1),
    modeled_vs_observed_irrigation_percent =
      100 * (new_irrigation_m3 / observed_irrigation_m3 - 1)
  )

old_metrics <- read_csv(
  file.path(before_dir, "yield_final_model_metrics.csv"),
  show_col_types = FALSE
)
new_metrics <- read_csv(
  here("hpc_opt", "outputs", "model_checks", "yield_final_model_metrics.csv"),
  show_col_types = FALSE
)

yield_model_comparison <- old_metrics %>%
  rename_with(~ paste0("old_", .x), -Crop) %>%
  left_join(new_metrics %>% rename_with(~ paste0("new_", .x), -Crop), by = "Crop")

write_csv(classification_comparison, file.path(audit_dir, "classification_comparison.csv"))
write_csv(annual_comparison, file.path(audit_dir, "annual_outcome_comparison.csv"))
write_csv(yield_model_comparison, file.path(audit_dir, "yield_model_comparison.csv"))

message("Wrote historical irrigation audit tables to: ", audit_dir)
