# Compare legacy full-county basin climate weights with corrected
# county-within-EKSRB overlap weights.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(here)
})

legacy_dir <- here(
  "hpc_opt", "outputs", "backup_pre_overlap_weight_fix_20260806",
  "common_inputs_basin"
)
current_dir <- here("hpc_opt", "outputs")
audit_dir <- here("hpc_opt", "outputs", "climate_weighting_audit")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

files <- c(
  "common_inputs_basin_hist_baseline.csv",
  paste0(
    "common_inputs_basin_ensemble_",
    c(
      "rcp45_early", "rcp45_mid", "rcp45_late",
      "rcp85_early", "rcp85_mid", "rcp85_late"
    ),
    ".csv"
  )
)

missing_legacy <- files[!file.exists(file.path(legacy_dir, files))]
missing_current <- files[!file.exists(file.path(current_dir, files))]
if (length(missing_legacy) > 0L || length(missing_current) > 0L) {
  stop(
    "Missing climate-weighting comparison inputs. Legacy: ",
    paste(missing_legacy, collapse = ", "),
    "; current: ", paste(missing_current, collapse = ", ")
  )
}

climate_cols <- c(
  "Climate_precip_m", "Climate_Tmin_C", "Climate_Tmax_C", "Climate_Tmean_C"
)

annual_comparison <- map_dfr(files, function(file_name) {
  scenario <- sub("^common_inputs_basin_", "", file_name)
  scenario <- sub("^ensemble_", "", scenario)
  scenario <- sub("\\.csv$", "", scenario)

  legacy <- read_csv(file.path(legacy_dir, file_name), show_col_types = FALSE) %>%
    select(Year, all_of(climate_cols)) %>%
    rename_with(~ paste0(.x, "_legacy"), all_of(climate_cols))

  current <- read_csv(file.path(current_dir, file_name), show_col_types = FALSE) %>%
    select(Year, all_of(climate_cols)) %>%
    rename_with(~ paste0(.x, "_corrected"), all_of(climate_cols))

  full_join(legacy, current, by = "Year") %>%
    mutate(
      scenario = scenario,
      Climate_precip_diff_m = Climate_precip_m_corrected - Climate_precip_m_legacy,
      Climate_precip_diff_pct = 100 * Climate_precip_diff_m / Climate_precip_m_legacy,
      Climate_Tmin_diff_C = Climate_Tmin_C_corrected - Climate_Tmin_C_legacy,
      Climate_Tmax_diff_C = Climate_Tmax_C_corrected - Climate_Tmax_C_legacy,
      Climate_Tmean_diff_C = Climate_Tmean_C_corrected - Climate_Tmean_C_legacy,
      .before = Year
    )
})

if (anyNA(annual_comparison)) {
  stop("Climate-weighting comparison contains missing values or unmatched years.")
}

summary_comparison <- annual_comparison %>%
  group_by(scenario) %>%
  summarise(
    first_year = min(Year),
    last_year = max(Year),
    n_years = n(),
    precip_diff_pct_min = min(Climate_precip_diff_pct),
    precip_diff_pct_mean = mean(Climate_precip_diff_pct),
    precip_diff_pct_max = max(Climate_precip_diff_pct),
    precip_diff_pct_mean_abs = mean(abs(Climate_precip_diff_pct)),
    tmean_diff_C_min = min(Climate_Tmean_diff_C),
    tmean_diff_C_mean = mean(Climate_Tmean_diff_C),
    tmean_diff_C_max = max(Climate_Tmean_diff_C),
    tmean_diff_C_mean_abs = mean(abs(Climate_Tmean_diff_C)),
    .groups = "drop"
  )

write_csv(
  annual_comparison,
  file.path(audit_dir, "basin_climate_weighting_annual_comparison.csv")
)
write_csv(
  summary_comparison,
  file.path(audit_dir, "basin_climate_weighting_summary.csv")
)

print(summary_comparison, n = Inf, width = Inf)
message("Wrote climate-weighting audit outputs to: ", audit_dir)
