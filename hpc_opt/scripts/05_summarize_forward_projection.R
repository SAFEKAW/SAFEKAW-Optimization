suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
})

# Summarize the deterministic "project climate and land use forward" experiment.
# Management is held at current irrigation and fertilizer settings. The fixed
# land-use scenario therefore represents climate-only change, while BAU combines
# climate change with the configured land-use trajectory.

run_root <- here("hpc_opt", "outputs", "factorial_runs")
hist_objectives_file <- here(
  "hpc_opt", "outputs", "integration", "int_basin_annual.csv"
)
hist_irrigation_file <- here(
  "hpc_opt", "outputs", "common_inputs_basin_hist_baseline.csv"
)
outdir <- here("hpc_opt", "outputs", "deterministic_summary")

required_files <- c(hist_objectives_file, hist_irrigation_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required input(s): ", paste(missing_files, collapse = ", "))
}

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

hist_objectives <- read_csv(hist_objectives_file, show_col_types = FALSE)
hist_irrigation <- read_csv(hist_irrigation_file, show_col_types = FALSE)

hist_ref <- tibble(
  objective = c("nitrate", "irrigation", "net_returns"),
  historical_mean = c(
    mean(hist_objectives$NitrateFluxPredicted_kg, na.rm = TRUE),
    mean(hist_irrigation$Irrigation_total_m3, na.rm = TRUE),
    mean(hist_objectives$NetReturn_total_usd, na.rm = TRUE)
  ),
  units = c("kg N/yr", "m3/yr", "USD/yr"),
  preferred_direction = c("lower", "lower", "higher")
)

run_dirs <- list.dirs(run_root, recursive = FALSE, full.names = TRUE)
period_files <- unlist(lapply(run_dirs, function(run_dir) {
  list.files(
    run_dir,
    pattern = "^deterministic_summary_by_period_.*[.]csv$",
    full.names = TRUE
  )
}), use.names = FALSE)

if (length(period_files) == 0) {
  stop("No deterministic by-period summaries found under: ", run_root)
}

# Read the run-level files directly. This avoids relying on a consolidated
# factorial_check table that may predate the latest rerun of one pathway.
future <- bind_rows(lapply(
  period_files,
  read_csv,
  show_col_types = FALSE
)) %>%
  filter(irrigation_name == "current", fertilizer_name == "current") %>%
  select(
    climate_pathway, period, landuse_name,
    mean_nitrate_kgyr, mean_irrigation_m3yr, mean_profit_usdyr
  )

expected_n <- 2L * 3L * 2L
if (nrow(future) != expected_n) {
  stop(
    "Expected ", expected_n,
    " current-management pathway-period-land-use rows, found ", nrow(future)
  )
}

period_levels <- c("early", "mid", "late")
pathway_levels <- c("rcp45", "rcp85")
landuse_levels <- c("fixed", "bau")

future_long <- future %>%
  pivot_longer(
    cols = c(mean_nitrate_kgyr, mean_irrigation_m3yr, mean_profit_usdyr),
    names_to = "objective",
    values_to = "projected_mean"
  ) %>%
  mutate(
    objective = recode(
      objective,
      mean_nitrate_kgyr = "nitrate",
      mean_irrigation_m3yr = "irrigation",
      mean_profit_usdyr = "net_returns"
    )
  ) %>%
  left_join(hist_ref, by = "objective") %>%
  mutate(
    absolute_change = projected_mean - historical_mean,
    percent_change = 100 * absolute_change / historical_mean,
    relative_to_historical = projected_mean / historical_mean,
    period = factor(period, levels = period_levels),
    climate_pathway = factor(climate_pathway, levels = pathway_levels),
    landuse_name = factor(landuse_name, levels = landuse_levels)
  ) %>%
  arrange(objective, climate_pathway, period, landuse_name)

# Main machine-readable table: raw means and historical-relative changes.
write_csv(
  future_long %>% mutate(across(where(is.factor), as.character)),
  file.path(outdir, "forward_projection_vs_historical_long.csv")
)

# Compact manuscript table, with objectives in columns (million units/year).
manuscript_table <- future_long %>%
  transmute(
    climate_pathway = recode(
      as.character(climate_pathway), rcp45 = "RCP 4.5", rcp85 = "RCP 8.5"
    ),
    period = recode(
      as.character(period),
      early = "2025-2049", mid = "2050-2074", late = "2075-2099"
    ),
    land_use = recode(
      as.character(landuse_name),
      fixed = "Fixed historical land use",
      bau = "BAU land-use projection"
    ),
    objective,
    mean_million_per_year = projected_mean / 1e6,
    change_from_historical_pct = percent_change
  ) %>%
  pivot_wider(
    names_from = objective,
    values_from = c(mean_million_per_year, change_from_historical_pct),
    names_glue = "{objective}_{.value}"
  ) %>%
  mutate(
    across(ends_with("_mean_million_per_year"), ~ round(.x, 2)),
    across(ends_with("_change_from_historical_pct"), ~ round(.x, 1))
  ) %>%
  arrange(climate_pathway, period, land_use)

write_csv(
  manuscript_table,
  file.path(outdir, "forward_projection_manuscript_table.csv")
)

# Land-use effect: BAU minus fixed under the same projected climate. This is an
# additive contrast, not a causal decomposition when model responses interact.
landuse_effect <- future_long %>%
  select(climate_pathway, period, objective, landuse_name, projected_mean) %>%
  pivot_wider(names_from = landuse_name, values_from = projected_mean) %>%
  mutate(
    bau_minus_fixed = bau - fixed,
    bau_vs_fixed_pct = 100 * (bau - fixed) / fixed
  ) %>%
  left_join(hist_ref %>% select(objective, units, preferred_direction), by = "objective") %>%
  arrange(objective, climate_pathway, period) %>%
  mutate(across(where(is.factor), as.character))

write_csv(
  landuse_effect,
  file.path(outdir, "landuse_effect_bau_vs_fixed.csv")
)

# Historical reference table makes denominators explicit and records the years
# used by each source file.
historical_reference <- hist_ref %>%
  mutate(
    historical_start_year = if_else(
      objective == "irrigation",
      min(hist_irrigation$Year, na.rm = TRUE),
      min(hist_objectives$Year, na.rm = TRUE)
    ),
    historical_end_year = if_else(
      objective == "irrigation",
      max(hist_irrigation$Year, na.rm = TRUE),
      max(hist_objectives$Year, na.rm = TRUE)
    )
  )

write_csv(
  historical_reference,
  file.path(outdir, "historical_objective_reference.csv")
)

message("Wrote deterministic projection tables to: ", outdir)
