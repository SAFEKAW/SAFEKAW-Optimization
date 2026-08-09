suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(purrr)
  library(tidyr)
})

results_root <- here("hpc_opt", "outputs", "factorial_runs")
run_dirs <- list.dirs(results_root, recursive = FALSE, full.names = TRUE)
files <- unlist(lapply(run_dirs, function(run_dir) {
  current <- file.path(run_dir, "deterministic_results.csv")
  if (file.exists(current)) return(current)
  list.files(
    run_dir,
    pattern = "^deterministic_results_.*[.]csv$",
    full.names = TRUE
  )
}), use.names = FALSE)
if (!length(files)) stop("No deterministic factorial results found.")

results <- map_dfr(files, read_csv, show_col_types = FALSE) %>%
  filter(
    !cap_hit,
    irrigation_technology_name %in% c("current", "efficient"),
    irrigation_extent_name %in% c("baseline", "expanded"),
    fertilizer_name %in% c("current", "efficient")
  ) %>%
  mutate(profit_usdyr = -neg_profit_usdyr)

pair_keys <- c(
  "climate_pathway", "period", "landuse_name", "irrigation_extent_name",
  "fertilizer_name", "Year"
)
paired_technology <- results %>%
  select(
    all_of(pair_keys), irrigation_technology_name, nitrate_kgyr,
    profit_usdyr, irrigation_m3yr
  ) %>%
  pivot_wider(
    names_from = irrigation_technology_name,
    values_from = c(nitrate_kgyr, profit_usdyr, irrigation_m3yr)
  ) %>%
  filter(
    if_all(
      c(ends_with("_current"), ends_with("_efficient")),
      is.finite
    )
  ) %>%
  mutate(
    expected_efficient_irrigation_m3yr = 0.85 * irrigation_m3yr_current,
    expected_profit_gain_usdyr =
      0.15 * irrigation_m3yr_current * 0.029,
    nitrate_abs_error = abs(nitrate_kgyr_efficient - nitrate_kgyr_current),
    irrigation_abs_error = abs(
      irrigation_m3yr_efficient - expected_efficient_irrigation_m3yr
    ),
    profit_gain_abs_error = abs(
      (profit_usdyr_efficient - profit_usdyr_current) -
        expected_profit_gain_usdyr
    ),
    nitrate_ok = nitrate_abs_error <=
      1e-8 * pmax(1, abs(nitrate_kgyr_current)),
    irrigation_ok = irrigation_abs_error <=
      1e-8 * pmax(1, abs(expected_efficient_irrigation_m3yr)),
    profit_ok = profit_gain_abs_error <=
      1e-8 * pmax(1, abs(expected_profit_gain_usdyr)),
    paired_relationships_ok = nitrate_ok & irrigation_ok & profit_ok
  )

fertilizer_pairs <- results %>%
  select(
    climate_pathway, period, landuse_name, irrigation_technology_name,
    irrigation_extent_name, Year, fertilizer_name, nitrate_kgyr,
    fert_factor, nutrient_management_cost_usdyr
  ) %>%
  pivot_wider(
    names_from = fertilizer_name,
    values_from = c(nitrate_kgyr, fert_factor, nutrient_management_cost_usdyr)
  ) %>%
  filter(if_all(c(ends_with("_current"), ends_with("_efficient")), is.finite)) %>%
  mutate(
    fertilizer_factors_ok =
      abs(fert_factor_current - 1) <= 1e-8 &
      abs(fert_factor_efficient - 0.70) <= 1e-8,
    nutrient_cost_ok =
      abs(nutrient_management_cost_usdyr_current) <= 1e-8 &
      nutrient_management_cost_usdyr_efficient > 0,
    nitrate_direction_ok = nitrate_kgyr_efficient <= nitrate_kgyr_current +
      1e-8 * pmax(1, abs(nitrate_kgyr_current)),
    paired_relationships_ok =
      fertilizer_factors_ok & nutrient_cost_ok & nitrate_direction_ok
  )

outdir <- here("hpc_opt", "outputs", "factorial_checks")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
write_csv(
  paired_technology,
  file.path(outdir, "paired_irrigation_technology_checks.csv")
)
write_csv(
  fertilizer_pairs,
  file.path(outdir, "paired_fertilizer_checks.csv")
)

if (!nrow(paired_technology) || !all(paired_technology$paired_relationships_ok)) {
  stop("One or more paired irrigation-technology checks failed.")
}
if (!nrow(fertilizer_pairs) || !all(fertilizer_pairs$paired_relationships_ok)) {
  stop("One or more paired fertilizer checks failed.")
}
message("All deterministic paired-counterfactual checks passed.")
