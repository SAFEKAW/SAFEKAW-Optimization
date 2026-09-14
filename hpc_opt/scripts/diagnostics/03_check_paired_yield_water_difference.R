suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(tidyr)
})

input_file <- here(
  "hpc_opt", "outputs", "historical_irrigation_audit",
  "yield_specifications", "validation_predictions.csv"
)
out_file <- here(
  "hpc_opt", "outputs", "historical_irrigation_audit",
  "paired_yield_boxplots", "paired_water_yield_relationship.csv"
)

paired <- read_csv(input_file, show_col_types = FALSE) %>%
  distinct(Crop, Year, FIPS, WaterManagement, totalWater_m, observed) %>%
  pivot_wider(
    names_from = WaterManagement,
    values_from = c(totalWater_m, observed),
    names_glue = "{.value}_{WaterManagement}"
  ) %>%
  mutate(
    irrigation_increment_m = totalWater_m_Irrigated -
      .data[["totalWater_m_Non-Irrigated"]],
    yield_advantage_kgHa = observed_Irrigated -
      .data[["observed_Non-Irrigated"]]
  )

results <- paired %>%
  group_by(Crop) %>%
  group_modify(function(dat, key) {
    fit <- lm(yield_advantage_kgHa ~ irrigation_increment_m, data = dat)
    tibble(
      county_years = nrow(dat),
      mean_irrigation_increment_m = mean(dat$irrigation_increment_m),
      mean_yield_advantage_kgHa = mean(dat$yield_advantage_kgHa),
      correlation = cor(dat$irrigation_increment_m, dat$yield_advantage_kgHa),
      intercept_kgHa = unname(coef(fit)[1]),
      slope_kgHa_per_m = unname(coef(fit)[2]),
      slope_p_value = summary(fit)$coefficients[2, 4],
      r_squared = summary(fit)$r.squared
    )
  }) %>%
  ungroup()

write_csv(results, out_file)
print(results, n = Inf)
