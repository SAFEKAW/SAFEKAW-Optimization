suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(readr)
  library(tidyr)
})

out_dir <- here("hpc_opt", "outputs", "historical_irrigation_audit", "paired_yield_boxplots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

yield_data <- read_csv(
  here("data", "CropYieldData_County.csv"),
  show_col_types = FALSE,
  col_types = cols(FIPS = col_character())
) %>%
  filter(WaterManagement %in% c("Irrigated", "Non-Irrigated"))

# A county-year enters the comparison only when the same crop has both
# management observations. This holds county, year, and crop composition fixed.
paired_keys <- yield_data %>%
  distinct(Year, FIPS, Crop, WaterManagement) %>%
  count(Year, FIPS, Crop, name = "management_records") %>%
  filter(management_records == 2L)

paired <- yield_data %>%
  inner_join(paired_keys, by = c("Year", "FIPS", "Crop")) %>%
  mutate(
    WaterManagement = factor(
      WaterManagement,
      levels = c("Non-Irrigated", "Irrigated")
    )
  )

plot_data <- paired %>%
  transmute(
    Year, FIPS, Crop, WaterManagement,
    Yield_kgHa = yield_kgHa_detrended
  ) %>%
  filter(is.finite(Yield_kgHa))

summary_table <- plot_data %>%
  group_by(Crop, WaterManagement) %>%
  summarise(
    county_years = n(),
    counties = n_distinct(FIPS),
    years = n_distinct(Year),
    mean_kgHa = mean(Yield_kgHa),
    median_kgHa = median(Yield_kgHa),
    q25_kgHa = quantile(Yield_kgHa, 0.25),
    q75_kgHa = quantile(Yield_kgHa, 0.75),
    .groups = "drop"
  )

paired_differences <- paired %>%
  select(Year, FIPS, Crop, WaterManagement, yield_kgHa, yield_kgHa_detrended) %>%
  pivot_wider(
    names_from = WaterManagement,
    values_from = c(yield_kgHa, yield_kgHa_detrended)
  ) %>%
  transmute(
    Year, FIPS, Crop,
    observed_difference_kgHa = yield_kgHa_Irrigated - `yield_kgHa_Non-Irrigated`,
    detrended_difference_kgHa =
      yield_kgHa_detrended_Irrigated - `yield_kgHa_detrended_Non-Irrigated`
  )

p <- ggplot(plot_data, aes(WaterManagement, Yield_kgHa, fill = WaterManagement)) +
  geom_boxplot(width = 0.62, outlier.shape = NA, alpha = 0.78) +
  geom_jitter(width = 0.12, alpha = 0.22, size = 0.75) +
  facet_wrap(~Crop, scales = "free_y") +
  scale_fill_manual(values = c("Non-Irrigated" = "#E69F00", "Irrigated" = "#56B4E9")) +
  labs(
    x = NULL,
    y = "Yield (kg/ha)",
    title = "Detrended irrigated and non-irrigated yields in matched county-years",
    subtitle = "Only crop/county/year combinations containing both management observations",
    caption = "Source: data/CropYieldData_County.csv"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(
  file.path(out_dir, "paired_irrigated_nonirrigated_yield_boxplots.png"),
  p, width = 10, height = 5.5, dpi = 220
)
write_csv(paired, file.path(out_dir, "paired_yield_observations.csv"))
write_csv(summary_table, file.path(out_dir, "paired_yield_summary.csv"))
write_csv(paired_differences, file.path(out_dir, "paired_yield_differences.csv"))

print(summary_table, n = Inf)
print(
  paired_differences %>%
    group_by(Crop) %>%
    summarise(
      county_years = n(),
      mean_observed_difference_kgHa = mean(observed_difference_kgHa, na.rm = TRUE),
      median_observed_difference_kgHa = median(observed_difference_kgHa, na.rm = TRUE),
      mean_detrended_difference_kgHa = mean(detrended_difference_kgHa, na.rm = TRUE),
      median_detrended_difference_kgHa = median(detrended_difference_kgHa, na.rm = TRUE),
      .groups = "drop"
    ),
  n = Inf
)
