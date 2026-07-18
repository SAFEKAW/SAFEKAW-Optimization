suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(readr)
  library(tidyr)
})

years_ref <- 2006:2023
outdir <- here("hpc_opt", "outputs", "irrigation_domain_comparison")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

read_water_use <- function(path, region) {
  read_csv(path, show_col_types = FALSE) %>%
    filter(Year %in% years_ref) %>%
    mutate(
      Region = region,
      waterUse_m3 = pmax(0, as.numeric(waterUse_m3)),
      irrArea_ha = pmax(0, as.numeric(irrArea_ha))
    )
}

read_land_cover <- function(path, region) {
  read_csv(path, show_col_types = FALSE) %>%
    filter(Year %in% years_ref) %>%
    mutate(Region = region)
}

water_use <- bind_rows(
  read_water_use(
    here("data", "WaterUseByCrop_EKSRB.csv"), "Whole basin"
  ),
  read_water_use(
    here("data", "WaterUseByCrop_AlluvialCorridor.csv"), "Alluvial corridor"
  )
)

land_cover <- bind_rows(
  read_land_cover(
    here("data", "LandCoverData-CDL_EKSRB.csv"), "Whole basin"
  ),
  read_land_cover(
    here("data", "LandCoverData-CDL_AlluvialCorridor.csv"), "Alluvial corridor"
  )
)

cultivated_classes <- c(
  "Corn", "Soybeans", "Sorghum", "Wheat", "Other Crops", "Double Crop"
)

region_land <- land_cover %>%
  group_by(Region, Year) %>%
  summarise(
    region_area_ha = sum(area_ha, na.rm = TRUE),
    cultivated_area_ha = sum(
      area_ha[LandCover %in% cultivated_classes], na.rm = TRUE
    ),
    .groups = "drop"
  )

region_totals <- water_use %>%
  group_by(Region, Year) %>%
  summarise(
    irrigated_area_ha = sum(irrArea_ha, na.rm = TRUE),
    withdrawal_m3 = sum(waterUse_m3, na.rm = TRUE),
    implied_depth_m = withdrawal_m3 / (irrigated_area_ha * 1e4),
    .groups = "drop"
  ) %>%
  left_join(region_land, by = c("Region", "Year")) %>%
  mutate(
    irrigated_fraction_region = irrigated_area_ha / region_area_ha,
    irrigated_fraction_cultivated = irrigated_area_ha / cultivated_area_ha
  )

crop_totals <- water_use %>%
  group_by(Region, Year, Crop) %>%
  summarise(
    irrigated_area_ha = sum(irrArea_ha, na.rm = TRUE),
    withdrawal_m3 = sum(waterUse_m3, na.rm = TRUE),
    implied_depth_m = withdrawal_m3 / (irrigated_area_ha * 1e4),
    .groups = "drop"
  )

domain_capture <- region_totals %>%
  select(Region, Year, irrigated_area_ha, withdrawal_m3) %>%
  pivot_wider(
    names_from = Region,
    values_from = c(irrigated_area_ha, withdrawal_m3)
  ) %>%
  transmute(
    Year,
    area_capture =
      `irrigated_area_ha_Alluvial corridor` / `irrigated_area_ha_Whole basin`,
    withdrawal_capture =
      `withdrawal_m3_Alluvial corridor` / `withdrawal_m3_Whole basin`
  )

crop_capture <- crop_totals %>%
  select(Region, Year, Crop, irrigated_area_ha, withdrawal_m3) %>%
  pivot_wider(
    names_from = Region,
    values_from = c(irrigated_area_ha, withdrawal_m3),
    values_fill = 0
  ) %>%
  mutate(
    area_capture = if_else(
      `irrigated_area_ha_Whole basin` > 0,
      `irrigated_area_ha_Alluvial corridor` / `irrigated_area_ha_Whole basin`,
      NA_real_
    ),
    withdrawal_capture = if_else(
      `withdrawal_m3_Whole basin` > 0,
      `withdrawal_m3_Alluvial corridor` / `withdrawal_m3_Whole basin`,
      NA_real_
    )
  )

write_csv(region_totals, file.path(outdir, "regional_irrigation_by_year.csv"))
write_csv(crop_totals, file.path(outdir, "regional_irrigation_by_crop_year.csv"))
write_csv(domain_capture, file.path(outdir, "alluvial_share_of_basin_by_year.csv"))
write_csv(crop_capture, file.path(outdir, "alluvial_share_of_basin_by_crop_year.csv"))

metric_long <- region_totals %>%
  select(Region, Year, irrigated_area_ha, withdrawal_m3, implied_depth_m) %>%
  pivot_longer(
    c(irrigated_area_ha, withdrawal_m3, implied_depth_m),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = recode(
      Metric,
      irrigated_area_ha = "Reported irrigated area (ha)",
      withdrawal_m3 = "Withdrawal volume (m³)",
      implied_depth_m = "Implied mean depth (m)"
    )
  )

p_totals <- ggplot(metric_long, aes(Year, Value, color = Region)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3) +
  facet_wrap(~Metric, scales = "free_y", ncol = 1) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "Whole-basin versus alluvial-corridor irrigation records",
    x = NULL, y = NULL, color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

fraction_long <- region_totals %>%
  select(
    Region, Year,
    `Share of total regional area` = irrigated_fraction_region,
    `Share of regional cultivated area` = irrigated_fraction_cultivated
  ) %>%
  pivot_longer(-c(Region, Year), names_to = "Denominator", values_to = "Fraction")

p_fractions <- ggplot(fraction_long, aes(Year, Fraction, color = Region)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3) +
  facet_wrap(~Denominator, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Each irrigation file divided by land area from its own domain",
    x = NULL, y = "Reported irrigated fraction", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

capture_long <- domain_capture %>%
  pivot_longer(-Year, names_to = "Metric", values_to = "Fraction") %>%
  mutate(
    Metric = recode(
      Metric,
      area_capture = "Irrigated-area capture",
      withdrawal_capture = "Withdrawal-volume capture"
    )
  )

p_capture <- ggplot(capture_long, aes(Year, Fraction, color = Metric)) +
  geom_hline(yintercept = 1, color = "grey60", linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Fraction of whole-basin irrigation captured inside the alluvial corridor",
    x = NULL, y = "Alluvial / whole basin", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

crop_mean <- crop_totals %>%
  group_by(Region, Crop) %>%
  summarise(
    mean_irrigated_area_ha = mean(irrigated_area_ha, na.rm = TRUE),
    mean_withdrawal_m3 = mean(withdrawal_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(mean_irrigated_area_ha > 0) %>%
  mutate(Crop = reorder(Crop, mean_irrigated_area_ha, FUN = max))

p_crop <- ggplot(crop_mean, aes(mean_irrigated_area_ha, Crop, color = Region)) +
  geom_point(size = 2.8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "Mean annual reported irrigated area by crop category",
    x = "Irrigated area (ha/year)", y = NULL, color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave(file.path(outdir, "basin_vs_alluvial_totals.png"), p_totals,
       width = 9, height = 9, dpi = 300)
ggsave(file.path(outdir, "basin_vs_alluvial_land_denominators.png"), p_fractions,
       width = 10, height = 5.5, dpi = 300)
ggsave(file.path(outdir, "alluvial_capture_of_basin.png"), p_capture,
       width = 9, height = 5.5, dpi = 300)
ggsave(file.path(outdir, "basin_vs_alluvial_by_crop.png"), p_crop,
       width = 9, height = 6, dpi = 300)

message("Wrote domain comparison to: ", outdir)
