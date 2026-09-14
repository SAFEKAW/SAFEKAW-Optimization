suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(readr)
  library(scales)
  library(tidyr)
})

out_dir <- here(
  "hpc_opt", "outputs", "historical_irrigation_audit", "extent_data"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_water <- function(name) {
  read_csv(here("data", name), show_col_types = FALSE)
}

crop_basin <- read_water("WaterUseByCrop_EKSRB.csv") %>%
  mutate(Domain = "Entire basin")
crop_alluvial <- read_water("WaterUseByCrop_AlluvialCorridor.csv") %>%
  mutate(Domain = "Alluvial corridor")
sector_basin <- read_water("WaterUseData_EKSRB.csv") %>%
  mutate(Domain = "Entire basin")
sector_alluvial <- read_water("WaterUseData_AlluvialCorridor.csv") %>%
  mutate(Domain = "Alluvial corridor")
county_depth <- read_water("WaterUseData_County.csv") %>%
  mutate(FIPS = as.character(FIPS))

crop_all <- bind_rows(crop_basin, crop_alluvial)
sector_all <- bind_rows(sector_basin, sector_alluvial)

inventory <- bind_rows(
  tibble(
    file = c(
      "WaterUseByCrop_EKSRB.csv", "WaterUseByCrop_AlluvialCorridor.csv",
      "WaterUseData_EKSRB.csv", "WaterUseData_AlluvialCorridor.csv",
      "WaterUseData_County.csv"
    ),
    first_year = c(
      min(crop_basin$Year), min(crop_alluvial$Year),
      min(sector_basin$Year), min(sector_alluvial$Year), min(county_depth$Year)
    ),
    last_year = c(
      max(crop_basin$Year), max(crop_alluvial$Year),
      max(sector_basin$Year), max(sector_alluvial$Year), max(county_depth$Year)
    ),
    rows = c(
      nrow(crop_basin), nrow(crop_alluvial), nrow(sector_basin),
      nrow(sector_alluvial), nrow(county_depth)
    )
  )
)

crop_year_totals <- crop_all %>%
  filter(Crop != "Unknown") %>%
  group_by(Domain, Year) %>%
  summarise(
    crop_water_m3 = sum(waterUse_m3, na.rm = TRUE),
    crop_irrigated_area_ha = sum(irrArea_ha, na.rm = TRUE),
    .groups = "drop"
  )

sector_irrigation <- sector_all %>%
  filter(Sector == "IRR") %>%
  transmute(
    Domain, Year,
    sector_irrigation_m3 = waterUse_m3,
    irrigation_gw_m3 = GWuse_m3,
    irrigation_sw_m3 = SWuse_m3,
    gw_fraction = GWuse_m3 / waterUse_m3
  )

reconciliation <- left_join(
  crop_year_totals, sector_irrigation, by = c("Domain", "Year")
) %>%
  mutate(
    crop_minus_sector_m3 = crop_water_m3 - sector_irrigation_m3,
    crop_to_sector_ratio = crop_water_m3 / sector_irrigation_m3
  )

domain_comparison <- crop_basin %>%
  select(Year, Crop, basin_water_m3 = waterUse_m3,
         basin_irrigated_area_ha = irrArea_ha) %>%
  full_join(
    crop_alluvial %>%
      select(Year, Crop, alluvial_water_m3 = waterUse_m3,
             alluvial_irrigated_area_ha = irrArea_ha),
    by = c("Year", "Crop")
  ) %>%
  mutate(
    alluvial_water_fraction = alluvial_water_m3 / basin_water_m3,
    alluvial_area_fraction = alluvial_irrigated_area_ha /
      basin_irrigated_area_ha,
    basin_depth_m = basin_water_m3 / (basin_irrigated_area_ha * 10000),
    alluvial_depth_m = alluvial_water_m3 /
      (alluvial_irrigated_area_ha * 10000)
  )

historical_crop_summary <- domain_comparison %>%
  filter(Year %in% 2006:2023, Crop != "Unknown") %>%
  group_by(Crop) %>%
  summarise(
    years = n_distinct(Year),
    mean_basin_area_ha = mean(basin_irrigated_area_ha, na.rm = TRUE),
    mean_alluvial_area_ha = mean(alluvial_irrigated_area_ha, na.rm = TRUE),
    mean_alluvial_area_fraction = mean(alluvial_area_fraction, na.rm = TRUE),
    mean_basin_water_m3 = mean(basin_water_m3, na.rm = TRUE),
    mean_alluvial_water_m3 = mean(alluvial_water_m3, na.rm = TRUE),
    mean_alluvial_water_fraction = mean(alluvial_water_fraction, na.rm = TRUE),
    mean_basin_depth_m = mean(basin_depth_m, na.rm = TRUE),
    mean_alluvial_depth_m = mean(alluvial_depth_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_basin_area_ha))

county_summary <- county_depth %>%
  filter(Year %in% 2006:2023) %>%
  group_by(Crop) %>%
  summarise(
    county_years = n(),
    counties = n_distinct(FIPS),
    years = n_distinct(Year),
    mean_depth_mm = mean(irrigation_mm_mean, na.rm = TRUE),
    median_depth_mm = median(irrigation_mm_median, na.rm = TRUE),
    total_pdiv_records = sum(n_pdiv, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_pdiv_records))

write_csv(inventory, file.path(out_dir, "file_inventory.csv"))
write_csv(reconciliation, file.path(out_dir, "crop_sector_reconciliation.csv"))
write_csv(domain_comparison, file.path(out_dir, "basin_alluvial_crop_year.csv"))
write_csv(historical_crop_summary, file.path(out_dir, "historical_crop_summary_2006_2023.csv"))
write_csv(county_summary, file.path(out_dir, "county_depth_summary_2006_2023.csv"))

major_crops <- historical_crop_summary %>%
  slice_head(n = 6) %>%
  pull(Crop)

p_area <- crop_all %>%
  filter(Year %in% 2006:2023, Crop %in% major_crops) %>%
  ggplot(aes(Year, irrArea_ha, color = Domain)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Crop, scales = "free_y") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    x = NULL, y = "Reported irrigated area (ha)", color = NULL,
    title = "Irrigated crop area in the basin and alluvial corridor"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "irrigated_area_by_crop.png"), p_area,
       width = 11, height = 7, dpi = 220)

p_share <- domain_comparison %>%
  filter(Year %in% 2006:2023, Crop %in% major_crops) %>%
  select(Year, Crop, alluvial_area_fraction, alluvial_water_fraction) %>%
  pivot_longer(
    starts_with("alluvial_"), names_to = "Measure", values_to = "Fraction"
  ) %>%
  mutate(Measure = recode(
    Measure,
    alluvial_area_fraction = "Irrigated area",
    alluvial_water_fraction = "Water volume"
  )) %>%
  ggplot(aes(Year, Fraction, color = Measure)) +
  geom_hline(yintercept = 1, color = "grey75", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Crop) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    x = NULL, y = "Alluvial share of basin total", color = NULL,
    title = "Concentration of crop irrigation in the alluvial corridor"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "alluvial_share_by_crop.png"), p_share,
       width = 11, height = 7, dpi = 220)

p_reconcile <- reconciliation %>%
  filter(Year %in% 2006:2023) %>%
  ggplot(aes(Year)) +
  geom_line(aes(y = sector_irrigation_m3, color = "Sector IRR total"),
            linewidth = 0.9) +
  geom_line(aes(y = crop_water_m3, color = "Sum of crop records"),
            linewidth = 0.9, linetype = "dashed") +
  facet_wrap(~Domain, scales = "free_y") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    x = NULL, y = "Annual irrigation water (million m3)", color = NULL,
    title = "Crop-specific and sector irrigation totals"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "crop_sector_reconciliation.png"), p_reconcile,
       width = 10, height = 5.5, dpi = 220)

print(inventory, n = Inf)
print(historical_crop_summary, n = Inf)
print(
  reconciliation %>%
    filter(Year %in% 2006:2023) %>%
    group_by(Domain) %>%
    summarise(
      mean_sector_irrigation_m3 = mean(sector_irrigation_m3),
      mean_crop_water_m3 = mean(crop_water_m3),
      mean_crop_to_sector_ratio = mean(crop_to_sector_ratio),
      mean_gw_fraction = mean(gw_fraction),
      .groups = "drop"
    ),
  n = Inf
)
print(county_summary, n = Inf)
