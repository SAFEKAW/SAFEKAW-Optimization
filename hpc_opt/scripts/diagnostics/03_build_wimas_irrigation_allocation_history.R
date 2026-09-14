suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(readr)
  library(sf)
  library(tidyr)
})

out_dir <- here(
  "hpc_opt", "outputs", "historical_irrigation_audit", "wimas_allocation"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

af_to_m3 <- 1233.4818375475
acre_to_ha <- 0.404686

crop_name <- function(code) {
  code <- suppressWarnings(as.numeric(code))
  case_when(
    code == 1 ~ "Alfalfa",
    code == 2 ~ "Corn",
    code == 3 ~ "Sorghum",
    code == 4 ~ "Soybeans",
    code == 5 ~ "Wheat",
    code %in% c(6:10, 12:15, 77, 78) ~ "Other Crop",
    code %in% 16:74 ~ "Multi-Crop",
    code %in% c(11, 76) ~ "Golf Course/Turf/Sod",
    code == 75 ~ "Pasture",
    TRUE ~ "Unknown"
  )
}

raw <- read_csv(
  here("data", "raw", "wimas", "wimas_eksrb_1957_2023.csv"),
  show_col_types = FALSE,
  col_types = cols(crop_code = col_character())
) %>%
  filter(
    year %in% 1990:2023,
    use == "IRR",
    is.finite(amt), amt > 0,
    is.finite(lat), is.finite(lon)
  )

# The downloaded file is already unique by pdiv/year/source/use. Assert that
# property so water volume and irrigated acreage cannot be silently duplicated.
duplicate_keys <- raw %>%
  count(pdiv_id, year, source, use) %>%
  filter(n > 1)
if (nrow(duplicate_keys) > 0) {
  stop("Raw WIMAS file has duplicate pdiv/year/source/use records.")
}

watershed <- st_read(
  here("data", "Boundary_EKSRBwatershed.gpkg"), quiet = TRUE
)
alluvial <- st_read(
  here("data", "Boundary_EKSRBalluvialCorridor.gpkg"), quiet = TRUE
)
counties <- st_read(
  here("data", "Boundary_EKSRBcounties.gpkg"), quiet = TRUE
) %>%
  select(FIPS, County = name)

points <- raw %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(watershed))

in_watershed <- lengths(st_within(points, watershed)) > 0
points <- points[in_watershed, ]
points$Alluvial <- lengths(st_within(points, alluvial)) > 0
points <- st_join(points, counties, join = st_within, left = TRUE)

if (any(is.na(points$FIPS))) {
  stop(sum(is.na(points$FIPS)), " irrigation records lack a county FIPS.")
}

irrigation_records <- points %>%
  st_drop_geometry() %>%
  transmute(
    Year = year,
    FIPS = as.character(FIPS),
    County,
    Crop = crop_name(crop_code),
    Domain = if_else(Alluvial, "Alluvial", "Non-alluvial"),
    Source = recode(source, G = "Groundwater", S = "Surface water",
                    .default = as.character(source)),
    pdiv_id,
    waterUse_m3 = amt * af_to_m3,
    irrArea_ha = if_else(
      is.finite(acres_irr) & acres_irr > 0,
      acres_irr * acre_to_ha,
      NA_real_
    )
  )

county_crop_year <- irrigation_records %>%
  group_by(Year, FIPS, County, Crop, Domain) %>%
  summarise(
    GWuse_m3 = sum(waterUse_m3[Source == "Groundwater"]),
    SWuse_m3 = sum(waterUse_m3[Source == "Surface water"]),
    n_pdiv = n_distinct(pdiv_id),
    n_missing_irrArea = sum(is.na(irrArea_ha)),
    water_with_area_m3 = sum(waterUse_m3[is.finite(irrArea_ha)]),
    irrArea_ha = sum(irrArea_ha, na.rm = TRUE),
    waterUse_m3 = sum(waterUse_m3),
    .groups = "drop"
  ) %>%
  mutate(
    area_data_water_fraction = water_with_area_m3 / waterUse_m3,
    applied_depth_m = if_else(
      irrArea_ha > 0, waterUse_m3 / (irrArea_ha * 10000), NA_real_
    )
  )

crop_year_domain <- county_crop_year %>%
  group_by(Year, Crop, Domain) %>%
  summarise(
    waterUse_m3 = sum(waterUse_m3),
    GWuse_m3 = sum(GWuse_m3),
    SWuse_m3 = sum(SWuse_m3),
    irrArea_ha = sum(irrArea_ha),
    n_pdiv = sum(n_pdiv),
    water_with_area_m3 = sum(water_with_area_m3),
    .groups = "drop"
  ) %>%
  mutate(
    area_data_water_fraction = water_with_area_m3 / waterUse_m3,
    applied_depth_m = if_else(
      irrArea_ha > 0, waterUse_m3 / (irrArea_ha * 10000), NA_real_
    )
  )

crop_year_basin <- crop_year_domain %>%
  group_by(Year, Crop) %>%
  summarise(
    waterUse_m3 = sum(waterUse_m3),
    GWuse_m3 = sum(GWuse_m3),
    SWuse_m3 = sum(SWuse_m3),
    irrArea_ha = sum(irrArea_ha),
    n_pdiv = sum(n_pdiv),
    water_with_area_m3 = sum(water_with_area_m3),
    .groups = "drop"
  ) %>%
  mutate(
    area_data_water_fraction = water_with_area_m3 / waterUse_m3,
    applied_depth_m = if_else(
      irrArea_ha > 0, waterUse_m3 / (irrArea_ha * 10000), NA_real_
    )
  )

sector_reference <- read_csv(
  here("data", "WaterUseData_EKSRB.csv"), show_col_types = FALSE
) %>%
  filter(Sector == "IRR", Year %in% 1990:2023) %>%
  select(Year, sector_waterUse_m3 = waterUse_m3,
         sector_GWuse_m3 = GWuse_m3, sector_SWuse_m3 = SWuse_m3)

reconciliation <- crop_year_basin %>%
  group_by(Year) %>%
  summarise(
    raw_waterUse_m3 = sum(waterUse_m3),
    raw_GWuse_m3 = sum(GWuse_m3),
    raw_SWuse_m3 = sum(SWuse_m3),
    .groups = "drop"
  ) %>%
  left_join(sector_reference, by = "Year") %>%
  mutate(
    water_difference_m3 = raw_waterUse_m3 - sector_waterUse_m3,
    relative_difference = water_difference_m3 / sector_waterUse_m3
  )

allocation_annual <- county_crop_year %>%
  filter(Year %in% 2006:2023, Crop %in% c("Corn", "Soybeans")) %>%
  complete(
    Year = 2006:2023,
    nesting(Crop, Domain, FIPS, County),
    fill = list(
      waterUse_m3 = 0, GWuse_m3 = 0, SWuse_m3 = 0, irrArea_ha = 0,
      n_pdiv = 0, n_missing_irrArea = 0, water_with_area_m3 = 0
    )
  ) %>%
  group_by(Year, Crop, Domain) %>%
  mutate(annual_domain_area_share = if (sum(irrArea_ha) > 0) {
    irrArea_ha / sum(irrArea_ha)
  } else {
    rep(0, n())
  }) %>%
  ungroup()

allocation_summary <- allocation_annual %>%
  group_by(Crop, Domain, FIPS, County) %>%
  summarise(
    years_active = sum(irrArea_ha > 0),
    mean_irrigated_area_ha = mean(irrArea_ha),
    mean_waterUse_m3 = mean(waterUse_m3),
    mean_applied_depth_m = weighted.mean(
      waterUse_m3 / if_else(irrArea_ha > 0, irrArea_ha * 10000, NA_real_),
      w = irrArea_ha, na.rm = TRUE
    ),
    total_pdiv_years = sum(n_pdiv),
    mean_annual_domain_area_share = mean(annual_domain_area_share),
    sd_annual_domain_area_share = sd(annual_domain_area_share),
    .groups = "drop"
  ) %>%
  group_by(Crop, Domain) %>%
  mutate(
    historical_area_share = mean_irrigated_area_ha /
      sum(mean_irrigated_area_ha)
  ) %>%
  ungroup()

groundwater_county_annual <- irrigation_records %>%
  filter(
    Year %in% 2006:2023, Crop %in% c("Corn", "Soybeans"),
    Source == "Groundwater"
  ) %>%
  group_by(Year, Crop, Domain, FIPS, County) %>%
  summarise(
    groundwater_irrigated_area_ha = sum(irrArea_ha, na.rm = TRUE),
    groundwater_withdrawal_m3 = sum(waterUse_m3),
    n_pdiv = n_distinct(pdiv_id),
    .groups = "drop"
  ) %>%
  complete(
    Year = 2006:2023,
    nesting(Crop, Domain, FIPS, County),
    fill = list(
      groundwater_irrigated_area_ha = 0,
      groundwater_withdrawal_m3 = 0,
      n_pdiv = 0
    )
  )

groundwater_allocation_summary <- groundwater_county_annual %>%
  group_by(Crop, Domain, FIPS, County) %>%
  summarise(
    years_active = sum(groundwater_irrigated_area_ha > 0),
    mean_groundwater_irrigated_area_ha =
      mean(groundwater_irrigated_area_ha),
    mean_groundwater_withdrawal_m3 = mean(groundwater_withdrawal_m3),
    mean_groundwater_depth_m = weighted.mean(
      groundwater_withdrawal_m3 /
        if_else(
          groundwater_irrigated_area_ha > 0,
          groundwater_irrigated_area_ha * 10000,
          NA_real_
        ),
      w = groundwater_irrigated_area_ha,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  group_by(Crop, Domain) %>%
  mutate(
    historical_groundwater_area_share =
      mean_groundwater_irrigated_area_ha /
      sum(mean_groundwater_irrigated_area_ha)
  ) %>%
  ungroup()

write_csv(irrigation_records, file.path(out_dir, "irrigation_records_1990_2023.csv"))
write_csv(county_crop_year, file.path(out_dir, "county_crop_year_irrigation.csv"))
write_csv(crop_year_domain, file.path(out_dir, "crop_year_domain_irrigation.csv"))
write_csv(crop_year_basin, file.path(out_dir, "crop_year_basin_irrigation.csv"))
write_csv(reconciliation, file.path(out_dir, "sector_reconciliation.csv"))
write_csv(allocation_annual, file.path(out_dir, "county_allocation_annual_2006_2023.csv"))
write_csv(allocation_summary, file.path(out_dir, "county_allocation_summary_2006_2023.csv"))
write_csv(
  groundwater_county_annual,
  file.path(out_dir, "county_groundwater_irrigation_annual_2006_2023.csv")
)
write_csv(
  groundwater_allocation_summary,
  file.path(out_dir, "county_groundwater_allocation_summary_2006_2023.csv")
)

p_county <- allocation_summary %>%
  mutate(County = reorder(County, mean_irrigated_area_ha, sum)) %>%
  ggplot(aes(mean_irrigated_area_ha, County, fill = Domain)) +
  geom_col() +
  facet_wrap(~Crop, scales = "free_x") +
  scale_fill_manual(values = c(
    "Alluvial" = "#0072B2", "Non-alluvial" = "#E69F00"
  )) +
  labs(
    x = "Mean annual irrigated area (ha)", y = NULL, fill = NULL,
    title = "Historical corn and soybean irrigation by county",
    subtitle = "2006 to 2023; inactive county-years included as zero"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(
  file.path(out_dir, "county_irrigated_area_2006_2023.png"),
  p_county, width = 10, height = 7, dpi = 220
)

p_groundwater_county <- groundwater_allocation_summary %>%
  mutate(County = reorder(County, mean_groundwater_irrigated_area_ha, sum)) %>%
  ggplot(aes(mean_groundwater_irrigated_area_ha, County, fill = Domain)) +
  geom_col() +
  facet_wrap(~Crop, scales = "free_x") +
  scale_fill_manual(values = c(
    "Alluvial" = "#0072B2", "Non-alluvial" = "#E69F00"
  )) +
  labs(
    x = "Mean annual groundwater-irrigated area (ha)",
    y = NULL, fill = NULL,
    title = "Historical groundwater irrigation by county",
    subtitle = "2006 to 2023; inactive county-years included as zero"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(
  file.path(out_dir, "county_groundwater_irrigated_area_2006_2023.png"),
  p_groundwater_county, width = 10, height = 7, dpi = 220
)

print(
  reconciliation %>% summarise(
    years = n(),
    max_abs_relative_difference = max(abs(relative_difference), na.rm = TRUE),
    mean_abs_relative_difference = mean(abs(relative_difference), na.rm = TRUE)
  )
)
print(
  crop_year_domain %>%
    filter(Year %in% 2006:2023, Crop %in% c("Corn", "Soybeans")) %>%
    group_by(Crop, Domain) %>%
    summarise(
      mean_area_ha = mean(irrArea_ha),
      mean_water_m3 = mean(waterUse_m3),
      mean_depth_m = weighted.mean(applied_depth_m, irrArea_ha, na.rm = TRUE),
      mean_area_data_water_fraction = mean(area_data_water_fraction),
      .groups = "drop"
    ),
  n = Inf
)
