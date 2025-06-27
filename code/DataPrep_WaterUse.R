## DataPrep_WaterUse.R
# Author: Sam Zipper
# Date: 5/23/2025
#
# Purpose: This script processes annual water use data from WIMAS to create three 
#   output CSV files.
#    County CSV file (applied irrigation by crop): WaterUseData_County.csv
#    | Year | FIPS | Crop | irrigation_mm_mean | irrigation_mm_median | irrigation_mm_p5| … | n_pdiv
#    
#    EKSRB CSV file (total water use by sector/source): WaterUseData_EKSRB.csv
#    | Year | Sector | waterUse_m3 | GWuse_m3 | SWuse_m3 |
#    
#    Alluvial Corridor CSV file (total water use by sector and source): WaterUseData_AlluvialCorridor.csv
#    | Year | Sector | waterUse_m3 | GWuse_m3 | SWuse_m3 |
#
# Subdomain boundaries are created in `DataPrep_Boundaries.R` script and saved in `data` folder.
# 
# Output is saved in `data` folder.

# Set up workspace --------------------------------------------------------

source(file.path("code", "paths+packages.R"))

# load boundaries ------------------------------------

sf_corridor <- st_read(file.path("data", "Boundary_EKSRBalluvialCorridor.gpkg"))
sf_counties <- st_read(file.path("data", "Boundary_EKSRBcounties.gpkg"))
sf_watershed <- st_read(file.path("data", "Boundary_EKSRBwatershed.gpkg"))

# load WIMAS data ---------------------------------------------------------

# WIMAS data was downloaded by Camden and includes everything within the bounding
# box defined by the EKSRB watershed (so includes some data beyond the EKSRB extent)
df_wimas_in <- 
  read_csv(file.path(path_data, "Water", "WaterUse", "wimas_eksrb_1957_2023.csv")) |> 
  # subset to years of interest
  filter(year >= 1990 & year <= 2023)

# convert to sf using lat/long
sf_wimas_in <- 
  df_wimas_in %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(sf_watershed))

# plot and inspect
ggplot() +
  geom_sf(data = sf_counties) +
  geom_sf(data = sf_watershed, fill = NA, color = col.cat.blu) +
  geom_sf(data = sf_wimas_in, color = col.cat.red)

# exclude WTR (enormous) and categories representing <1% of total use
# sum total water use and water use by sector for each year
df_wimas_in_total <- 
  df_wimas_in |> 
  subset(use != "WTR") |> 
  group_by(year) |> 
  summarise(
    WaterUse_Total_m3 = sum(amt * 1233.4818375475, na.rm = TRUE),
    .groups = "drop"
  )

df_wimas_in_sector <- 
  df_wimas_in |> 
  subset(use != "WTR") |> 
  group_by(year, use) |> 
  summarise(
    WaterUse_Sector_m3 = sum(amt * 1233.4818375475, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  left_join(df_wimas_in_total, by = "year") |> 
  mutate(WaterUse_Sector_prc = WaterUse_Sector_m3 / WaterUse_Total_m3)

ggplot(df_wimas_in_sector, aes(x = WaterUse_Sector_prc*100)) +
  geom_histogram() +
  facet_wrap(~use, scales = "free_x")

# exclude uses
use_to_exclude <- c("WTR", "AR", "CON", "DEW", "DOM", "FPR", "HYD", "SED", "STK", "THX")

# make trimmed version of df_wimas_in
sf_wimas_trim <- subset(sf_wimas_in, !use %in% use_to_exclude)

# Process for watershed ---------------------------------------------------

# subset sf_wimas to any wells within the watershed
sf_wimas_watershed <- 
  st_filter(sf_wimas_trim, sf_watershed)

# plot and inspect
ggplot() +
  geom_sf(data = sf_watershed) +
  geom_sf(data = sf_wimas_watershed, color = col.cat.red)

# calculate total water use by year, use, and source
df_wimas_watershedTotal <- 
  sf_wimas_watershed |> 
  st_drop_geometry() |> 
  group_by(year, use, source) |> 
  summarise(
    WaterUse_Total_m3 = sum(amt * 1233.4818375475, na.rm = TRUE),
    .groups = "drop"
  )

# plot to inspect
ggplot(df_wimas_watershedTotal, aes(x = year, y = WaterUse_Total_m3, fill = source)) +
  geom_col(position = "stack") +
  facet_wrap(~ use, scales = "free_y")

# calculate total water use in each year by sector
df_wimas_watershedSector <- 
  df_wimas_watershedTotal |> 
  group_by(year, use) |> 
  summarise(
    waterUse_m3 = sum(WaterUse_Total_m3, na.rm = TRUE),
    .groups = "drop"
  )

# calculate groundwater use in each year by sector
df_wimas_watershedSectorGW <- 
  df_wimas_watershedTotal |> 
  subset(source == "G") |>
  group_by(year, use) |> 
  summarise(
    GWuse_m3 = sum(WaterUse_Total_m3, na.rm = TRUE),
    .groups = "drop"
  )

# calculate surface water use in each year by sector
df_wimas_watershedSectorSW <- 
  df_wimas_watershedTotal |> 
  subset(source == "S") |>
  group_by(year, use) |> 
  summarise(
    SWuse_m3 = sum(WaterUse_Total_m3, na.rm = TRUE),
    .groups = "drop"
  )

# join
df_wimas_watershed_out <-
  left_join(df_wimas_watershedSector, df_wimas_watershedSectorGW, by = c("year", "use")) |> 
  left_join(df_wimas_watershedSectorSW, by = c("year", "use")) |>
  rename(all_of(c("Year" = "year",
                  "Sector" = "use")))

write_csv(df_wimas_watershed_out, file.path("data", "WaterUseData_EKSRB.csv"))

# within irrigation water use: calculate annual total by crop type
#  crop codes are here: https://geohydro.kgs.ku.edu/geohydro/ofr/2005_30/wimas_ofr2005_30_manual.htm
df_wimas_watershed_byCrop <-
  sf_wimas_watershed |> 
  st_drop_geometry() |> 
  filter(is.finite(amt) & amt > 0) |> 
  mutate(crop_code = as.numeric(crop_code)) |> 
  mutate(Crop = case_when(
    crop_code == 0 | is.na(crop_code) ~ "Unknown",
    crop_code == 1 ~ "Alfalfa",
    crop_code == 2 ~ "Corn",
    crop_code == 3 ~ "Sorghum",
    crop_code == 4 ~ "Soybeans",
    crop_code == 5 ~ "Wheat",
    crop_code %in% c(seq(6, 10), 12, 13, 14, 15, 77, 78) ~ "Other Crop",
    crop_code %in% seq(16, 74) ~ "Multi-Crop",
    crop_code %in% c(11, 76) ~ "Golf Course/Turf/Sod",
    crop_code == 75 ~ "Pasture"
  )) |> 
  group_by(year, Crop) |> 
  summarize(waterUse_m3 = sum(amt * 1233.4818375475, na.rm = TRUE),
            n_waterUse_NA = sum(is.na(amt)),
            irrArea_ha = sum(acres_irr * ha_per_ac, na.rm = TRUE),
            n_irrArea_NA = sum(is.na(acres_irr))) |> 
  rename(Year = year)

ggplot(df_wimas_watershed_byCrop, aes(x = Year, y = waterUse_m3)) +
  geom_col() + facet_wrap(~Crop)
ggplot(df_wimas_watershed_byCrop, aes(x = Year, y = irrArea_ha)) +
  geom_col() + facet_wrap(~Crop)

write_csv(df_wimas_watershed_byCrop, file.path("data", "WaterUseByCrop_EKSRB.csv"))

# Process for corridor ---------------------------------------------------

# subset sf_wimas to any wells within the corridor
sf_wimas_corridor <- 
  st_filter(sf_wimas_trim, sf_corridor)

# plot and inspect
ggplot() +
  geom_sf(data = sf_corridor) +
  geom_sf(data = sf_wimas_corridor, color = col.cat.red)

# calculate total water use by year, use, and source
df_wimas_corridorTotal <- 
  sf_wimas_corridor |> 
  st_drop_geometry() |> 
  group_by(year, use, source) |> 
  summarise(
    WaterUse_Total_m3 = sum(amt * 1233.4818375475, na.rm = TRUE),
    .groups = "drop"
  )

# plot to inspect
ggplot(df_wimas_corridorTotal, aes(x = year, y = WaterUse_Total_m3, fill = source)) +
  geom_col(position = "stack") +
  facet_wrap(~ use, scales = "free_y")

# calculate total water use in each year by sector
df_wimas_corridorSector <- 
  df_wimas_corridorTotal |> 
  group_by(year, use) |> 
  summarise(
    waterUse_m3 = sum(WaterUse_Total_m3, na.rm = TRUE),
    .groups = "drop"
  )

# calculate groundwater use in each year by sector
df_wimas_corridorSectorGW <- 
  df_wimas_corridorTotal |> 
  subset(source == "G") |>
  group_by(year, use) |> 
  summarise(
    GWuse_m3 = sum(WaterUse_Total_m3, na.rm = TRUE),
    .groups = "drop"
  )

# calculate surface water use in each year by sector
df_wimas_corridorSectorSW <- 
  df_wimas_corridorTotal |> 
  subset(source == "S") |>
  group_by(year, use) |> 
  summarise(
    SWuse_m3 = sum(WaterUse_Total_m3, na.rm = TRUE),
    .groups = "drop"
  )

# join
df_wimas_corridor_out <-
  left_join(df_wimas_corridorSector, df_wimas_corridorSectorGW, by = c("year", "use")) |> 
  left_join(df_wimas_corridorSectorSW, by = c("year", "use")) |>
  rename(all_of(c("Year" = "year",
                  "Sector" = "use")))

write_csv(df_wimas_corridor_out, file.path("data", "WaterUseData_AlluvialCorridor.csv"))


# within irrigation water use: calculate annual total by crop type
#  crop codes are here: https://geohydro.kgs.ku.edu/geohydro/ofr/2005_30/wimas_ofr2005_30_manual.htm
df_wimas_corridor_byCrop <-
  sf_wimas_corridor |> 
  st_drop_geometry() |> 
  filter(is.finite(amt) & amt > 0) |> 
  mutate(crop_code = as.numeric(crop_code)) |> 
  mutate(Crop = case_when(
    crop_code == 0 | is.na(crop_code) ~ "Unknown",
    crop_code == 1 ~ "Alfalfa",
    crop_code == 2 ~ "Corn",
    crop_code == 3 ~ "Sorghum",
    crop_code == 4 ~ "Soybeans",
    crop_code == 5 ~ "Wheat",
    crop_code %in% c(seq(6, 10), 12, 13, 14, 15, 77, 78) ~ "Other Crop",
    crop_code %in% seq(16, 74) ~ "Multi-Crop",
    crop_code %in% c(11, 76) ~ "Golf Course/Turf/Sod",
    crop_code == 75 ~ "Pasture"
  )) |> 
  group_by(year, Crop) |> 
  summarize(waterUse_m3 = sum(amt * 1233.4818375475, na.rm = TRUE),
            n_waterUse_NA = sum(is.na(amt)),
            irrArea_ha = sum(acres_irr * ha_per_ac, na.rm = TRUE),
            n_irrArea_NA = sum(is.na(acres_irr))) |> 
  rename(Year = year)

ggplot(df_wimas_corridor_byCrop, aes(x = Year, y = waterUse_m3)) +
  geom_col() + facet_wrap(~Crop)
ggplot(df_wimas_corridor_byCrop, aes(x = Year, y = irrArea_ha)) +
  geom_col() + facet_wrap(~Crop)

write_csv(df_wimas_corridor_byCrop, file.path("data", "WaterUseByCrop_AlluvialCorridor.csv"))


# Process for counties ----------------------------------------------------

# in sf_wimas, subset to IRR and add a column for FIPS
sf_wimas_counties <- 
  sf_wimas_trim |> 
  # trim to irrigation
  subset(use == "IRR") |> 
  st_join(sf_counties, join = st_within) |> 
  # remove any points that don't fall within a county
  filter(!is.na(FIPS)) |> 
  # remove anything with NA or 0 for water use or irrigated acres
  mutate(crop_code = as.numeric(crop_code)) |>
  filter(is.finite(amt) & amt > 0 &
           is.finite(acres_irr) & acres_irr > 0)

ggplot() + 
  geom_sf(data = sf_counties) +
  geom_sf(data = sf_wimas_counties, aes(color = FIPS))

# inspect crop codes
sf_wimas_counties |> 
  st_drop_geometry() |> 
  group_by(crop_code) |> 
  summarize(count = n()) |> 
  arrange(-count)

# frequent crops
#  2 = corn
#  4 = soybeans
#  24 = corn & soybeans
#  11 = golf course
#  16 = more than one crop
#  15 = other
# other crops of interest
#  3 = grain sorghum
#  5 = wheat

focus_crops <- c(2, 3, 4, 5)
df_cropNames <- data.frame(
  crop_code = c(2, 3, 4, 5),
  Crop = c("Corn", "Sorghum", "Soybeans", "Wheat")
  )

# calculate applied irrigation for each point
df_wimas_counties_points <-
  sf_wimas_counties |> 
  st_drop_geometry() |> 
  left_join(df_cropNames, by = "crop_code") |> 
  # filter to focus crops
  filter(crop_code %in% focus_crops) |> 
  # irrigated area screening following Obembe et al (2023)
  filter(acres_irr >= 40 & acres_irr <= 500) |> 
  # unit conversions
  mutate(irrigation_m3 = amt * 1233.4818375475,
         cropArea_ha = acres_irr * 0.404686,
         irrigation_mm = 1000 * irrigation_m3 / (cropArea_ha * 10000))

# for each year, crop_code, and FIPS: calculate the mean, median, p25, p75, p5, and p95 irrigation_mm
df_wimas_counties_out <- 
  df_wimas_counties_points |> 
  group_by(year, Crop, FIPS) |> 
  summarise(
    irrigation_mm_mean = mean(irrigation_mm, na.rm = TRUE),
    irrigation_mm_median = median(irrigation_mm, na.rm = TRUE),
    irrigation_mm_stdev = sd(irrigation_mm, na.rm = TRUE),
    irrigation_mm_p5 = quantile(irrigation_mm, 0.05, na.rm = TRUE),
    irrigation_mm_p25 = quantile(irrigation_mm, 0.25, na.rm = TRUE),
    irrigation_mm_p75 = quantile(irrigation_mm, 0.75, na.rm = TRUE),
    irrigation_mm_p95 = quantile(irrigation_mm, 0.95, na.rm = TRUE),
    n_pdiv = n(),
    .groups = "drop"
  ) |> 
  rename(Year = year)

# plot to inspect
ggplot(df_wimas_counties_points, aes(x = year, y = irrigation_mm, group = year)) +
  geom_boxplot() + facet_wrap(~Crop)

# save output
write_csv(df_wimas_counties_out, file.path("data", "WaterUseData_County.csv"))
          