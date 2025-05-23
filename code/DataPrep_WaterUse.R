## DataPrep_WaterUse.R
# Author: Sam Zipper
# Date: 5/23/2025
#
# Purpose: This script processes annual water use data from WIMAS to create three 
#   output CSV files.
#    County CSV file (applied irrigation by crop): WaterUseData_County.csv
#    | Year | FIPS | Crop | WaterUse_m3 | CropArea_ha
#    
#    EKSRB CSV file (total water use by sector/source): WaterUseData_EKSRB.csv
#    | Year | WaterUse_Total_m3 | WaterUse_Irrigation_m3 | {other sectors} | PercentGW_Total | PercentGW_Irrigation | {other sectors}
#    
#    Alluvial Corridor CSV file (total water use by sector and source): WaterUseData_AlluvialCorridor.csv
#    | Year | WaterUse_Total_m3 | WaterUse_Irrigation_m3 | {other sectors} | PercentGW_Total | PercentGW_Irrigation | {other sectors} | PercentGW_Total | PercentGW_Irrigation | {other sectors}
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
  # get rid of missing lat/long
  filter(!is.na(lat) & !is.na(lon)) |>
  # subset to years of interest
  filter(year >= 1990 & year <= 2023)

# Bowersock is pdiv_id 76439

# convert to sf using lat/long
sf_wimas_in <- 
  df_wimas_in %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(sf_watershed))

# plot and inspect
ggplot() +
  geom_sf(data = sf_watershed) +
  geom_sf(data = sf_wimas_in, color = col.cat.red)

# Process for watershed ---------------------------------------------------

# subset sf_wimas to any wells within the watershed
sf_wimas_watershed <- 
  st_filter(sf_wimas_in, sf_watershed)

# plot and inspect
ggplot() +
  geom_sf(data = sf_watershed) +
  geom_sf(data = sf_wimas_watershed, color = col.cat.red)

# calculate total water use by year, use, and source
df_wimas_watershedTotal <- 
  sf_wimas_watershed %>%
  group_by(year, use, source) %>%
  summarise(
    WaterUse_Total_m3 = sum(amt * 1233.4818375475, na.rm = TRUE),
    .groups = "drop"
  )
