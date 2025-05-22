## DataPrep_CropYield.R
# Author: Sam Zipper
# Date: 5/22/2025
#
# Purpose: This script summarizes historic crop yield data at county resolution.
# 
# Output is a county CSV file: CropYieldData_County.csv
# | Year | County | Crop | WaterManagement | yield | yield_units
# *WaterManagement = “Irrigated” or “Rainfed”
#
# Output is saved in `data` folder.

# Set up workspace --------------------------------------------------------

source(file.path("code", "paths+packages.R"))

# load counties and yield data ------------------------------------

sf_counties <- st_read(file.path("data", "Boundary_EKSRBcounties.gpkg"))
df_yield_raw <- read_xlsx(file.path("data", "raw", "YieldCorrelationTool_2025.xlsx"),
                          sheet = "data", skip = 1)

# raw yield data
#  Column I: County
#  Column C: Crop
#  Column K: Irrigation practice
#  Column X: Year
#  Column U: Yield

# process yield data ------------------------------------

# basic cleaning
df_yield_out <-
  df_yield_raw |> 
  # trim to counties of interest
  mutate(FIPS = paste0(`State Code`, `County Code`)) |> 
  subset(FIPS %in% sf_counties$FIPS) |> 
  # rename columns
  rename(all_of(c(
    Crop = "Commodity Name",
    Year = "Yield Year...24",
    WaterManagement = "Irrigation Practice Name",
    yield_buAc = "Yield Amount"
  ))) |> 
  # convert to kg/ha
  left_join(yield_conversion, by = "Crop") |> 
  mutate(yield_kgHa = yield_buAc*kg_per_bushel/ha_per_ac) |> 
  # trim to needed columns
  dplyr::select(Year, FIPS, Crop, WaterManagement, yield_kgHa)

# visualize and save ------------------------------------------------------

ggplot(df_yield_out, aes(x = yield_kgHa)) +
  geom_histogram() + 
  facet_grid(WaterManagement~Crop, scales = "free_x")

write_csv(df_yield_out, file.path("data", "CropYieldData_County.csv"))
