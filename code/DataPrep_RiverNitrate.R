## DataPrep_RiverNitrate.R
# Author: Sam Zipper
# Date: 6/26/2025
#
# This script prepares the nitrate data from Shreya for use in the water quality model.
# 
# Output: CSV file with following fields:
#  - Year
#  - NitrateFlux_kg = total annual nitrate flux, estimated from Shreya's WRTDS model
#  - NitrateFlux_mgL = average annual nitrate concentration, estimated from Shreya's WRTDS model
#  - NitrateFluxShreyaPredicted_kg = total annual nitrate flux predicted by Shreya's MLR model

# Output is saved in `data` folder.

# Set up workspace --------------------------------------------------------

source(file.path("code", "paths+packages.R"))

# load data
df_in <- read_xlsx(file.path(path_data, "MLRdataFromShreya", "Current MLR Data.xlsx"), 
                   sheet = "Sheet1")

# # build MLR test model to confirm I reproduce Shreya's model
# mlr_shreya <- lm(`Total Annual Flux WRTDS output (kg/year)` ~ 
#                    `Precipitation (mm)` + 
#                    `NLCD Landuse (All developed % (open space, low intensity, medium intensity, high intensity))` +
#                    `NLCD Landuse (Grassland/Herbaceous %)` +
#                    `NLCD Landuse (Pasture/Hay %)` + 
#                    `NLCD Landuse (Cultivated Crops %)` +
#                    `Irrigational Water Use` +
#                    `Average Annual Temperature (C)` +
#                    `SPEI6 (annual average)` +
#                    `Precipitation Percentile Change (WWI)` +
#                    `Fertilizer Use IDW (1997-2016) (TonsN)`, 
#                  data = df_in)
# df_in$NitrateFluxPredictedBySam_kg <- predict(mlr_shreya, newdata = df_in)

# select columns, rename, save
df_in |> 
  rename(NitrateFlux_kg = `Total Annual Flux WRTDS output (kg/year)`,
         NitrateFlux_mgL = `Total Annual Concentration WRTDS output (mg/L)`,
         NitrateFluxShreyaPredicted_kg = `MLR Model Predicted Annual Flux (kg/year)`) |>
  dplyr::select(Year, NitrateFlux_kg, NitrateFlux_mgL, NitrateFluxShreyaPredicted_kg) |> 
  write_csv(file.path("data", "RiverNitrateData_EKSRB.csv"))

