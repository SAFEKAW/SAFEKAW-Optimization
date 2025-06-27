## Submodel_WaterQuality.R
# Author: Sam Zipper
# Date: 6/26/2025
#
# Purpose: This script integrates climate, irrigation, and crop yield data to
#   develop crop-water productivity functions for the region.
#
# Outputs: Function that predicts annual nitrate flux [kg/yr] as a function of the following predictors:
#  - Precipitation (mm)
#  - NLCD Landuse (All developed % (open space, low intensity, medium intensity, high intensity))
#  - NLCD Landuse (Grassland/Herbaceous %)
#  - NLCD Landuse (Pasture/Hay %)
#  - NLCD Landuse (Cultivated Crops %)
#  - Irrigational Water Use
#  - Average Annual Temperature (C)
#  - SPEI6 (annual average)
#  - Precipitation Percentile Change (WWI)
#  - Fertilizer Use IDW (1997-2016) (TonsN)
#
# Differences between my model and Shreya's model:
#  - Land cover: 
#     - Shreya uses interpolated NLCD, I use CDL. This means my model has annual data but shorter time period.
#     - I combined grass/herb/pasture/hay into single predictor
#  - Climate (SPEI6, precip, temperature, whiplash):
#     - I use gridmet from 1979-present, Shreya used PRISM 1979-2022.
#  - Irrigation:
#     - I use surface water + groundwater, Shreya just uses groundwater.
#  - Fertilizer:
#     - I estimate based on EKSRB total land cover and typical fertilizer rates, Shreya gets from county dataset and weights by county

# Set up workspace --------------------------------------------------------

source(file.path("code", "paths+packages.R"))

# load necessary input data ------------------------------------

# load river nitrate data
df_nitrate <- read_csv(file.path("data", "RiverNitrateData_EKSRB.csv"))

# load land cover data
df_landcover <- read_csv(file.path("data", "LandCoverData-CDL_EKSRB.csv"))

# load irrigation data
df_irrigation <- read_csv(file.path("data", "WaterUseData_EKSRB.csv"))

# load climate data
df_climate <- read_csv(file.path("data", "ClimateData_EKSRB.csv"))

# load fertilizer data
df_fertilizer <- read_csv(file.path("data", "FertilizerData_EKSRB.csv"))

# Data transformations ----------------------------------------------------

## land cover: group into developed, grassland/herbaceous, pasture/hay, and cultivated crops
# will combine grassland, herbaceous, pasture, and hay into single group for now
landCoverGroups <- data.frame(
  LandCover = c("Developed",
                "Grass/Pasture/Shrubland",
                "Corn", "Double Crop", "Other Crops", "Sorghum", "Soybeans", "Wheat"),
  LandCoverGroup = c("developed", "grassPastureHay", rep("crop", 6)))

df_landcover_ready <- 
  df_landcover |> 
  left_join(landCoverGroups, by = "LandCover") |> 
  subset(!is.na(LandCoverGroup)) |> 
  group_by(Year, LandCoverGroup) |> 
  summarize(group_area_ha = sum(area_ha)) |> 
  pivot_wider(names_from = LandCoverGroup, names_prefix = "LandCover_ha_", 
              values_from = group_area_ha, values_fill = NA)

## climate: calculate precip, temp, percentile change, and SPEI
#  use gridmet since focus is recent years and need ETr
# step 1: calculate SPEI6 at monthly timescale
library(SPEI)
df_climate_SPEI <- 
  df_climate |> 
  subset(Year >= 1979) |> 
  mutate(Date = ymd(paste0(Year, "-", Month, "-01")),
         Tmean_C_gridmet = (tmmx_C_gridmet + tmmn_C_gridmet) / 2,
         SPEI6 = spei(precip_mm_gridmet - etr_mm_gridmet, 
                      scale = 6, na.rm = TRUE)$fitted)
# ggplot(df_climate_SPEI, aes(x = Date, y = SPEI6)) + geom_line()

# get annual values
df_climate_yr <-
  df_climate_SPEI |> 
  group_by(Year) |> 
  summarize(
    precip_mm = sum(precip_mm_gridmet, na.rm = TRUE),
    Tmean_C = mean(Tmean_C_gridmet, na.rm = TRUE),
    SPEI6_mean = mean(SPEI6, na.rm = TRUE)
  )

# create ecdf for percentile
ecdf_precip <- ecdf(df_climate_yr$precip_mm)

# calculate annual percentile and percentile change
df_climate_yr$precip_percentile <- ecdf_precip(df_climate_yr$precip_mm)
#ggplot(df_climate_yr, aes(x = precip_mm, y = precip_percentile)) + geom_point()
df_climate_yr$precip_percentileChange <- df_climate_yr$precip_percentile - lag(df_climate_yr$precip_percentile, 1)

df_climate_ready <- 
  df_climate_yr |> 
  dplyr::select(-precip_percentile) |> 
  rename(Climate_precip_mm = precip_mm,
         Climate_Tmean_C = Tmean_C,
         Climate_SPEI6_mean = SPEI6_mean,
         Climate_precip_percentileChange = precip_percentileChange)

## irrigation: sum all sources in each year
df_irrigation_ready <- 
  df_irrigation |> 
  subset(Sector == "IRR") |> 
  dplyr::select(Year, waterUse_m3) |> 
  rename(Management_irrigation_m3 = waterUse_m3)

## fertilizer: estimate watershed total based on area of each crop type

df_landCoverFertilizer <-
  df_landcover |> 
  subset(LandCover %in% df_fertilizer$Crop) |> 
  mutate(fertilizer_kgHa = 
           case_when(
             LandCover == "Corn" ~ 250, # rough average of irr and rainfed
             LandCover == "Sorghum" ~ 112,
             LandCover == "Soybeans" ~ 56, # rough average of irr and rainfed
             LandCover == "Wheat" ~ 89,
             TRUE ~ NA_real_),
         fertilizer_kgCrop = fertilizer_kgHa * area_ha)

df_fertilizer_ready <- 
  df_landCoverFertilizer |> 
  group_by(Year) |> 
  summarize(Management_FertilizerUse_kg = sum(fertilizer_kgCrop, na.rm = TRUE)) |> 
  ungroup()

# combine all variables and build model ---------------------------------------------------

df_combined <- 
  df_climate_ready |> 
  left_join(df_landcover_ready, by = "Year") |> 
  left_join(df_irrigation_ready, by = "Year") |> 
  left_join(df_fertilizer_ready, by = c("Year")) |> 
  left_join(df_nitrate, by = "Year") |> 
  # subset to complete cases
  na.omit()

# build MLR model
mlr_nitrate <- lm(NitrateFlux_kg ~ 
                    LandCover_ha_developed + 
                    LandCover_ha_grassPastureHay + 
                    LandCover_ha_crop + 
                    Climate_precip_mm + 
                    Climate_Tmean_C + 
                    Climate_SPEI6_mean + 
                    Climate_precip_percentileChange + 
                    Management_irrigation_m3 + 
                    Management_FertilizerUse_kg, 
                  data = df_combined)

df_combined$NitrateFluxPredicted_kg <- predict(mlr_nitrate, newdata = df_combined)

# compare NitrateFlux_kg, NitrateFluxPredicted_kg, NitrateFluxShreyaPredicted_kg
df_combined |> 
  dplyr::select(Year, NitrateFlux_kg, NitrateFluxPredicted_kg, NitrateFluxShreyaPredicted_kg) |> 
  pivot_longer(-Year,
               names_to = "Variable", 
               values_to = "NitrateFlux_kg") |>
  mutate(Source = case_when(
    Variable == "NitrateFlux_kg" ~ "Observed",
    Variable == "NitrateFluxPredicted_kg" ~ "Optimization MLR",
    Variable == "NitrateFluxShreyaPredicted_kg" ~ "Shreya MLR"
  )) |> 
  ggplot(aes(x = Year, y = NitrateFlux_kg, color = Source)) +
  geom_point() +
  geom_line() +
  scale_color_manual(name = NULL, values = c("black", col.cat.red, col.cat.grn)) +
  labs(y = "Nitrate Flux [kg/yr]") +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.05, 0.95),
        legend.justification = c(0, 1)) +
  guides(color = guide_legend(nrow = 3))
ggsave(file.path("figures", "Submodel_WaterQuality_ComparisonWithShreya.png"),
       width = 120, height = 95, units = "mm")
mae(df_combined$NitrateFluxPredicted_kg, df_combined$NitrateFlux_kg, na.rm = TRUE)
mae(df_combined$NitrateFluxShreyaPredicted_kg, df_combined$NitrateFlux_kg, na.rm = TRUE)

mae(df_combined$NitrateFluxPredicted_kg, df_combined$NitrateFlux_kg, na.rm = TRUE)/(max(df_combined$NitrateFluxPredicted_kg) - min(df_combined$NitrateFluxPredicted_kg))
mae(df_combined$NitrateFluxShreyaPredicted_kg, df_combined$NitrateFlux_kg, na.rm = TRUE)/(max(df_combined$NitrateFluxPredicted_kg) - min(df_combined$NitrateFluxPredicted_kg))