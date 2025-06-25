## Submodel_CropWater.R
# Author: Sam Zipper
# Date: 6/19/2025
#
# Purpose: This script integrates climate, irrigation, and crop yield data to
#   develop crop-water productivity functions for the region.
# 
# Outputs:
#  - Function that calculates crop yield [cal/ha] and irrigation water use [mm/ha]
#    as a function of precipitation for each crop and water management system.
#  - Graphs of crop-water productivity function for validation.
#
# Output is saved in `data` folder.

# Set up workspace --------------------------------------------------------

source(file.path("code", "paths+packages.R"))

# load necessary input data ------------------------------------

# load raw data and trim to common period of data (1990-2023)
yrs_common <- 1990:2023

df_yield <- read_csv(file.path("data", "CropYieldData_County.csv")) |> 
  subset(Year %in% yrs_common)

df_climate <- read_xlsx(file.path("data", "ClimateData_County.xlsx"),
                        col_types = rep("numeric", 15)) |> 
  subset(Year %in% yrs_common) |> 
  group_by(Year, FIPS) |> 
  summarize(precip_mm = sum(precip_mm_gridmet))

df_irrigation_in <- read_csv(file.path("data", "WaterUseData_County.csv")) |> 
  subset(Year %in% yrs_common)

# inspect irrigation options and decide which variable to use
# ggplot(subset(df_irrigation_in, n_pdiv >= 3), 
#        aes(x = irrigation_mm_mean, y = irrigation_mm_median)) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1)

irrigation_var <- "irrigation_mm_mean"  # mean and median pretty much same in graph
df_irrigation <-
  df_irrigation_in |> 
  rename(irrigation_mm = !!sym(irrigation_var)) |> 
  dplyr::select(Year, FIPS, Crop, irrigation_mm, n_pdiv)

# combine data into single data frame -------------------------------------

df_combo <- 
  df_yield |> 
  left_join(df_climate, by = c("Year", "FIPS")) |> 
  left_join(df_irrigation, by = c("Year", "FIPS", "Crop")) |> 
  # all non-irrigated crops should be set to irrigation = 0
  mutate(irrigation_mm = ifelse(WaterManagement == "Non-Irrigated", 0, irrigation_mm)) |> 
  # calculate water available
  mutate(totalWater_mm = precip_mm + irrigation_mm) |> 
  # get rid of NA values in total water (happens when irrigated amount is unknown)
  filter(!is.na(totalWater_mm))

# plot data ---------------------------------------------------------------

ggplot(df_combo, aes(x = totalWater_mm, y = yield_kgHa_detrended)) +
  geom_point(aes(color = WaterManagement)) +
  facet_wrap(~ Crop, scales = "free") +
  labs(x = "Precipitation + Irrigation [mm]", 
       y = "Yield [kg/ha]") + 
  scale_color_manual(name = "Water Management",
                     values = c("Non-Irrigated" = col.cat.org, 
                                "Irrigated" = col.cat.blu)) + 
  stat_smooth(method = "loess")

ggplot(df_combo, aes(x = totalWater_mm, y = yield_kcalHa_detrended)) +
  geom_point(aes(color = WaterManagement)) +
  facet_wrap(~ Crop, scales = "free") +
  labs(x = "Precipitation + Irrigation [mm]", 
       y = "Yield [kcal/ha]") + 
  scale_color_manual(name = "Water Management",
                     values = c("Non-Irrigated" = col.cat.org, 
                                "Irrigated" = col.cat.blu)) + 
  stat_smooth(method = "loess")

# fit quadratic equation for each crop ------------------------------------

# fit quadratic equation to detrended yield in kcal/ha
# and kg/ha as a function of total water (precipitation + irrigation)

df_corn <- subset(df_combo, Crop == "Corn")
df_soy <- subset(df_combo, Crop == "Soybeans")
df_sorghum <- subset(df_combo, Crop == "Sorghum")
df_wheat <- subset(df_combo, Crop == "Wheat")

fit_corn_kcalHa <- lm(yield_kcalHa_detrended ~ poly(totalWater_mm, 2), data = df_corn)
fit_soy_kcalHa <- lm(yield_kcalHa_detrended ~ poly(totalWater_mm, 2), data = df_soy)
fit_sorghum_kcalHa <- lm(yield_kcalHa_detrended ~ poly(totalWater_mm, 2), data = df_sorghum)
fit_wheat_kcalHa <- lm(yield_kcalHa_detrended ~ poly(totalWater_mm, 2), data = df_wheat)

fit_corn_kgHa <- lm(yield_kgHa_detrended ~ poly(totalWater_mm, 2), data = df_corn)
fit_soy_kgHa <- lm(yield_kgHa_detrended ~ poly(totalWater_mm, 2), data = df_soy)
fit_sorghum_kgHa <- lm(yield_kgHa_detrended ~ poly(totalWater_mm, 2), data = df_sorghum)
fit_wheat_kgHa <- lm(yield_kgHa_detrended ~ poly(totalWater_mm, 2), data = df_wheat)

# plot data points with fitted quadratic equation
ggplot(df_combo, aes(x = totalWater_mm, y = yield_kcalHa_detrended)) +
  geom_point(aes(color = WaterManagement)) +
  facet_wrap(~ Crop, scales = "free_y") +
  labs(x = "Precipitation + Irrigation [mm]", 
       y = "Yield [kcal/ha]") + 
  scale_color_manual(name = "Water Management",
                     values = c("Non-Irrigated" = col.cat.org, 
                                "Irrigated" = col.cat.blu)) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black")
