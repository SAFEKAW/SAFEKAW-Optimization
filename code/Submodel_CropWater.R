## Submodel_CropWater.R
# Author: Sam Zipper
# Date: 6/26/2025
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
#yrs_common <- 1990:2023
yrs_common <- 2006:2023

df_yield <- read_csv(file.path("data", "CropYieldData_County.csv")) |> 
  subset(Year %in% yrs_common)

df_climate <- read_csv(file.path("data", "ClimateData_County.csv")) |> 
  subset(Year %in% yrs_common) |> 
  group_by(Year, FIPS) |> 
  summarize(precip_mm = sum(precip_mm_gridmet),
            tmax_C = mean(tmmx_C_gridmet))

df_irrigation_in <- read_csv(file.path("data", "WaterUseData_County.csv")) |> 
  subset(Year %in% yrs_common)



# summarize land cover data
df_landcover <- read_csv(file.path("data", "LandCoverData-CDL_County.csv"))

df_landcover_byCounty <- 
  df_landcover |> 
  subset(LandCover %in% c("Corn", "Soybeans", "Sorghum", "Wheat")) |>
  group_by(FIPS, LandCover, Year) |>
  summarize(area_ha_mean = mean(area_ha, na.rm = TRUE),
            area_prc_mean = mean(area_prc, na.rm = TRUE))


ggplot(df_landcover_byCounty, aes(x = factor(FIPS), y = area_ha_mean)) +
  geom_col() +
  facet_wrap(~LandCover, scales = "free") +
  coord_flip()

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

df_combo_all <- 
  df_yield |> 
  left_join(df_climate, by = c("Year", "FIPS")) |> 
  left_join(df_irrigation, by = c("Year", "FIPS", "Crop")) |> 
  # all non-irrigated crops should be set to irrigation = 0
  mutate(irrigation_mm = ifelse(WaterManagement == "Non-Irrigated", 0, irrigation_mm)) |> 
  # calculate water available
  mutate(totalWater_mm = precip_mm + irrigation_mm) |> 
  # get rid of NA values in total water (happens when irrigated amount is unknown)
  filter(!is.na(totalWater_mm)) |> 
  left_join(df_landcover_byCounty, by = c("FIPS", "Crop"="LandCover", "Year")) |>
  #adding in fertilizer here
  mutate(fertilizer_kgHa = 
           case_when(
             Crop == "Corn" ~ 250, # rough average of irr and rainfed
             Crop == "Sorghum" ~ 112,
             Crop == "Soybeans" ~ 56, # rough average of irr and rainfed
             Crop == "Wheat" ~ 89,
             TRUE ~ NA_real_),
         fertilizer_kgCrop = fertilizer_kgHa * area_ha_mean)
 


# trim to get rid of small sample size data points
area_ha_thres <- 64.75*10 # 160 acre quarter-section is 64.75 ha
pdiv_thres <- 3
df_combo <- 
  df_combo_all |> 
  filter(!(WaterManagement == "Irrigated" & n_pdiv < pdiv_thres)) |> 
  filter(area_ha_mean > area_ha_thres)

write.csv(df_combo, "df_combo.csv")
# fit quadratic equation for each crop ------------------------------------

# fit quadratic equation to detrended yield in kcal/ha
# and kg/ha as a function of total water (precipitation + irrigation)

df_corn <- subset(df_combo, Crop == "Corn")
df_soy <- subset(df_combo, Crop == "Soybeans")
df_sorghum <- subset(df_combo, Crop == "Sorghum")
df_wheat <- subset(df_combo, Crop == "Wheat")

fit_corn_kcalHa <- lm(yield_kcalHa_detrended ~ poly(totalWater_mm, 2)+ tmax_C + fertilizer_kgCrop , data = df_corn) 
fit_soy_kcalHa <- lm(yield_kcalHa_detrended ~ poly(totalWater_mm, 2)+ tmax_C + fertilizer_kgCrop, data = df_soy)
fit_sorghum_kcalHa <- lm(yield_kcalHa_detrended ~ poly(totalWater_mm, 2)+ tmax_C , data = df_sorghum)
fit_wheat_kcalHa <- lm(yield_kcalHa_detrended ~ poly(totalWater_mm, 2)+ tmax_C + fertilizer_kgCrop, data = df_wheat)

fit_corn_kgHa <- lm(yield_kgHa_detrended ~ poly(totalWater_mm, 2)+ tmax_C + fertilizer_kgCrop, data = df_corn)
fit_soy_kgHa <- lm(yield_kgHa_detrended ~ poly(totalWater_mm, 2)+ tmax_C + fertilizer_kgCrop, data = df_soy)
fit_sorghum_kgHa <- lm(yield_kgHa_detrended ~ poly(totalWater_mm, 2)+ tmax_C , data = df_sorghum)
fit_wheat_kgHa <- lm(yield_kgHa_detrended ~ poly(totalWater_mm, 2)+ tmax_C + fertilizer_kgCrop, data = df_wheat)

# summarize output
df_fit <-
  data.frame(
    Crop = c("Corn", "Soybeans", "Sorghum", "Wheat"),
    fit_R2_kcalHa = c(summary(fit_corn_kcalHa)$adj.r.squared, 
                      summary(fit_soy_kcalHa)$adj.r.squared, 
                      summary(fit_sorghum_kcalHa)$adj.r.squared, 
                      summary(fit_wheat_kcalHa)$adj.r.squared),
    fit_R2_kgHa = c(summary(fit_corn_kgHa)$adj.r.squared, 
                    summary(fit_soy_kgHa)$adj.r.squared, 
                    summary(fit_sorghum_kgHa)$adj.r.squared, 
                    summary(fit_wheat_kgHa)$adj.r.squared)
  )

# get predicted lm to investigate residual by county
df_corn$yield_kcalHa_detrended_fit <- fit_corn_kcalHa$fitted.values
df_soy$yield_kcalHa_detrended_fit <- fit_soy_kcalHa$fitted.values
df_sorghum$yield_kcalHa_detrended_fit <- fit_sorghum_kcalHa$fitted.values
df_wheat$yield_kcalHa_detrended_fit <- fit_wheat_kcalHa$fitted.values
df_combo <- bind_rows(df_corn, df_soy, df_sorghum, df_wheat)

# plot data ---------------------------------------------------------------

# plot data points with fitted quadratic equation
ggplot(df_combo, aes(x = totalWater_mm, y = yield_kcalHa_detrended)) +
  geom_point(aes(color = WaterManagement)) +
  facet_wrap(~ Crop, scales = "free_y") +
  labs(x = "Precipitation + Irrigation [mm]", 
       y = "Yield [kcal/ha]") + 
  scale_color_manual(name = "Water Management",
                     values = c("Non-Irrigated" = col.cat.org, 
                                "Irrigated" = col.cat.blu)) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black") +
  theme(legend.position = "bottom")
ggsave(file.path("figures", "Submodel_CropWater_WaterProductivityCurves.png"),
       width = 120, height = 120, units = "mm")

# plot residuals by county with counties order by mean annual precip
df_climate_meanPrecip <- 
  df_climate |> 
  group_by(FIPS) |> 
  summarize(precip_mm_mean = mean(precip_mm)) |> 
  arrange(precip_mm_mean)

df_combo |> 
  left_join(df_climate_meanPrecip, by = "FIPS") |> 
  mutate(FIPS_ordered = factor(FIPS, levels = c(df_climate_meanPrecip$FIPS))) |> 
  ggplot(aes(x = FIPS_ordered, y = (yield_kcalHa_detrended - yield_kcalHa_detrended_fit))) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_boxplot() +
  facet_wrap(~ Crop, scales = "free_y") +
  labs(x = "County FIPS (driest --> wettest)", 
       y = "Residual (actual - predicted) [kcal/ha]") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(file.path("figures", "Submodel_CropWater_ResidualByCounty.png"),
       width = 190, height = 120, units = "mm")



# Join R² values (kcal/ha) to plotting data
r2_df <- df_fit|>
  select(Crop, fit_R2_kcalHa) |>
  mutate(
    label = paste0("R² = ", round(fit_R2_kcalHa, 2))
  )



# plot predicted vs observed
ggplot(df_combo, aes(x = yield_kcalHa_detrended_fit, y = yield_kcalHa_detrended)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(aes(color = WaterManagement)) +
  facet_wrap(~ Crop, scales = "free") +
  scale_color_manual(name = "Water Management",
                     values = c("Non-Irrigated" = col.cat.org, 
                                "Irrigated" = col.cat.blu)) + 
  stat_smooth(method = "lm", color = "black") +
  labs(x = "Predicted Yield [kcal/ha]", 
       y = "Observed Yield [kcal/ha]")  +
  geom_text(data = r2_df, aes(x = 3000, y = 22500, label = label),
            inherit.aes = FALSE, hjust = 0) +
 # ylim(0, 25000) +
  #xlim(0, 25000) +
  ggtitle("Predicted vs Observed") 

#large scatter across all (low R^2's)
#corn & sorghum estimates are in right range, but shifted below 1-1 line, 
#seems to pretty significantly underestimate soybean & wheat

