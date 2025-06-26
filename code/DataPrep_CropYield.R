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
#  Column W: Detrended Yield

# process yield data ------------------------------------

# basic cleaning
df_yield_in <-
  df_yield_raw |> 
  # trim to counties of interest
  mutate(FIPS = paste0(`State Code`, `County Code`)) |> 
  subset(FIPS %in% sf_counties$FIPS) |> 
  # rename columns
  rename(all_of(c(
    Crop = "Commodity Name",
    Year = "Yield Year...24",
    WaterManagement = "Irrigation Practice Name",
    yield_buAc = "Yield Amount",
    yield_buAc_detrended = "Detrended Yield Amount"
  )))

# for each crop type and water management, get yield stats which can be used 
# for fertilizer data creation based on tables:
#  https://sorghumpartners.com/wp-content/uploads/2021/04/KSU-Soil-Fertilizer-Recommendations-mf2586.pdf

# determine 90th percentile as assumed yield target
df_yield_stats <- 
  df_yield_in |> 
  group_by(Crop, WaterManagement) |> 
  summarize(yield_buAc_detrended_mean = mean(yield_buAc_detrended),
            yield_buAc_detrended_median = median(yield_buAc_detrended),
            yield_buAc_detrended_q90 = quantile(yield_buAc_detrended, 0.9))

# plot
ggplot() +
  geom_line(data = df_yield_in, aes(x = Year, y = yield_buAc_detrended)) +
  geom_point(data = df_yield_in, aes(x = Year, y = yield_buAc_detrended)) +
  # add a horizontal line for each facet at the value of yield_buAc_detrended_q90 from df_yield_stats
  geom_hline(data = df_yield_stats, aes(yintercept = yield_buAc_detrended_q90), color = col.cat.red) +
  facet_wrap(Crop ~ WaterManagement, scales = "free_y")

df_yield_out <- 
  df_yield_in |> 
  # convert to kg/ha
  left_join(yield_conversion, by = "Crop") |> 
  mutate(yield_kgHa = yield_buAc*kg_per_bushel/ha_per_ac,
         yield_kcalHa = yield_kgHa*kcal_per_kg,
         yield_kgHa_detrended = yield_buAc_detrended*kg_per_bushel/ha_per_ac,
         yield_kcalHa_detrended = yield_kgHa_detrended*kcal_per_kg) |> 
  # trim to needed columns
  dplyr::select(Year, FIPS, Crop, WaterManagement, yield_kgHa, yield_kcalHa, yield_kgHa_detrended, yield_kcalHa_detrended)

# rename "Grain Sorghum" to "Sorghum" to match CDL and water use
df_yield_out$Crop[df_yield_out$Crop == "Grain Sorghum"] <- "Sorghum"

# for wheat and sorghum, there is minimal irrigation in this region; remove irrigated fields
df_yield_out <- 
  df_yield_out |> 
  subset(!(Crop %in% c("Wheat", "Sorghum") & WaterManagement == "Irrigated"))

# visualize and save ------------------------------------------------------

ggplot(df_yield_out, aes(x = yield_kgHa)) +
  geom_histogram() + 
  facet_grid(WaterManagement~Crop, scales = "free_x")

ggplot(df_yield_out, aes(x = yield_kcalHa)) +
  geom_histogram() + 
  facet_grid(WaterManagement~Crop)

write_csv(df_yield_out, file.path("data", "CropYieldData_County.csv"))
