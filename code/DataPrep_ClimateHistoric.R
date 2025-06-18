## DataPrep_ClimateHistoric.R
# Author: Archi Howlader
# Date: 5/22/2025
# Changed by AH
#
# Purpose: This script gets monthly historic climate data, averaged over the following
#   subdomains: Counties, EKSRB, AlluvialCorridor.
# Subdomain boundaries are created in `DataPrep_Boundaries.R` script and saved in `data` folder.
# 
# CSV files created are:
#  County CSV file: ClimateData_County.csv
#  | Year | Month | FIPS | precip_mm | {otherClimateVars}
#  
#  EKSRB CSV file: ClimateData_EKSRB.csv
#  | Year | Month | precip_mm | {otherClimateVars}
#  
#  Alluvial Corridor CSV file: ClimateData_AlluvialCorridor.csv
#  | Year | Month | precip_mm | {otherClimateVars}
#
# Output is saved in `data` folder.

# Set up workspace --------------------------------------------------------


cat("\014") 
rm(list=ls())
library(dataRetrieval)
library(leaflet)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(base64enc)
library(lubridate)
library(patchwork)
library(grid)
library(stringr)
library(wql)
library(measurements)
library(tidyverse)
library(openair)
library(heatwaveR)
library(data.table)
library(methods)
library(investr)
library(gridExtra)
library(Metrics)
library(scales)
library(plotly)
library(geosphere)
library(cowplot)
library(FedData)
library(terra)
library(units)
library(zoo)
library(sf)
library(ggpubr)
library(climateR)
library(writexl)

file_Path_Variable_IT <- "C:/Users/a905h226/OneDrive - University of Kansas/Desktop/TooBigForGithub_Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "C:/Users/a905h226/OneDrive - University of Kansas/Desktop/Steps_Workflow_Sept17/Output"
file_Path_Variable_I<- "C:/Users/a905h226/OneDrive - University of Kansas/Desktop/Steps_Workflow_Sept17/InputFiles"

sf_corridor <- st_read(file.path(file_Path_Variable_I, "SafeKaw_Optimization/Boundary_EKSRBalluvialCorridor.gpkg"))
sf_counties <- st_read(file.path(file_Path_Variable_I, "SafeKaw_Optimization/Boundary_EKSRBcounties.gpkg"))
sf_watershed <- st_read(file.path(file_Path_Variable_I, "SafeKaw_Optimization/Boundary_EKSRBwatershed.gpkg"))


ggplot() +
  geom_sf(data = sf_watershed, fill = "lightblue", color = "blue", alpha = 0.3) +
  geom_sf(data = sf_counties, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = sf_corridor, fill = "orange", color = "red", alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Overlay of Watershed, Counties, and Alluvial Corridor",
    caption = "Data: SafeKAW"
  )


corridor_df <- sf_corridor %>%
  mutate(
    SOURCE = "SafeKAW",
    NAME = "Alluvial Corridor",
    ID = "Corridor"
  ) %>%
  dplyr::select(SOURCE, NAME, ID, geom)

counties_df <- sf_counties %>%
  mutate(
    SOURCE = "SafeKAW",
    NAME = "EKSRB Counties",
    ID = as.character(FIPS)  
  ) %>%
  dplyr::select(SOURCE, NAME, ID, geom)

watershed_df <- sf_watershed %>%
  mutate(
    SOURCE = "SafeKAW",
    NAME = "EKSRB Watershed",
    ID = "Watershed"
  ) %>%
  dplyr::select(SOURCE, NAME, ID, geom)

SafeKAW_Region_df <- bind_rows(corridor_df, counties_df, watershed_df)
print(SafeKAW_Region_df)
SafeKAW_Region_df <- st_transform(SafeKAW_Region_df, crs = 32614)







#Prism starts from 1895 
#Gridmet from 1979
#Combined them, NA where no data 


# 
# For temperature, radiation, and wind it should be monthly mean instead of sum
# Only ETo and precipitation would be sum  #no idea how to put it in the naming convention 




Climate_Variables <- readRDS(file.path(file_Path_Variable_O, "GridmetAll_WithPrism_6152025.rds"))
Climate_Variables <- Climate_Variables %>%
  rename(
    x = RefET_x,
    y = RefET_y
  )



Climate_Variables<- Climate_Variables %>% dplyr::select(x,y,combined_data)
Climate_Variables <- Climate_Variables %>%
  mutate(
    combined_data = map(combined_data, ~ .x %>%
                          rename(
                            gridmet_tmmx_k = gridmet_tmmx_degC,
                            gridmet_tmmn_k = gridmet_tmmn_degC
                          ))
  )


Climate_Variables_sf <- Climate_Variables %>%
  st_as_sf(coords = c("x", "y"), crs = 32614)


# CSV files created are:
#  County CSV file: ClimateData_County.csv
#  | Year | Month | FIPS | precip_mm | {otherClimateVars}
#  
#  EKSRB CSV file: ClimateData_EKSRB.csv
#  | Year | Month | precip_mm | {otherClimateVars}
#  
#  Alluvial Corridor CSV file: ClimateData_AlluvialCorridor.csv
#  | Year | Month | precip_mm | {otherClimateVars}
#
compute_monthly_summary <- function(data) {
  data %>%
    unnest(combined_data) %>%
    mutate(
      Year = year(Date),
      Month = month(Date)
    ) %>%
    group_by(Year, Month) %>%
    summarise(
      precip_mm_gridmet = mean(gridmet_pr_mm, na.rm = TRUE),
      etr_mm_gridmet = mean(gridmet_etr_mm, na.rm = TRUE),
      tmmx_K_gridmet = mean(gridmet_tmmx_k, na.rm = TRUE),
      tmmn_K_gridmet = mean(gridmet_tmmn_k, na.rm = TRUE),
      tmmx_C_gridmet = mean(gridmet_tmmx_k - 273.15, na.rm = TRUE),
      tmmn_C_gridmet = mean(gridmet_tmmn_k - 273.15, na.rm = TRUE),
      srad_wpm2_gridmet = mean(gridmet_srad_wpm2, na.rm = TRUE),
      wind_mps_gridmet = mean(gridmet_wind_mps, na.rm = TRUE),
      rhmax_pct_gridmet = mean(gridmet_rhmax_pct, na.rm = TRUE),
      rhmin_pct_gridmet = mean(gridmet_rhmin_pct, na.rm = TRUE),
      vpd_kpa_gridmet = mean(gridmet_vpd_kpa, na.rm = TRUE),
      .groups = "drop"
    )
}

all_extreme_results <- list()

for (i in 1:nrow(SafeKAW_Region_df)) {
  
  cat("Processing division", i, "of", nrow(SafeKAW_Region_df), "\n")
  
  first_climate_division <- SafeKAW_Region_df[i, ]
  
  Climate_Var_Subset <- tryCatch({
    st_intersection(Climate_Variables_sf, first_climate_division)
  }, error = function(e) {
    message("Intersection failed for division ", i)
    return(NULL)
  })
  
  if (is.null(Climate_Var_Subset) || nrow(Climate_Var_Subset) == 0) next
  Climate_Var_Summary <- compute_monthly_summary(Climate_Var_Subset)
  
  if (nrow(Climate_Var_Summary) < 2) next
  
  Climate_Var_Summary$ID <- SafeKAW_Region_df$ID[i]
  Climate_Var_Summary$NAME <- SafeKAW_Region_df$NAME[i]
  
  all_extreme_results[[i]] <- Climate_Var_Summary
}

final_extreme_event_df_gridmet <- bind_rows(all_extreme_results)
head(final_extreme_event_df_gridmet)

ClimateData_AlluvialCorridor_Gridmet<- final_extreme_event_df_gridmet %>% filter(NAME=="Alluvial Corridor")
ClimateData_County_Gridmet<-final_extreme_event_df_gridmet %>% filter(NAME=="EKSRB Counties")
ClimateData_EKSRB_Gridmet<-final_extreme_event_df_gridmet %>% filter(NAME=="EKSRB Watershed")




write_xlsx(
  ClimateData_AlluvialCorridor_Gridmet,
  path = file.path(file_Path_Variable_O, "ClimateData_AlluvialCorridor_Gridmet.xlsx")
)



write_xlsx(
  ClimateData_County_Gridmet,
  path = file.path(file_Path_Variable_O, "ClimateData_County_Gridmet.xlsx")
)



write_xlsx(
  ClimateData_EKSRB_Gridmet,
  path = file.path(file_Path_Variable_O, "ClimateData_EKSRB_Gridmet.xlsx")
)



Precipitation_Data_Gridded_Filtered <- readRDS(file.path(file_Path_Variable_O, "step14_Precipitation_Data_Gridded_Filtered_extremes_Upuntil2023_feb28.rds"))
Precipitation_Data_Gridded_Filtered<- Precipitation_Data_Gridded_Filtered %>% dplyr::select(x,y,data)
Precipitation_Data_Gridded_Filtered$unit<- "mm"


Precip_sf <- Precipitation_Data_Gridded_Filtered %>%
  st_as_sf(coords = c("x", "y"), crs = 32614)



compute_monthly_summary <- function(data, target_column) {
  data %>%
    unnest(data) %>%
    group_by(YearMonth) %>%
    summarise(
      monthly_avg = mean(.data[[target_column]], na.rm = TRUE),
      .groups = "drop"
    ) 
}


all_extreme_results <- list()

for (i in 1:nrow(SafeKAW_Region_df)) {
  
  cat("Processing division", i, "of", nrow(SafeKAW_Region_df), "\n")
  
  first_climate_division <- SafeKAW_Region_df[i, ]
  
  Precipitation_Subset <- tryCatch({
    st_intersection(Precip_sf, first_climate_division)
  }, error = function(e) {
    message("Intersection failed for division ", i)
    return(NULL)
  })
  
  if (is.null(Precipitation_Subset) || nrow(Precipitation_Subset) == 0) next
  Precipitation_Summary <- compute_monthly_summary(Precipitation_Subset, "total_rainfall_monthly")
  
  # Precipitation_Summary <- Precipitation_Subset %>%
  #   unnest(data) %>%
  #   group_by(YearMonth) %>%
  #   summarise(Total_precip_mm_avg = mean(total_rainfall_monthly, na.rm = TRUE), .groups = "drop")  %>%
  #   mutate(year = year(YearMonth)) %>%
  #   group_by(year) %>%
  #   summarise(Total_precip_mm = sum(Total_precip_mm_avg, na.rm = TRUE), .groups = "drop") 
  #starting with total sum of monthly
  #average that for the whole area 
  #sum it for the year 
  
  if (nrow(Precipitation_Summary) < 2) next
  
  annual_precip_trend_df <- data.frame(
    Year = year(Precipitation_Summary$YearMonth),
    Month = month(Precipitation_Summary$YearMonth),
    precip_mm_prism = Precipitation_Summary$monthly_avg
  )
  
  annual_precip_trend_df$ID <- SafeKAW_Region_df$ID[i]
  annual_precip_trend_df$NAME <- SafeKAW_Region_df$NAME[i]
  
  all_extreme_results[[i]] <- annual_precip_trend_df
}

final_extreme_event_df_Prism <- bind_rows(all_extreme_results)
head(final_extreme_event_df_Prism)


ClimateData_AlluvialCorridor_prism<- final_extreme_event_df_Prism %>% filter(NAME=="Alluvial Corridor")
ClimateData_County_prism<-final_extreme_event_df_Prism %>% filter(NAME=="EKSRB Counties")
ClimateData_EKSRB_prism<-final_extreme_event_df_Prism %>% filter(NAME=="EKSRB Watershed")




write_xlsx(
  ClimateData_AlluvialCorridor_prism,
  path = file.path(file_Path_Variable_O, "ClimateData_AlluvialCorridor_prism.xlsx")
)



write_xlsx(
  ClimateData_County_prism,
  path = file.path(file_Path_Variable_O, "ClimateData_County_prism.xlsx")
)



write_xlsx(
  ClimateData_EKSRB_prism,
  path = file.path(file_Path_Variable_O, "ClimateData_EKSRB_prism.xlsx")
)



ClimateData_EKSRB_combined <- full_join(
  ClimateData_EKSRB_prism,
  ClimateData_EKSRB_Gridmet,
  by = c("Year", "Month", "ID", "NAME")
)

ClimateData_County_combined <- full_join(
  ClimateData_County_prism,
  ClimateData_County_Gridmet,
  by = c("Year", "Month", "ID", "NAME")
)

ClimateData_Corridor_combined <- full_join(
  ClimateData_AlluvialCorridor_prism,
  ClimateData_AlluvialCorridor_Gridmet,
  by = c("Year", "Month", "ID", "NAME")
)
ClimateData_EKSRB_combined <- ClimateData_EKSRB_combined %>% select(-geometry)
ClimateData_County_combined <- ClimateData_County_combined %>% select(-geometry)
ClimateData_Corridor_combined <- ClimateData_Corridor_combined %>% select(-geometry)

ClimateData_EKSRB_combined <- ClimateData_EKSRB_combined %>%
  dplyr::select(Year, Month, ID, NAME, everything())

ClimateData_County_combined <- ClimateData_County_combined %>%
  dplyr::select(Year, Month, ID, NAME, everything())

ClimateData_Corridor_combined <- ClimateData_Corridor_combined %>%
  dplyr::select(Year, Month, ID, NAME, everything())



ClimateData_EKSRB_combined <- ClimateData_EKSRB_combined %>% select(-NAME,-ID)
ClimateData_County_combined <- ClimateData_County_combined %>% select(-NAME)
ClimateData_Corridor_combined <- ClimateData_Corridor_combined %>% select(-NAME,-ID)



ClimateData_County_combined<- ClimateData_County_combined %>% rename(FIPS=ID)

write_xlsx(
  ClimateData_EKSRB_combined,
  path = file.path(file_Path_Variable_O, "ClimateData_EKSRB.xlsx")
)



write_xlsx(
  ClimateData_County_combined,
  path = file.path(file_Path_Variable_O, "ClimateData_County.xlsx")
)



write_xlsx(
  ClimateData_Corridor_combined,
  path = file.path(file_Path_Variable_O, "ClimateData_Corridor.xlsx")
)










# ggplot() +
#   geom_point(data = Watershed_df, aes(x = Year, y = prism_precip_mm)) +
#   theme_minimal() +
#   labs(
#     title = "Monthly PRISM Precipitation in EKSRB Watershed",
#     x = "Year",
#     y = "Precipitation (mm)"
#   )

library(dataRetrieval)
site_numbers <- c("06821500", "06892350")
site_info <- readNWISsite(site_numbers)

site_info_selected <- site_info %>%
  select(site_no, station_nm, dec_lat_va, dec_long_va)

site_info_selected <- site_info_selected %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326) %>%
  st_transform(32614)


Climate_grid_points <- Climate_Variables %>%
  mutate(id = row_number()) %>%
  st_as_sf(coords = c("x", "y"), crs = 32614)


library(nngeo)

nearest_to_sites <- st_nn(site_info_selected, Climate_grid_points, k = 1, returnDist = TRUE)

site_info_selected$nearest_index <- nearest_to_sites$nn
site_info_selected$distance_to_nearest <- nearest_to_sites$dist

nearest_climate_data <- Climate_Variables %>%
  slice(unlist(site_info_selected$nearest_index)) %>%
  mutate(
    station_id = site_info_selected$site_no,
    station_name = site_info_selected$station_nm,
    distance_to_station_m = unlist(site_info_selected$distance_to_nearest)
  )


arikaree_data <- nearest_climate_data %>% filter(station_id == "06821500")
arikaree_df <- data.frame(arikaree_data$combined_data[[1]])

desoto_data <- nearest_climate_data %>% filter(station_id == "06892350")
desoto_df <- data.frame(desoto_data$combined_data[[1]])



arikaree_data <- nearest_climate_data %>% filter(station_id == "06821500")
arikaree_df <- data.frame(arikaree_data$combined_data[[1]])

desoto_data <- nearest_climate_data %>% filter(station_id == "06892350")
desoto_df <- data.frame(desoto_data$combined_data[[1]])



arikaree_df <- data.frame(arikaree_data$combined_data[[1]]) %>%
  mutate(Station = "Arikaree River (06821500)")

desoto_df <- data.frame(desoto_data$combined_data[[1]]) %>%
  mutate(Station = "Kansas River at DeSoto (06892350)")

combined_df <- bind_rows(arikaree_df, desoto_df)

combined_long <- combined_df %>%
  pivot_longer(
    cols = -c(Date, Station),
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(combined_long, aes(x = Date, y = Value, color = Station)) +
  geom_line(size = 0.8) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Climate Variables at Arikaree and DeSoto Stations",
    x = "Date", y = "Value", color = "Station"
  )


# 
# 
# 
# source(file.path("code", "paths+packages.R"))
# 
# # load boundaries ------------------------------------
# 
# sf_corridor <- st_read(file.path("data", "Boundary_EKSRBalluvialCorridor.gpkg"))
# sf_counties <- st_read(file.path("data", "Boundary_EKSRBcounties.gpkg"))
# sf_watershed <- st_read(file.path("data", "Boundary_EKSRBwatershed.gpkg"))
# 
