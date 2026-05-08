# hpc_opt/scripts/01_common_inputs_make.R
# Build common inputs at county + basin scales (minimal, batchable).

source(here::here("hpc_opt", "R", "00_setup.R"))

# ---- I/O paths (relative to repo root) ----
#scenario_tag <- "hist_baseline"
# scenario_tag <- "rcp45_early"
 #scenario_tag <- "rcp45_mid"
 #scenario_tag <- "rcp45_late"
 # scenario_tag <- "rcp85_early"
 # scenario_tag <- "rcp85_mid"
#  scenario_tag <- "rcp85_late"
 # gcm_name <- "ensemble"

 
 if (!exists("scenario_tag")) {
   scenario_tag <- "hist_baseline"
 }
  
  if (!exists("gcm_name")) {
    gcm_name <- ifelse(scenario_tag == "hist_baseline", NA_character_, "ensemble")
  }
  
  message("Using scenario_tag = ", scenario_tag)
  message("Using gcm_name    = ", gcm_name)


if (scenario_tag == "hist_baseline") {
  yrs_common <- 2006:2023
} else if (scenario_tag == "rcp45_early") {
  yrs_common <- 2025:2049
} else if (scenario_tag == "rcp45_mid") {
  yrs_common <- 2050:2074
} else if (scenario_tag == "rcp45_late") {
  yrs_common <- 2075:2099
} else if (scenario_tag == "rcp85_early") {
  yrs_common <- 2025:2049
} else if (scenario_tag == "rcp85_mid") {
  yrs_common <- 2050:2074
} else if (scenario_tag == "rcp85_late") {
  yrs_common <- 2075:2099
} else {
  stop("Unknown scenario_tag: ", scenario_tag)
}
 
is_future <- scenario_tag != "hist_baseline"
  
in_landcover <- here::here("data", "LandCoverData-CDL_County.csv")
in_irrig     <- here::here("data", "WaterUseData_County.csv")
in_irrig_alluvial <- here::here("data", "WaterUseByCrop_AlluvialCorridor.csv")

  if (!is_future) {
    in_climate <- here::here("data", paste0("ClimateData_County_", scenario_tag, ".csv"))
    in_crop_climate <- here::here("data", paste0("crop_climate_gs_", scenario_tag, ".csv"))
    
    out_county <- here::here("hpc_opt", "outputs", paste0("common_inputs_county_", scenario_tag, ".csv"))
    out_basin  <- here::here("hpc_opt", "outputs", paste0("common_inputs_basin_", scenario_tag, ".csv"))
    out_areas  <- here::here("hpc_opt", "outputs", paste0("county_areas_", scenario_tag, ".csv"))
  } else {
    in_climate <- here::here("data", paste0("ClimateData_County_", gcm_name, "_", scenario_tag, ".csv"))
    in_crop_climate <- here::here("data", paste0("crop_climate_gs_", gcm_name, "_", scenario_tag, ".csv"))
    
    out_county <- here::here("hpc_opt", "outputs", paste0("common_inputs_county_", gcm_name, "_", scenario_tag, ".csv"))
    out_basin  <- here::here("hpc_opt", "outputs", paste0("common_inputs_basin_", gcm_name, "_", scenario_tag, ".csv"))
    out_areas  <- here::here("hpc_opt", "outputs", paste0("county_areas_", gcm_name, "_", scenario_tag, ".csv"))
  }
  

dir.create(dirname(out_county), recursive = TRUE, showWarnings = FALSE)

needed_files <- c(in_climate, in_landcover, in_irrig, in_irrig_alluvial, in_crop_climate)
missing_files <- needed_files[!file.exists(needed_files)]

if (length(missing_files) > 0) {
  stop("Missing input files:\n", paste(" -", missing_files, collapse = "\n"))
}

# ---- 1) Climate (county, annual) ----
df_climate_county <- readr::read_csv(in_climate, show_col_types = FALSE) %>%
  filter(Year %in% yrs_common) %>%
  mutate(FIPS = as.character(FIPS)) %>%
  group_by(Year, FIPS) %>%
  summarise(
    precip_m     = sum(precip_mm_gridmet, na.rm = TRUE) / 1000,  # annual depth (m)
    precip_m_avg = mean(precip_mm_gridmet, na.rm = TRUE) / 1000, # mean daily (?) depth (m)
    tmin_C_avg   = mean(tmmn_C_gridmet, na.rm = TRUE),
    tmax_C_avg   = mean(tmmx_C_gridmet, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 2) Land cover (county) ----
raw_landcover_county <- readr::read_csv(in_landcover, show_col_types = FALSE) %>%
  mutate(
    FIPS = as.character(FIPS),
    area_m2 = area_ha * 10000
  ) %>%
  select(Year, FIPS, LandCover, area_m2, area_ha, area_prc)

if (!is_future) {
  df_landcover_county <- raw_landcover_county %>%
    filter(Year %in% yrs_common)
} else {
  landcover_hist_mean <- raw_landcover_county %>%
    filter(Year %in% 2006:2023) %>%
    group_by(FIPS, LandCover) %>%
    summarise(
      area_m2 = mean(area_m2, na.rm = TRUE),
      area_ha = mean(area_ha, na.rm = TRUE),
      area_prc = mean(area_prc, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_landcover_county <- tidyr::crossing(
    Year = yrs_common,
    landcover_hist_mean
  )
}

df_landcover_county %>%
  summarise(
    min_year = min(Year, na.rm = TRUE),
    max_year = max(Year, na.rm = TRUE),
    n_years = n_distinct(Year),
    n_fips = n_distinct(FIPS),
    n_na_area = sum(is.na(area_m2))
  ) %>%
  print()

landCoverGroups <- tibble::tibble(
  LandCover = c("Developed","Grass/Pasture/Shrubland",
                "Corn","Double Crop","Other Crops","Sorghum","Soybeans","Wheat"),
  LandCoverGroup = c("developed","grassPastureHay", rep("all_cult", 6))
)

df_landcover_all_class <- df_landcover_county %>%
  left_join(landCoverGroups, by = "LandCover") %>%
  filter(!is.na(LandCoverGroup)) %>%
  mutate(
    Crop = dplyr::case_when(
      LandCover %in% c("Corn","Sorghum","Soybeans","Wheat") ~ LandCover,
      TRUE ~ NA_character_
    )
  )

# ---- 3) Irrigation (county; crop-specific) ----
raw_irrigation_in <- readr::read_csv(in_irrig, show_col_types = FALSE) %>%
  filter(n_pdiv > 3) %>%
  mutate(
    FIPS = as.character(FIPS),
    irrigation_WaterUse_m = irrigation_mm_mean / 1000,
    LandCoverGroup = "all_cult"
  ) %>%
  select(Year, FIPS, Crop, LandCoverGroup, irrigation_WaterUse_m, n_pdiv)

if (!is_future) {
  df_irrigation_in <- raw_irrigation_in %>%
    filter(Year %in% yrs_common)
} else {
  irrigation_hist_mean <- raw_irrigation_in %>%
    filter(Year %in% 2006:2023) %>%
    group_by(FIPS, Crop, LandCoverGroup) %>%
    summarise(
      irrigation_WaterUse_m = mean(irrigation_WaterUse_m, na.rm = TRUE),
      n_pdiv = mean(n_pdiv, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_irrigation_in <- tidyr::crossing(
    Year = yrs_common,
    irrigation_hist_mean
  )
}

# ---- 3b) Basin/alluvial irrigation totals from actual water-use area + volume ----
raw_irrigation_alluvial <- readr::read_csv(in_irrig_alluvial, show_col_types = FALSE) %>%
  mutate(
    Crop = as.character(Crop),
    irrArea_m2 = irrArea_ha * 1e4
  )

if (!is_future) {
  df_irrigation_alluvial <- raw_irrigation_alluvial %>%
    filter(Year %in% yrs_common)
} else {
  # Use historical mean annual water use/area for future common-input basin summaries.
  # Scenario-specific future irrigation changes are handled later in eval/policy.
  irrigation_alluvial_hist_mean <- raw_irrigation_alluvial %>%
    filter(Year %in% 2006:2023) %>%
    group_by(Crop) %>%
    summarise(
      waterUse_m3 = mean(waterUse_m3, na.rm = TRUE),
      irrArea_ha  = mean(irrArea_ha, na.rm = TRUE),
      irrArea_m2  = mean(irrArea_m2, na.rm = TRUE),
      n_waterUse_NA = mean(n_waterUse_NA, na.rm = TRUE),
      n_irrArea_NA  = mean(n_irrArea_NA, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_irrigation_alluvial <- tidyr::crossing(
    Year = yrs_common,
    irrigation_alluvial_hist_mean
  )
}

# ---- 4) Fertilizer (county; simple constants by crop) ----
fert_rate_tbl <- tibble::tibble(
  Crop = c("Corn","Sorghum","Soybeans","Wheat"),
  fert_kgHa = c(250, 112, 56, 89)
)

df_fert_county <- df_landcover_county %>%
  filter(LandCover %in% fert_rate_tbl$Crop) %>%
  rename(Crop = LandCover) %>%
  left_join(fert_rate_tbl, by = "Crop") %>%
  mutate(
    fertilizer_kgHa   = fert_kgHa,
    fertilizer_kgCrop = fert_kgHa * area_ha
  ) %>%
  select(Year, FIPS, Crop, fertilizer_kgHa, fertilizer_kgCrop)

# ---- 5) Crop growing-season climate (county; crop-specific) ----
df_crop_climate <- readr::read_csv(in_crop_climate, show_col_types = FALSE) %>%
  filter(Year %in% yrs_common) %>%
  mutate(
    FIPS = as.character(FIPS),
    Crop = crop
  ) %>%
  select(
    Year, FIPS, Crop,
    GDD,
    precip_gs_mm,
    any_of(c("GDD_sd", "precip_gs_mm_sd", "n_gcms"))
  )

# ---- 6) Combined county table (wide-ish, mostly for debugging/training joins) ----
df_combined_county <- df_landcover_all_class %>%
  left_join(df_irrigation_in, by = c("Year","FIPS","LandCoverGroup","Crop")) %>%
  left_join(df_fert_county,   by = c("Year","FIPS","Crop")) %>%
  left_join(df_crop_climate, by = c("Year","FIPS","Crop")) %>%
  left_join(df_climate_county,by = c("Year","FIPS"))

# ---- 7) Basin aggregation (TEMPORARY area weights) ----
# NOTE: This replicates your current approach: county area is derived from landcover totals.
#       Later you’ll swap this with true "county-in-watershed" clipped areas.
df_county_areas <- df_landcover_county %>%
  group_by(FIPS) %>%
  summarise(area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop")

df_climate_basin <- df_climate_county %>%
  left_join(df_county_areas, by = "FIPS") %>%
  group_by(Year) %>%
  summarise(
    Climate_precip_m = wmean(precip_m,   area_m2),
    Climate_Tmin_C   = wmean(tmin_C_avg, area_m2),
    Climate_Tmax_C   = wmean(tmax_C_avg, area_m2),
    .groups = "drop"
  ) %>%
  mutate(Climate_Tmean_C = (Climate_Tmin_C + Climate_Tmax_C) / 2)

# Basin irrigation depth: area-weighted over corn+soy area (as in your script)
# Basin irrigation totals from actual alluvial water-use volume and irrigated area.
# IMPORTANT:
# - waterUse_m3 is actual groundwater withdrawal volume
# - irrArea_ha is actual irrigated area
# - Management_irrigation_m is depth over irrigated area, not total crop area
df_irr_basin <- df_irrigation_alluvial %>%
  filter(Crop %in% c("Corn", "Soybeans")) %>%
  group_by(Year) %>%
  summarise(
    Irrigation_total_m3 = sum(waterUse_m3, na.rm = TRUE),
    IrrigArea_m2 = sum(irrArea_m2, na.rm = TRUE),
    Management_irrigation_m = if_else(
      IrrigArea_m2 > 0,
      Irrigation_total_m3 / IrrigArea_m2,
      0
    ),
    .groups = "drop"
  )



df_fert_basin <- df_landcover_county %>%
  filter(LandCover %in% fert_rate_tbl$Crop) %>%
  rename(Crop = LandCover) %>%
  left_join(fert_rate_tbl, by = "Crop") %>%
  group_by(Year) %>%
  summarise(
    Management_FertilizerUse_kg = sum(fert_kgHa * area_ha, na.rm = TRUE),
    .groups = "drop"
  )

df_landcover_basin_groups <- df_landcover_all_class %>%
  group_by(Year, LandCoverGroup) %>%
  summarise(group_area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = LandCoverGroup,
    values_from = group_area_m2,
    names_prefix = "LandCover_m2_"
  )

common_input_basin <- df_climate_basin %>%
  left_join(df_irr_basin,  by = "Year") %>%
  left_join(df_fert_basin, by = "Year") %>%
  left_join(df_landcover_basin_groups, by = "Year") %>%
  arrange(Year)

# Sanity checks
required_cols <- c("Year","Climate_precip_m","Climate_Tmean_C",
                   "Management_irrigation_m","Management_FertilizerUse_kg")
missing <- setdiff(required_cols, names(common_input_basin))
if (length(missing) > 0) stop("Missing required cols in basin table: ", paste(missing, collapse = ", "))

# ---- 8) Write outputs ----
readr::write_csv(df_combined_county, out_county)
readr::write_csv(common_input_basin, out_basin)
readr::write_csv(df_county_areas, out_areas)

message("Wrote:\n - ", out_county, "\n - ", out_basin, "\n - ", out_areas)
