# hpc_opt/scripts/01_common_inputs_make.R
# Build common inputs at county + basin scales (minimal, batchable).

source(here::here("hpc_opt", "R", "00_setup.R"))

# ---- I/O paths (relative to repo root) ----
in_climate   <- here::here("data", "ClimateData_County.csv")
in_landcover <- here::here("data", "LandCoverData-CDL_County.csv")
in_irrig     <- here::here("data", "WaterUseData_County.csv")
in_gdd       <- here::here("data", "gdd_all_gs.csv")

out_county   <- here::here("hpc_opt", "outputs", "common_inputs_county.csv")
out_basin    <- here::here("hpc_opt", "outputs", "common_inputs_basin.csv")
out_areas    <- here::here("hpc_opt", "outputs", "county_areas.csv")

dir.create(dirname(out_county), recursive = TRUE, showWarnings = FALSE)

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
df_landcover_county <- readr::read_csv(in_landcover, show_col_types = FALSE) %>%
  mutate(
    FIPS = as.character(FIPS),
    area_m2 = area_ha * 10000
  ) %>%
  select(Year, FIPS, LandCover, area_m2, area_ha, area_prc)

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
df_irrigation_in <- readr::read_csv(in_irrig, show_col_types = FALSE) %>%
  filter(Year %in% yrs_common, n_pdiv > 3) %>%
  mutate(
    FIPS = as.character(FIPS),
    irrigation_WaterUse_m = irrigation_mm_mean / 1000,
    LandCoverGroup = "all_cult"
  ) %>%
  select(Year, FIPS, Crop, LandCoverGroup, irrigation_WaterUse_m, n_pdiv)

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

# ---- 5) GDD (county; crop-specific) ----
df_gdd <- readr::read_csv(in_gdd, show_col_types = FALSE) %>%
  filter(Year %in% yrs_common) %>%
  mutate(
    FIPS = as.character(FIPS),
    Crop = crop
  ) %>%
  select(Year, FIPS, Crop, GDD)

# ---- 6) Combined county table (wide-ish, mostly for debugging/training joins) ----
df_combined_county <- df_landcover_all_class %>%
  left_join(df_irrigation_in, by = c("Year","FIPS","LandCoverGroup","Crop")) %>%
  left_join(df_fert_county,   by = c("Year","FIPS","Crop")) %>%
  left_join(df_gdd,           by = c("Year","FIPS","Crop")) %>%
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
cult_area_by_county_crop <- df_landcover_county %>%
  filter(LandCover %in% c("Corn","Soybeans")) %>%
  transmute(Year, FIPS, Crop = LandCover, area_m2)

df_irr_basin <- cult_area_by_county_crop %>%
  left_join(
    df_irrigation_in %>%
      filter(Crop %in% c("Corn","Soybeans")) %>%
      select(Year, FIPS, Crop, irrigation_WaterUse_m),
    by = c("Year","FIPS","Crop")
  ) %>%
  mutate(irrigation_WaterUse_m = dplyr::coalesce(irrigation_WaterUse_m, 0)) %>%
  group_by(Year) %>%
  summarise(
    Management_irrigation_m = wmean(irrigation_WaterUse_m, area_m2),
    Irrigation_total_m3     = sum(irrigation_WaterUse_m * area_m2, na.rm = TRUE),
    IrrigArea_m2            = sum(area_m2, na.rm = TRUE),
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
