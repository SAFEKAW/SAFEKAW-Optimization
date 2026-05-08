# ---- setup ----
library(dplyr)
library(ggplot2)
library(here)
library(sf)

source(here("hpc_opt","R","18_irrig_allocation.R"))

# Getting alluvial aquifer extent
# ---- boundaries ----
sf_counties  <- st_read(here("data", "Boundary_EKSRBcounties.gpkg"), quiet = TRUE)
sf_watershed <- st_read(here("data", "Boundary_EKSRBwatershed.gpkg"), quiet = TRUE)
sf_corridor  <- st_read(here("data", "Boundary_EKSRBalluvialCorridor.gpkg"), quiet = TRUE)

# ---- clean geometries ----
sf_counties  <- st_make_valid(sf_counties)
sf_watershed <- st_make_valid(sf_watershed)
sf_corridor  <- st_make_valid(sf_corridor)

# ---- align CRS for plotting/intersection ----
if (st_crs(sf_counties) != st_crs(sf_watershed)) {
  sf_watershed <- st_transform(sf_watershed, st_crs(sf_counties))
}
if (st_crs(sf_corridor) != st_crs(sf_counties)) {
  sf_corridor <- st_transform(sf_corridor, st_crs(sf_counties))
}

# ---- clip counties and corridor to watershed ----
clipped_counties <- st_intersection(sf_counties, sf_watershed)

corridor_in_watershed <- st_intersection(sf_corridor, sf_watershed)

# ---- calculate alluvial corridor area using equal-area CRS ----
corridor_union <- corridor_in_watershed %>%
  st_transform(5070) %>%
  st_union()

alluvial_area_m2  <- as.numeric(st_area(corridor_union))
alluvial_area_km2 <- alluvial_area_m2 / 1e6
alluvial_area_ha  <- alluvial_area_m2 / 1e4

alluvial_area_m2
alluvial_area_km2
alluvial_area_ha


ggplot() +
  geom_sf(data = sf_watershed, fill = "lightblue", color = "blue", alpha = 0.3) +
  geom_sf(data = clipped_counties, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = corridor_in_watershed, fill = "orange", color = "red", alpha = 0.3) +
  theme_minimal() +
  labs(
    title = "EKSRB watershed, clipped counties, and alluvial corridor",
    subtitle = paste0(
      "Alluvial corridor area = ",
      round(alluvial_area_km2, 1), " km² / ",
      round(alluvial_area_ha, 0), " ha"
    ),
    caption = "Data: SafeKAW"
  )



#calcing & checking cap
# ---- load baseline ----
precomp_hist <- readRDS(
  here("hpc_opt","outputs","precompute","precomp_hist_baseline.rds")
)

lu_baseline <- precomp_hist$lu_baseline
baseline_irrig_frac <- precomp_hist$baseline_irrig_frac
hist_mix <- precomp_hist$hist_mix

# ---- load historical county inputs ----
county_hist <- read_csv(
  here("hpc_opt","outputs","common_inputs_county_hist_baseline.csv"),
  show_col_types = FALSE
)

df_county <- county_hist %>%
  mutate(FIPS = as.character(FIPS))

# ---- function ----
calc_irrigated_area_year <- function(Y) {
  crops4 <- c("Corn", "Soybeans", "Sorghum", "Wheat")
  
  df_crop_y <- df_county %>%
    filter(Year == Y, Crop %in% crops4)
  
  lu_y <- lu_baseline %>% filter(Year == Y)
  Cult_m2 <- lu_y$LC_cult_base
  
  shares <- df_crop_y %>%
    group_by(Crop) %>%
    summarise(area = sum(area_m2, na.rm = TRUE), .groups = "drop") %>%
    mutate(share = area / sum(area, na.rm = TRUE)) %>%
    select(Crop, share) %>%
    tibble::deframe()
  
  shares <- shares[c("Corn", "Soybeans", "Sorghum", "Wheat")]
  shares[is.na(shares)] <- 0
  
  alloc <- allocate_irrigation(
    crop_shares = shares,
    theta_irrig = baseline_irrig_frac,
    hist_mix    = hist_mix,
    mode        = "preserve_hist_mix"
  )
  
  Cult_m2 * sum(alloc$irrig)
}

# ---- run ----
years_check <- 2015:2023

irrig_area <- sapply(years_check, calc_irrigated_area_year)

df_plot <- data.frame(
  Year = years_check,
  irrig_area_km2 = irrig_area / 1e6
)

# ---- plot ----
ggplot(df_plot, aes(Year, irrig_area_km2)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = alluvial_area_m2 / 1e6,
             linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    y = "Irrigated area (km²)",
    title = "Baseline irrigated area vs aquifer cap"
  )


df_check <- df_county %>%
  filter(Crop %in% c("Corn", "Soybeans", "Sorghum", "Wheat")) %>%
  mutate(
    is_irrigated = irrigation_WaterUse_m > 0
  ) %>%
  group_by(Year) %>%
  summarise(
    total_area_m2 = sum(area_m2, na.rm = TRUE),
    irrig_area_m2 = sum(area_m2[is_irrigated], na.rm = TRUE),
    irrig_frac = irrig_area_m2 / total_area_m2,
    irrig_area_km2 = irrig_area_m2 / 1e6,
    .groups = "drop"
  )

ggplot(df_check, aes(Year)) +
  geom_line(aes(y = irrig_area_km2), size = 1.2) +
  geom_hline(
    yintercept = alluvial_area_m2 / 1e6,
    linetype = "dashed", color = "red"
  ) +
  labs(
    y = "Irrigated area (km²)",
    title = "Observed irrigated area vs aquifer cap",
    subtitle = "From irrigation_water_use_m > 0"
  ) +
  theme_minimal()

baseline_irrig_frac
mean(df_check$irrig_frac, na.rm = TRUE)

scripts <- list.files(
  here("hpc_opt", "scripts"),
  pattern = "\\.R$",
  recursive = TRUE,
  full.names = TRUE
)

for (s in scripts) {
  txt <- paste(readLines(s, warn = FALSE), collapse = "\n")
  if (grepl("common_inputs_county|df_county|irrigation_water_use_m", txt)) {
    cat("\n\n---", s, "---\n")
    cat(grep("common_inputs_county|df_county|irrigation_water_use_m", 
             readLines(s, warn = FALSE), value = TRUE), sep = "\n")
  }
}


df_county %>%
  count(Year, FIPS, Crop) %>%
  filter(n > 1) %>%
  arrange(desc(n))

df_county %>%
  filter(Crop %in% c("Corn", "Soybeans", "Sorghum", "Wheat")) %>%
  group_by(Year) %>%
  summarise(
    total_crop_area_km2 = sum(area_m2, na.rm = TRUE) / 1e6,
    .groups = "drop"
  )

lu_baseline %>%
  transmute(
    Year,
    LC_cult_base_km2 = LC_cult_base / 1e6
  )

df_county %>%
  filter(Crop %in% c("Corn", "Soybeans", "Sorghum", "Wheat")) %>%
  summarise(
    min_irr = min(irrigation_WaterUse_m, na.rm = TRUE),
    mean_irr = mean(irrigation_WaterUse_m, na.rm = TRUE),
    max_irr = max(irrigation_WaterUse_m, na.rm = TRUE),
    n_positive = sum(irrigation_WaterUse_m > 0, na.rm = TRUE),
    n_total = n()
  )
df_county %>%
  filter(Crop %in% c("Corn", "Soybeans", "Sorghum", "Wheat")) %>%
  group_by(Crop) %>%
  summarise(
    mean_irr_m = mean(irrigation_WaterUse_m, na.rm = TRUE),
    max_irr_m = max(irrigation_WaterUse_m, na.rm = TRUE),
    frac_rows_positive = mean(irrigation_WaterUse_m > 0, na.rm = TRUE),
    .groups = "drop"
  )


df_county %>%
  filter(Crop %in% c("Corn", "Soybeans", "Sorghum", "Wheat")) %>%
  mutate(
    irrigation_m3_total = irrigation_WaterUse_m * area_m2
  ) %>%
  group_by(Year) %>%
  summarise(
    crop_area_km2 = sum(area_m2, na.rm = TRUE) / 1e6,
    positive_irr_area_km2 = sum(area_m2[irrigation_WaterUse_m > 0], na.rm = TRUE) / 1e6,
    total_irrigation_m3 = sum(irrigation_m3_total, na.rm = TRUE),
    mean_irr_depth_m_positive = weighted.mean(
      irrigation_WaterUse_m[irrigation_WaterUse_m > 0],
      area_m2[irrigation_WaterUse_m > 0],
      na.rm = TRUE
    ),
    .groups = "drop"
  )


df_county %>%
  filter(
    Crop %in% c("Corn", "Soybeans", "Sorghum", "Wheat"),
    irrigation_WaterUse_m > 0
  ) %>%
  select(Year, FIPS, Crop, area_m2, area_ha, irrigation_WaterUse_m, n_pdiv) %>%
  arrange(Year, FIPS, Crop) %>%
  print(n = 50)


df_county %>%
  filter(
    Crop %in% c("Corn", "Soybeans", "Sorghum", "Wheat"),
    irrigation_WaterUse_m > 0
  ) %>%
  summarise(
    positive_rows = n(),
    positive_area_km2 = sum(area_m2, na.rm = TRUE) / 1e6,
    mean_depth_m = mean(irrigation_WaterUse_m, na.rm = TRUE),
    median_depth_m = median(irrigation_WaterUse_m, na.rm = TRUE),
    max_depth_m = max(irrigation_WaterUse_m, na.rm = TRUE)
  )


# ---- feasible alluvial irrigation cap from land cover table ----

wu_alluvial <- readr::read_csv(
  here::here("data", "WaterUseByCrop_AlluvialCorridor.csv"),
  show_col_types = FALSE
)


alluvial_lu <- readr::read_csv(
  here("data", "LandCoverData-CDL_AlluvialCorridor.csv"),  # update filename
  show_col_types = FALSE
)

feasible_classes <- c(
  "Corn",
  "Soybeans",
  "Sorghum",
  "Wheat",
  "Other Crops",
  "Grass/Pasture/Shrubland", 
  "Fallow/Barren"
  
)

infeasible_classes <- c(
  "Developed",
  "Water",
  "Wetlands",
  "Forest"
)

feasible_cap_by_year <- alluvial_lu %>%
  mutate(
    feasible = LandCover %in% feasible_classes
  ) %>%
  group_by(Year) %>%
  summarise(
    total_alluvial_area_ha = sum(area_ha, na.rm = TRUE),
    feasible_area_ha = sum(area_ha[feasible], na.rm = TRUE),
    infeasible_area_ha = sum(area_ha[!feasible], na.rm = TRUE),
    feasible_frac = feasible_area_ha / total_alluvial_area_ha,
    feasible_cap_m2 = feasible_area_ha * 1e4,
    feasible_cap_km2 = feasible_cap_m2 / 1e6,
    .groups = "drop"
  )

feasible_cap_summary <- feasible_cap_by_year %>%
  summarise(
    feasible_cap_mean_m2 = mean(feasible_cap_m2, na.rm = TRUE),
    feasible_cap_recent_m2 = feasible_cap_m2[Year == max(Year, na.rm = TRUE)],
    feasible_cap_min_m2 = min(feasible_cap_m2, na.rm = TRUE),
    feasible_cap_max_m2 = max(feasible_cap_m2, na.rm = TRUE)
  )

feasible_cap_m2 <- feasible_cap_summary$feasible_cap_recent_m2
feasible_cap_km2 <- feasible_cap_m2 / 1e6
feasible_cap_ha <- feasible_cap_m2 / 1e4

feasible_cap_summary
feasible_cap_m2

feasible_cap_km2
feasible_cap_ha




wu_alluvial %>%
  filter(Crop %in% c("Corn", "Soybeans")) %>%
  group_by(Year) %>%
  summarise(
    waterUse_m3 = sum(waterUse_m3, na.rm = TRUE),
    irrArea_m2 = sum(irrArea_ha, na.rm = TRUE) * 1e4,
    irrArea_km2 = irrArea_m2 / 1e6,
    mean_depth_m = waterUse_m3 / irrArea_m2,
    .groups = "drop"
  )

wu_check <- wu_alluvial %>%
  filter(Crop %in% c("Corn", "Soybeans")) %>%
  group_by(Year) %>%
  summarise(
    irrArea_m2 = sum(irrArea_ha, na.rm = TRUE) * 1e4,
    irrArea_km2 = irrArea_m2 / 1e6,
    waterUse_m3 = sum(waterUse_m3, na.rm = TRUE),
    mean_depth_m = waterUse_m3 / irrArea_m2,
    .groups = "drop"
  )

ggplot(wu_check, aes(Year, irrArea_km2)) +
  geom_line(linewidth = 1.2) +
  geom_hline(
    yintercept = alluvial_area_m2 / 1e6,
    linetype = "dashed",
    color = "black"
  ) +
  geom_hline(
    yintercept = feasible_cap_m2 / 1e6,
    linetype = "dotted",
    color = "red"
  ) +
  theme_minimal() +
  labs(
    y = "Irrigated area (km²)",
    title = "Observed irrigation vs alluvial area caps",
    subtitle = "Black dashed = total corridor; red dotted = feasible land cap"
  )


ggplot(alluvial_lu, aes(Year, area_ha / 100, fill = LandCover)) +
  geom_area(alpha = 0.9) +
  theme_minimal() +
  labs(
    y = "Area (km²)",
    title = "Alluvial corridor land cover through time",
    subtitle = "Used to estimate feasible irrigation expansion cap"
  )


#checking volume
# ---- 2) Land cover (county) ----
yrs_common <- 2006:2023

in_landcover <- here::here("data", "LandCoverData-CDL_County.csv")
in_irrig     <- here::here("data", "WaterUseData_County.csv")

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


# Basin irrigation depth: area-weighted over corn+soy area (as in your script)
cult_area_by_county_crop <- df_landcover_county %>%
  filter(LandCover %in% c("Corn","Soybeans")) %>%
  transmute(Year, FIPS, Crop = LandCover, area_m2)


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


df_irr_basin <- cult_area_by_county_crop %>%
  left_join(
    df_irrigation_in %>%
      filter(Crop %in% c("Corn","Soybeans")) %>%
      select(Year, FIPS, Crop, irrigation_WaterUse_m),
    by = c("Year","FIPS","Crop")
  ) %>%
  mutate(irrigation_WaterUse_m = coalesce(irrigation_WaterUse_m, 0)) %>%
  group_by(Year) %>%
  summarise(
    Management_irrigation_m = wmean(irrigation_WaterUse_m, area_m2),
    Irrigation_total_m3 = sum(irrigation_WaterUse_m * area_m2, na.rm = TRUE),
    
    # NOT actual irrigated area
    CropArea_m2 = sum(area_m2, na.rm = TRUE),
    
    # Still not true irrigated area, but useful diagnostic
    PositiveIrrCropArea_m2 = sum(area_m2[irrigation_WaterUse_m > 0], na.rm = TRUE),
    
    .groups = "drop"
  )




# volume from current common_inputs logic
vol_from_common_inputs <- cult_area_by_county_crop %>%
  left_join(
    df_irrigation_in %>%
      filter(Crop %in% c("Corn","Soybeans")) %>%
      select(Year, FIPS, Crop, irrigation_WaterUse_m),
    by = c("Year","FIPS","Crop")
  ) %>%
  mutate(irrigation_WaterUse_m = coalesce(irrigation_WaterUse_m, 0)) %>%
  group_by(Year) %>%
  summarise(
    volume_from_depth_m3 = sum(irrigation_WaterUse_m * area_m2, na.rm = TRUE),
    .groups = "drop"
  )

# trusted volume from alluvial water-use file
vol_from_alluvial <- wu_alluvial %>%
  filter(Crop %in% c("Corn", "Soybeans")) %>%
  group_by(Year) %>%
  summarise(
    volume_alluvial_m3 = sum(waterUse_m3, na.rm = TRUE),
    irr_area_m2 = sum(irrArea_ha, na.rm = TRUE) * 1e4,
    observed_depth_m = volume_alluvial_m3 / irr_area_m2,
    .groups = "drop"
  )

vol_compare <- vol_from_common_inputs %>%
  left_join(vol_from_alluvial, by = "Year") %>%
  mutate(
    ratio_common_to_alluvial = volume_from_depth_m3 / volume_alluvial_m3
  )

vol_compare

#volume is wayy over estimated!! 12-50x larger!!!

######


in_irrig_alluvial <- here::here("data", "WaterUseByCrop_AlluvialCorridor.csv")


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

df_irr_basin %>%
  summarise(
    min_irrig_m3 = min(Irrigation_total_m3, na.rm = TRUE),
    max_irrig_m3 = max(Irrigation_total_m3, na.rm = TRUE),
    min_irrig_area_km2 = min(IrrigArea_m2, na.rm = TRUE) / 1e6,
    max_irrig_area_km2 = max(IrrigArea_m2, na.rm = TRUE) / 1e6,
    min_depth_m = min(Management_irrigation_m, na.rm = TRUE),
    max_depth_m = max(Management_irrigation_m, na.rm = TRUE)
  ) %>%
  print()


vol_compare_2<- df_irr_basin %>%
  left_join(vol_from_alluvial, by = "Year") %>%
  mutate(
    ratio_common_to_alluvial = Irrigation_total_m3 / volume_alluvial_m3
  )

vol_compare_2
