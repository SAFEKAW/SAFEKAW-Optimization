suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(sf)
  library(terra)
  library(AOI)
  library(climateR)
  library(exactextractr)
  library(readr)
  library(ggplot2)
  library(pollen)
})

# ---- project helpers ----
# adjust these if your helper files live elsewhere
source(here("code", "paths+packages.R"))
source(here("code", "calc_gdd.R"))

# ---- settings ----
yrs_common <- 2006:2023
start_date <- "2006-01-01"
end_date   <- "2023-12-31"


# ---- paths ----
dir.create(here("data", "climate", "gridmet_raw"), recursive = TRUE, showWarnings = FALSE)

tmin_file   <- here("data", "climate", "gridmet_raw", "tmin.tif")
tmax_file   <- here("data", "climate", "gridmet_raw", "tmax.tif")
precip_file <- here("data", "climate", "gridmet_raw", "precip.tif")

download_gridmet <- !all(file.exists(c(tmin_file, tmax_file, precip_file)))
#download_gridmet <- FALSE #if already created


message("Raw raster targets:")
message(" - ", tmin_file)
message(" - ", tmax_file)
message(" - ", precip_file)
message("download_gridmet = ", download_gridmet)

out_climate <- here("data", "ClimateData_County.csv")
out_gdd     <- here("data", "gdd_all_gs.csv")

# ---- boundaries ----
sf_counties  <- st_read(here("data", "Boundary_EKSRBcounties.gpkg"), quiet = TRUE)
sf_watershed <- st_read(here("data", "Boundary_EKSRBwatershed.gpkg"), quiet = TRUE)
sf_corridor  <- st_read(here("data", "Boundary_EKSRBalluvialCorridor.gpkg"), quiet = TRUE)

# make sure CRS align
if (st_crs(sf_counties) != st_crs(sf_watershed)) {
  sf_watershed <- st_transform(sf_watershed, st_crs(sf_counties))
}
if (st_crs(sf_corridor) != st_crs(sf_counties)) {
  sf_corridor <- st_transform(sf_corridor, st_crs(sf_counties))
}

clipped_counties <- st_intersection(sf_counties, sf_watershed)

# optional check plot
ggplot() +
  geom_sf(data = sf_watershed, fill = "lightblue", color = "blue", alpha = 0.3) +
  geom_sf(data = clipped_counties, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = sf_corridor, fill = "orange", color = "red", alpha = 0.3) +
  theme_minimal() +
  labs(
    title = "EKSRB watershed, clipped counties, and alluvial corridor",
    caption = "Data: SafeKAW"
  )

# ---- AOI for GridMET ----
bbox <- st_bbox(st_transform(sf_watershed, 4269)) %>%
  bbox_get()

# ---- download raw GridMET rasters if needed ----
if (download_gridmet) {
  message("Downloading GridMET daily rasters...")
  
  dat <- getGridMET(
    AOI = bbox,
    varname = c("pr", "tmmn", "tmmx"),
    startDate = start_date,
    endDate   = end_date
  )
  
  gm_precip <- project(dat$precipitation_amount, "epsg:4269")
  gm_tmin   <- project(dat$daily_minimum_temperature, "epsg:4269")
  gm_tmax   <- project(dat$daily_maximum_temperature, "epsg:4269")
  
  writeRaster(gm_precip, precip_file, overwrite = TRUE)
  writeRaster(gm_tmin,   tmin_file,   overwrite = TRUE)
  writeRaster(gm_tmax,   tmax_file,   overwrite = TRUE)
}

# ---- load rasters ----
mintemp <- rast(tmin_file)
maxtemp <- rast(tmax_file)
ppt     <- rast(precip_file)

# ensure vectors and rasters use same CRS
clipped_counties_4269 <- st_transform(clipped_counties, 4269)

# ---- helper to extract daily county means ----
extract_daily_means <- function(rast_obj, polygons, value_name, prefix_regex) {
  ext <- exact_extract(rast_obj, polygons, "mean")
  out <- cbind(polygons, ext) %>%
    st_drop_geometry()
  
  # keep only id columns + extracted mean columns
  id_cols <- intersect(c("name", "NAME", "FIPS"), names(out))
  mean_cols <- names(out)[grepl("^mean\\.", names(out))]
  
  out_long <- out %>%
    dplyr::select(all_of(id_cols), all_of(mean_cols)) %>%
    pivot_longer(
      cols = all_of(mean_cols),
      names_to = "layer_name",
      values_to = value_name
    ) %>%
    mutate(
      Date = str_remove(layer_name, prefix_regex),
      Date = ymd(Date),
      Year = year(Date),
      Month = month(Date),
      Day = day(Date)
    )
  
  # standardize county name column
  if ("NAME" %in% names(out_long) && !"name" %in% names(out_long)) {
    out_long <- out_long %>% rename(name = NAME)
  }
  
  out_long %>%
    mutate(FIPS = as.character(FIPS)) %>%
    dplyr::select(name, FIPS, Date, Year, Month, Day, all_of(value_name))
}

# ---- extract daily county means ----
message("Extracting daily county means for tmin...")
tmin_means <- extract_daily_means(
  rast_obj = mintemp,
  polygons = clipped_counties_4269,
  value_name = "tmin_K",
  prefix_regex = "^mean\\.tmmn_"
)

message("Extracting daily county means for tmax...")
tmax_means <- extract_daily_means(
  rast_obj = maxtemp,
  polygons = clipped_counties_4269,
  value_name = "tmax_K",
  prefix_regex = "^mean\\.tmmx_"
)

message("Extracting daily county means for precipitation...")
ppt_means <- extract_daily_means(
  rast_obj = ppt,
  polygons = clipped_counties_4269,
  value_name = "precip_mm_gridmet",
  prefix_regex = "^mean\\.pr_"
)

# ---- combine into daily county climate table ----
climate_daily <- tmin_means %>%
  left_join(
    tmax_means,
    by = c("name", "FIPS", "Date", "Year", "Month", "Day")
  ) %>%
  left_join(
    ppt_means,
    by = c("name", "FIPS", "Date", "Year", "Month", "Day")
  ) %>%
  mutate(
    tmmn_C_gridmet = tmin_K - 273.15,
    tmmx_C_gridmet = tmax_K - 273.15
  ) %>%
  filter(Year %in% yrs_common) %>%
  dplyr::select(
    FIPS, name, Date, Year, Month, Day,
    precip_mm_gridmet,
    tmmn_C_gridmet,
    tmmx_C_gridmet
  ) %>%
  arrange(FIPS, Date)

# ---- quick checks ----
print(names(climate_daily))
print(dplyr::glimpse(climate_daily))

climate_daily %>%
  summarise(
    min_year = min(Year, na.rm = TRUE),
    max_year = max(Year, na.rm = TRUE),
    n_counties = n_distinct(FIPS),
    n_rows = n()
  ) %>%
  print()

# ---- write county climate file ----
write_csv(climate_daily, out_climate)
message("Wrote climate file: ", out_climate)



# ---- outputs ----
out_crop_climate <- here("data", "crop_climate_gs_hist_baseline.csv")

# ---- helper to summarize one crop growing season ----
calc_crop_climate_gs <- function(df,
                                 crop,
                                 gs_start,
                                 gs_end,
                                 county_col = "FIPS",
                                 date_col   = "Date",
                                 tmin_col   = "tmin_C",
                                 tmax_col   = "tmax_C",
                                 precip_col = "precip_mm") {
  
  # GDD part comes from your helper
  gdd_df <- calc_gdd_county_year(
    df = df,
    crop = crop,
    county_col = county_col,
    date_col   = date_col,
    tmin_col   = tmin_col,
    tmax_col   = tmax_col,
    season     = "growing_season",
    gs_start   = gs_start,
    gs_end     = gs_end
  )
  
  # precip part uses the same crop-specific window
  ppt_df <- df %>%
    mutate(
      .date = as.Date(.data[[date_col]]),
      Year  = lubridate::year(.date),
      .start = as.Date(paste0(Year, "-", gs_start)),
      .end   = as.Date(paste0(Year, "-", gs_end))
    ) %>%
    filter(.date >= .start, .date <= .end) %>%
    group_by(across(all_of(county_col)), Year) %>%
    summarise(
      crop = crop,
      precip_gs_mm = sum(.data[[precip_col]], na.rm = TRUE),
      ppt_n_days = sum(!is.na(.data[[precip_col]])),
      .groups = "drop"
    )
  
  gdd_df %>%
    left_join(ppt_df, by = c(county_col, "Year", "crop"))
}

# ---- prep daily inputs for crop-season summaries ----
climate_crop_daily <- climate_daily %>%
  transmute(
    name      = name,
    FIPS      = as.character(FIPS),
    Date      = Date,
    Year      = Year,
    Month     = Month,
    Day       = Day,
    tmin_C    = tmmn_C_gridmet,
    tmax_C    = tmmx_C_gridmet,
    precip_mm = precip_mm_gridmet
  )

# ---- crop-specific seasonal summaries ----
crop_climate_corn <- calc_crop_climate_gs(
  df       = climate_crop_daily,
  crop     = "Corn",
  gs_start = "04-01",
  gs_end   = "10-31"
)

crop_climate_soy <- calc_crop_climate_gs(
  df       = climate_crop_daily,
  crop     = "Soybeans",
  gs_start = "04-01",
  gs_end   = "10-31"
)

crop_climate_sorg <- calc_crop_climate_gs(
  df       = climate_crop_daily,
  crop     = "Sorghum",
  gs_start = "04-01",
  gs_end   = "10-31"
)

crop_climate_wheat <- calc_crop_climate_gs(
  df       = climate_crop_daily,
  crop     = "Wheat",
  gs_start = "01-01",
  gs_end   = "06-30"
)

crop_climate_all <- bind_rows(
  crop_climate_corn,
  crop_climate_soy,
  crop_climate_sorg,
  crop_climate_wheat
) %>%
  mutate(FIPS = as.character(FIPS)) %>%
  dplyr::select(FIPS, Year, crop, GDD, n_days, precip_gs_mm, ppt_n_days) %>%
  arrange(crop, FIPS, Year)

# ---- quick checks ----
crop_climate_all %>%
  group_by(crop) %>%
  summarise(
    gdd_min   = min(GDD, na.rm = TRUE),
    gdd_max   = max(GDD, na.rm = TRUE),
    gdd_mean  = mean(GDD, na.rm = TRUE),
    ppt_min   = min(precip_gs_mm, na.rm = TRUE),
    ppt_max   = max(precip_gs_mm, na.rm = TRUE),
    ppt_mean  = mean(precip_gs_mm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

write_csv(crop_climate_all, out_crop_climate)
message("Wrote crop growing-season climate file: ", out_crop_climate)

# optional: keep writing the legacy GDD-only file too
gdd_all_gs <- crop_climate_all %>%
  dplyr::select(FIPS, Year, crop, GDD, n_days)

write_csv(gdd_all_gs, out_gdd)
message("Wrote GDD file: ", out_gdd)
