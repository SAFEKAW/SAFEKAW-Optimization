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

overwrite_extracts <- TRUE
# ---- project helpers ----
source(here("code", "paths+packages.R"))
source(here("code", "calc_gdd.R"))

#for manual testing
scenario_tag <- "rcp85_late" #rcp45_early, rcp45_mid, rcp45_late, #rcp85_early, rcp85_mid,  rcp85_late
maca_model   <- "GFDL-ESM2M"  #  GFDL-ESM2M	gfdl_esm2m
gcm_name     <- "gfdl_esm2m"
#CCSM4	ccsm4,BCC-CSM1-1	bcc_csm1_1, CanESM2	canesm2, 

# ---- scenario settings from command line ----
#args <- commandArgs(trailingOnly = TRUE)

#get_arg <- function(flag, default = NULL) {
#  i <- match(flag, args)
#  if (is.na(i) || i == length(args)) return(default)
#  args[[i + 1]]
#}

#scenario_tag <- get_arg("--scenario_tag", "rcp45_early")
#maca_model   <- get_arg("--maca_model", "CCSM4")
#gcm_name     <- get_arg("--gcm_name", "ccsm4")

valid_scenarios <- c(
  "rcp45_early", "rcp45_mid", "rcp45_late",
 "rcp85_early", "rcp85_mid", "rcp85_late")


if (!scenario_tag %in% valid_scenarios) {
  stop("This MACA script is for future scenarios only. Use the GridMET script for hist_baseline.")
}

maca_rcp <- case_when(
  str_detect(scenario_tag, "^rcp45_") ~ "rcp45",
  str_detect(scenario_tag, "^rcp85_") ~ "rcp85",
  TRUE ~ NA_character_
)

if (is.na(maca_rcp)) {
  stop("Could not infer MACA RCP from scenario_tag: ", scenario_tag)
}

period_lookup <- tibble::tribble(
  ~period, ~start_date,   ~end_date,
  "early", "2025-01-01",  "2049-12-31",
  "mid",   "2050-01-01",  "2074-12-31",
  "late",  "2075-01-01",  "2099-12-31"
)

period_name <- str_remove(scenario_tag, "^rcp45_|^rcp85_")

this_period <- period_lookup %>%
  filter(period == period_name)

if (nrow(this_period) != 1) {
  stop("scenario_tag period not found in period_lookup: ", scenario_tag)
}

start_date <- this_period$start_date[[1]]
end_date   <- this_period$end_date[[1]]
yrs_common <- seq(year(ymd(start_date)), year(ymd(end_date)))

message("Running MACA scenario:")
message("  scenario_tag = ", scenario_tag)
message("  maca_rcp     = ", maca_rcp)
message("  maca_model   = ", maca_model)
message("  gcm_name     = ", gcm_name)
message("  start_date   = ", start_date)
message("  end_date     = ", end_date)




# ---- paths ----
raw_dir <- here("data", "climate", "maca_raw", gcm_name, scenario_tag)
tmp_dir <- here("data", "climate", "maca_tmp", gcm_name, scenario_tag)

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

tmin_file   <- here(raw_dir, paste0("tmin_", gcm_name, "_", scenario_tag, ".tif"))
tmax_file   <- here(raw_dir, paste0("tmax_", gcm_name, "_", scenario_tag, ".tif"))
precip_file <- here(raw_dir, paste0("precip_", gcm_name, "_", scenario_tag, ".tif"))

#download_maca <- !all(file.exists(c(tmin_file, tmax_file, precip_file)))
download_maca <- TRUE

message("Raw raster targets:")
message(" - ", tmin_file)
message(" - ", tmax_file)
message(" - ", precip_file)
message("download_maca = ", download_maca)

out_climate <- here("data", paste0("ClimateData_County_", gcm_name, "_", scenario_tag, ".csv"))
out_gdd     <- here("data", paste0("gdd_all_gs_", gcm_name, "_", scenario_tag, ".csv"))
out_crop_climate <- here(  "data", paste0("crop_climate_gs_", gcm_name, "_", scenario_tag, ".csv"))

# optional: give GDAL a bit more cache
terra::gdalCache(4096)

# ---- boundaries ----
sf_counties  <- st_read(here("data", "Boundary_EKSRBcounties.gpkg"), quiet = TRUE)
sf_watershed <- st_read(here("data", "Boundary_EKSRBwatershed.gpkg"), quiet = TRUE)
sf_corridor  <- st_read(here("data", "Boundary_EKSRBalluvialCorridor.gpkg"), quiet = TRUE)

if (st_crs(sf_counties) != st_crs(sf_watershed)) {
  sf_watershed <- st_transform(sf_watershed, st_crs(sf_counties))
}
if (st_crs(sf_corridor) != st_crs(sf_counties)) {
  sf_corridor <- st_transform(sf_corridor, st_crs(sf_counties))
}

clipped_counties <- st_intersection(sf_counties, sf_watershed)

# optional check plot
#ggplot() +
 # geom_sf(data = sf_watershed, fill = "lightblue", color = "blue", alpha = 0.3) +
#  geom_sf(data = clipped_counties, fill = NA, color = "black", linewidth = 0.4) +
#  geom_sf(data = sf_corridor, fill = "orange", color = "red", alpha = 0.3) +
#  theme_minimal() +
 # labs(
#    title = paste("EKSRB counties for", scenario_tag),
#    caption = "Data: SafeKAW + MACA"
#  )

# ---- AOI for MACA ----
bbox <- st_bbox(st_transform(sf_watershed, 4269)) %>%
  bbox_get()

# ---- download raw MACA rasters if needed ----
if (download_maca) {
  message("Downloading MACA daily rasters...")
  
  dat <- getMACA(
    AOI = bbox,
    model = maca_model,
    scenario = maca_rcp,
    varname = c("pr", "tasmin", "tasmax"),
    startDate = start_date,
    endDate   = end_date
  )
  
  maca_precip <- project(dat$precipitation, "epsg:4269")
  
  air_names <- names(dat$air_temperature)
  tmin_idx <- str_detect(air_names, "tasmin")
  tmax_idx <- str_detect(air_names, "tasmax")
  
  if (!any(tmin_idx) || !any(tmax_idx)) {
    stop("Could not identify tasmin/tasmax layers in MACA air_temperature stack.")
  }
  
  maca_tmin <- subset(dat$air_temperature, which(tmin_idx))
  maca_tmax <- subset(dat$air_temperature, which(tmax_idx))
  
  test_tmin <- global(maca_tmin[[1]], "mean", na.rm = TRUE)[1, 1]
  test_tmax <- global(maca_tmax[[1]], "mean", na.rm = TRUE)[1, 1]
  
  message("Raw MACA first-layer means: tasmin = ", test_tmin, "; tasmax = ", test_tmax)
  
  maca_tmin <- project(maca_tmin, "epsg:4269")
  maca_tmax <- project(maca_tmax, "epsg:4269")
  
  writeRaster(maca_precip, precip_file, overwrite = TRUE)
  writeRaster(maca_tmin,   tmin_file,   overwrite = TRUE)
  writeRaster(maca_tmax,   tmax_file,   overwrite = TRUE)
}

# ---- load rasters ----
mintemp <- rast(tmin_file)
maxtemp <- rast(tmax_file)
ppt     <- rast(precip_file)

message("Loaded tmin first layer: ", names(mintemp)[1])
message("Loaded tmax first layer: ", names(maxtemp)[1])

# ---- robust swap check using raster means ----
test_tmin <- terra::global(mintemp[[1]], "mean", na.rm = TRUE)[1,1]
test_tmax <- terra::global(maxtemp[[1]], "mean", na.rm = TRUE)[1,1]

message("Initial raster means: tmin = ", test_tmin, "; tmax = ", test_tmax)

if (is.finite(test_tmin) && is.finite(test_tmax) && test_tmin > test_tmax) {
  message("Detected swapped MACA rasters → correcting")
  
  tmp <- mintemp
  mintemp <- maxtemp
  maxtemp <- tmp
}

clipped_counties_4269 <- st_transform(clipped_counties, 4269)

# ---- helper: extract one variable, one year at a time ----
extract_daily_means_maca_by_year_fast <- function(rast_obj, polygons, value_name, years_vec, tmp_dir) {
  lyr_names <- names(rast_obj)
  
  poly_ids <- polygons %>%
    st_drop_geometry()
  
  if ("NAME" %in% names(poly_ids) && !"name" %in% names(poly_ids)) {
    poly_ids <- poly_ids %>% rename(name = NAME)
  }
  
  poly_ids <- poly_ids %>%
    dplyr::select(any_of(c("name", "FIPS"))) %>%
    mutate(FIPS = as.character(FIPS))
  
  out_year_files <- character(length(years_vec))
  
  for (y_idx in seq_along(years_vec)) {
    yy <- years_vec[y_idx]
    out_file <- file.path(tmp_dir, paste0(value_name, "_", yy, ".csv"))
    
    if (file.exists(out_file) && !isTRUE(overwrite_extracts)) {
      message("Using existing yearly extract for ", value_name, " ", yy)
      out_year_files[y_idx] <- out_file
      next
    }
    
    message("Processing year ", yy, " for ", value_name, "...")
    
    keep_idx <- which(str_detect(lyr_names, paste0("_", yy, "-")))
    
    if (length(keep_idx) == 0) {
      stop("No layers found for year ", yy, " in ", value_name)
    }
    
    r_year <- rast_obj[[keep_idx]]
    
    vals <- exactextractr::exact_extract(
      r_year,
      polygons,
      "mean",
      progress = FALSE
    )
    
    vals_df <- as_tibble(vals)
    
    dates <- str_extract(names(r_year), "\\d{4}-\\d{2}-\\d{2}") %>%
      lubridate::ymd()
    
    if (any(is.na(dates))) {
      stop("Could not parse dates from raster layer names for ", value_name, " ", yy)
    }
    
    names(vals_df) <- as.character(dates)
    
    year_df <- bind_cols(poly_ids, vals_df) %>%
      pivot_longer(
        cols = all_of(as.character(dates)),
        names_to = "Date",
        values_to = value_name
      ) %>%
      mutate(
        Date = as.Date(Date),
        Year = year(Date),
        Month = month(Date),
        Day = day(Date)
      )
    
    write_csv(year_df, out_file)
    out_year_files[y_idx] <- out_file
    
    rm(r_year, vals, vals_df, year_df)
    gc()
  }
  
  bind_rows(lapply(out_year_files, read_csv, show_col_types = FALSE))
}

# ---- extract daily county means by year ----
tmin_means <- extract_daily_means_maca_by_year_fast(
  rast_obj = mintemp,
  polygons = clipped_counties_4269,
  value_name = "tmin_K",
  years_vec = yrs_common,
  tmp_dir = tmp_dir
)

tmax_means <- extract_daily_means_maca_by_year_fast(
  rast_obj = maxtemp,
  polygons = clipped_counties_4269,
  value_name = "tmax_K",
  years_vec = yrs_common,
  tmp_dir = tmp_dir
)

ppt_means <- extract_daily_means_maca_by_year_fast(
  rast_obj = ppt,
  polygons = clipped_counties_4269,
  value_name = "precip_mm_gridmet",
  years_vec = yrs_common,
  tmp_dir = tmp_dir
)

# ---- duplicate / NA checks ----
stopifnot(
  nrow(tmin_means %>% count(FIPS, Date) %>% filter(n > 1)) == 0,
  nrow(tmax_means %>% count(FIPS, Date) %>% filter(n > 1)) == 0,
  nrow(ppt_means  %>% count(FIPS, Date) %>% filter(n > 1)) == 0
)

stopifnot(
  sum(is.na(tmin_means$Date)) == 0,
  sum(is.na(tmax_means$Date)) == 0,
  sum(is.na(ppt_means$Date)) == 0
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


# ---- temperature QC checks ----
temp_qc <- climate_daily %>%
  summarise(
    tmin_min = min(tmmn_C_gridmet, na.rm = TRUE),
    tmin_mean = mean(tmmn_C_gridmet, na.rm = TRUE),
    tmin_max = max(tmmn_C_gridmet, na.rm = TRUE),
    tmax_min = min(tmmx_C_gridmet, na.rm = TRUE),
    tmax_mean = mean(tmmx_C_gridmet, na.rm = TRUE),
    tmax_max = max(tmmx_C_gridmet, na.rm = TRUE),
    n_tmin_gt_tmax = sum(tmmn_C_gridmet > tmmx_C_gridmet, na.rm = TRUE),
    frac_tmin_gt_tmax = mean(tmmn_C_gridmet > tmmx_C_gridmet, na.rm = TRUE)
  )

print(temp_qc)

if (temp_qc$n_tmin_gt_tmax > 0) {
  stop("Temperature QC failed: some daily tmin values exceed tmax.")
}

if (temp_qc$tmin_min < -50 || temp_qc$tmax_max > 60) {
  warning("Temperature QC warning: temperatures are outside plausible Celsius range.")
}

# ---- quick checks ----
print(names(climate_daily))
print(glimpse(climate_daily))

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
  
  # GDD from helper
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
  
  # cumulative growing-season precipitation over same window
  ppt_df <- df %>%
    mutate(
      .date  = as.Date(.data[[date_col]]),
      Year   = lubridate::year(.date),
      .start = as.Date(paste0(Year, "-", gs_start)),
      .end   = as.Date(paste0(Year, "-", gs_end))
    ) %>%
    filter(.date >= .start, .date <= .end) %>%
    group_by(across(all_of(county_col)), Year) %>%
    summarise(
      crop = crop,
      precip_gs_mm = sum(.data[[precip_col]], na.rm = TRUE),
      ppt_n_days   = sum(!is.na(.data[[precip_col]])),
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

if (nrow(crop_climate_all) == 0) {
  stop("Crop climate table is empty; check MACA dates and climate extraction.")
}

# ---- quick checks ----
crop_climate_all %>%
  group_by(crop) %>%
  summarise(
    gdd_min  = min(GDD, na.rm = TRUE),
    gdd_max  = max(GDD, na.rm = TRUE),
    gdd_mean = mean(GDD, na.rm = TRUE),
    ppt_min  = min(precip_gs_mm, na.rm = TRUE),
    ppt_max  = max(precip_gs_mm, na.rm = TRUE),
    ppt_mean = mean(precip_gs_mm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

write_csv(crop_climate_all, out_crop_climate)
message("Wrote crop growing-season climate file: ", out_crop_climate)

# ---- optional legacy GDD-only file ----
gdd_all_gs <- crop_climate_all %>%
  dplyr::select(FIPS, Year, crop, GDD, n_days)

write_csv(gdd_all_gs, out_gdd)
message("Wrote GDD file: ", out_gdd)



