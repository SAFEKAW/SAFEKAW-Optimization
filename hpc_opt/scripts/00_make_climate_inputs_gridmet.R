suppressPackageStartupMessages({
  # climateR must be the first spatial package attached in the KU Conda
  # environment to avoid an Rcpp/UDUNITS initialization error.
  library(climateR)
  library(AOI)
  library(here)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(sf)
  library(terra)
  library(exactextractr)
  library(readr)
  library(ggplot2)
})

# ---- project helpers ----
source(here("hpc_opt", "R", "calc_gdd.R"))

# ---- settings ----
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[[i + 1]]
}

smoke_year <- suppressWarnings(as.integer(get_arg("--smoke-year", NA_character_)))
is_smoke_test <- !is.na(smoke_year)

if (is_smoke_test) {
  yrs_common <- smoke_year
  start_date <- paste0(smoke_year - 1L, "-10-01")
  end_date <- paste0(smoke_year, "-12-31")
  run_tag <- paste0("smoke_", smoke_year)
} else {
  yrs_common <- 2006:2023
  start_date <- "2005-10-01"
  end_date <- "2023-12-31"
  run_tag <- "hist_baseline"
}

message("Running GridMET:")
message("  start_date = ", start_date)
message("  end_date   = ", end_date)
message("  smoke_test = ", is_smoke_test)


# ---- paths ----
climate_scratch <- Sys.getenv(
  "SAFEKAW_CLIMATE_SCRATCH",
  unset = here("data", "climate")
)
gridmet_raw_dir <- file.path(climate_scratch, "gridmet_raw_wheat_oct", run_tag)
dir.create(gridmet_raw_dir, recursive = TRUE, showWarnings = FALSE)

tmin_file   <- file.path(gridmet_raw_dir, "tmin.tif")
tmax_file   <- file.path(gridmet_raw_dir, "tmax.tif")
precip_file <- file.path(gridmet_raw_dir, "precip.tif")

download_gridmet <- !all(file.exists(c(tmin_file, tmax_file, precip_file)))
#download_gridmet <- FALSE #if already created


message("Raw raster targets:")
message(" - ", tmin_file)
message(" - ", tmax_file)
message(" - ", precip_file)
message("download_gridmet = ", download_gridmet)

output_dir <- if (is_smoke_test) {
  file.path(climate_scratch, "gridmet_smoke_outputs")
} else {
  here("data")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (is_smoke_test) {
  out_climate <- file.path(output_dir, paste0("ClimateData_County_", run_tag, ".csv"))
  out_gdd <- file.path(output_dir, paste0("gdd_all_gs_", run_tag, ".csv"))
  out_crop_climate <- file.path(output_dir, paste0("crop_climate_gs_", run_tag, ".csv"))
} else {
  # Preserve established filenames consumed by downstream historical-model
  # integration code.
  out_climate <- file.path(output_dir, "ClimateData_County.csv")
  out_gdd <- file.path(output_dir, "gdd_all_gs.csv")
out_crop_climate <- file.path(output_dir, "crop_climate_gs_hist_baseline.csv")
}

# The full historical stack benefits from a larger GDAL cache during county
# extraction. This remains comfortably below the 32 GB SLURM allocation.
terra::gdalCache(8192)

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

# ---- AOI for GridMET ----
bbox <- st_bbox(st_transform(sf_watershed, 4269)) %>%
  bbox_get()

# ---- download raw GridMET rasters if needed ----
if (download_gridmet) {
  message("Downloading GridMET daily rasters...")

  max_download_attempts <- suppressWarnings(as.integer(
    Sys.getenv("SAFEKAW_DOWNLOAD_ATTEMPTS", unset = "4")
  ))
  if (is.na(max_download_attempts) || max_download_attempts < 1L) {
    max_download_attempts <- 4L
  }

  dat <- NULL
  last_download_error <- NULL
  for (attempt in seq_len(max_download_attempts)) {
    message("GridMET download attempt ", attempt, " of ", max_download_attempts)

    dat <- tryCatch({
      # GridMET's obsolete port-8080 source fails even during metadata
      # inspection on KU. Rewrite the locally bundled catalog before
      # climateR's dap_crop() opens any remote NetCDF resource.
      climater_filter <- get("climater_filter", envir = asNamespace("climateR"))
      gridmet_catalog <- climater_filter(
        id = "gridmet",
        AOI = bbox,
        varname = c("pr", "tmmn", "tmmx")
      )

      old_nkn_host <- "http://thredds.northwestknowledge.net:8080"
      new_nkn_host <- "https://tds-proxy.nkn.uidaho.edu"
      gridmet_catalog$URL <- sub(
        paste0("^", old_nkn_host),
        new_nkn_host,
        gridmet_catalog$URL
      )

      if (any(startsWith(gridmet_catalog$URL, old_nkn_host)) ||
          !all(startsWith(gridmet_catalog$URL, new_nkn_host))) {
        stop("GridMET URL rewrite away from the obsolete port-8080 host failed.")
      }

      message("GridMET request host(s): ", paste(unique(sub(
        "^(https?://[^/]+).*$", "\\1", gridmet_catalog$URL
      )), collapse = ", "))

      climateR_dap <- get("dap", envir = asNamespace("climateR"))
      climateR_dap(
        catalog = gridmet_catalog,
        AOI = bbox,
        startDate = start_date,
        endDate = end_date,
        verbose = FALSE
      )
    }, error = function(e) {
      last_download_error <<- conditionMessage(e)
      message("GridMET attempt ", attempt, " failed: ", last_download_error)
      NULL
    })

    if (!is.null(dat)) break
    if (attempt < max_download_attempts) {
      wait_seconds <- 20L * attempt
      message("Waiting ", wait_seconds, " seconds before retry...")
      Sys.sleep(wait_seconds)
    }
  }

  if (is.null(dat)) {
    stop(
      "GridMET download failed after ", max_download_attempts,
      " attempts. Last error: ", last_download_error
    )
  }
  
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
climate_daily_all <- tmin_means %>%
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
  dplyr::select(
    FIPS, name, Date, Year, Month, Day,
    precip_mm_gridmet,
    tmmn_C_gridmet,
    tmmx_C_gridmet
  ) %>%
  arrange(FIPS, Date)

climate_daily <- climate_daily_all %>%
  filter(Year %in% yrs_common)

# ---- temperature QC checks ----
temp_crossings <- climate_daily %>%
  filter(
    is.finite(tmmn_C_gridmet),
    is.finite(tmmx_C_gridmet),
    tmmn_C_gridmet > tmmx_C_gridmet
  ) %>%
  mutate(excess_C = tmmn_C_gridmet - tmmx_C_gridmet)

if (nrow(temp_crossings) > 0) {
  message("GridMET Tmin/Tmax crossings before guarded correction:")
  print(temp_crossings)

  qc_dir <- file.path(climate_scratch, "qc")
  dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
  crossings_file <- file.path(
    qc_dir,
    paste0("gridmet_temperature_crossings_", run_tag, ".csv")
  )
  write_csv(temp_crossings, crossings_file)

  max_crossing_excess_C <- max(temp_crossings$excess_C, na.rm = TRUE)
  if (nrow(temp_crossings) > 10L || max_crossing_excess_C > 0.5) {
    stop(
      "GridMET temperature QC failed: ", nrow(temp_crossings),
      " crossing(s), maximum excess = ", round(max_crossing_excess_C, 3),
      " C. Diagnostic: ", crossings_file
    )
  }

  # For rare sub-0.5 C aggregation crossings, project Tmin/Tmax to their
  # midpoint. This enforces physical ordering while preserving daily mean
  # temperature—and therefore mean-temperature GDD—exactly.
  warning(
    "Correcting ", nrow(temp_crossings),
    " isolated GridMET aggregation crossing(s) to their midpoint; audit: ",
    crossings_file
  )
  climate_daily_all <- climate_daily_all %>%
    mutate(
      .crossing = is.finite(tmmn_C_gridmet) &
        is.finite(tmmx_C_gridmet) &
        tmmn_C_gridmet > tmmx_C_gridmet,
      .midpoint = (tmmn_C_gridmet + tmmx_C_gridmet) / 2,
      tmmn_C_gridmet = if_else(.crossing, .midpoint, tmmn_C_gridmet),
      tmmx_C_gridmet = if_else(.crossing, .midpoint, tmmx_C_gridmet)
    ) %>%
    dplyr::select(-.crossing, -.midpoint)
  climate_daily <- climate_daily_all %>%
    filter(Year %in% yrs_common)
}

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
  stop("GridMET temperature QC failed: some daily tmin values exceed tmax.")
}
if (temp_qc$tmin_min < -50 || temp_qc$tmax_max > 60) {
  warning("GridMET temperature QC warning: values are outside plausible Celsius range.")
}

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
climate_crop_daily <- climate_daily_all %>%
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
crop_climate_corn <- summarize_crop_climate_gs(
  df       = climate_crop_daily,
  crop     = "Corn",
  target_years = yrs_common
)

crop_climate_soy <- summarize_crop_climate_gs(
  df       = climate_crop_daily,
  crop     = "Soybeans",
  target_years = yrs_common
)

crop_climate_sorg <- summarize_crop_climate_gs(
  df       = climate_crop_daily,
  crop     = "Sorghum",
  target_years = yrs_common
)

crop_climate_wheat <- summarize_crop_climate_gs(
  df       = climate_crop_daily,
  crop     = "Wheat",
  target_years = yrs_common
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

validate_crop_climate_completeness(crop_climate_all, yrs_common)

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
