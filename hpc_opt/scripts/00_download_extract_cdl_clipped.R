suppressPackageStartupMessages({
  library(dplyr)
  library(exactextractr)
  library(here)
  library(httr)
  library(jsonlite)
  library(purrr)
  library(readr)
  library(sf)
  library(terra)
  library(tidyr)
})

# Download annual 30 m CDL imagery from the current USDA CroplandCROS
# ImageServer, cache one watershed raster per year, and regenerate county land
# cover using county polygons clipped to the EKSRB.

years <- 2006:2024
service_url <- paste0(
  "https://pdi.scinet.usda.gov/image/rest/services/",
  "CDL_WM/ImageServer/exportImage"
)
cache_dir <- here("data", "raw", "cdl_eksrb")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

county_file <- here("data", "Boundary_EKSRBcounties.gpkg")
watershed_file <- here("data", "Boundary_EKSRBwatershed.gpkg")
county_output <- here("data", "LandCoverData-CDL_County.csv")
county_backup <- here(
  "data", "LandCoverData-CDL_County_full_counties_preclip.csv"
)
watershed_reference_file <- here("data", "LandCoverData-CDL_EKSRB.csv")
audit_output <- here("hpc_opt", "outputs", "landcover_clipping_audit.csv")

counties <- st_read(county_file, quiet = TRUE) |>
  st_make_valid()
watershed <- st_read(watershed_file, quiet = TRUE) |>
  st_make_valid()

if (nrow(counties) != 17L) stop("Expected exactly 17 clipped counties.")
if (anyDuplicated(counties$FIPS)) stop("County FIPS values are not unique.")

counties$FIPS <- as.character(counties$FIPS)
counties$polygon_area_ha <- as.numeric(st_area(counties)) / 10000
watershed_area_ha <- sum(as.numeric(st_area(watershed))) / 10000

if (abs(sum(counties$polygon_area_ha) - watershed_area_ha) >= 1) {
  stop("Clipped county areas do not sum to the watershed area.")
}

# Use a stable 30 m Web Mercator grid. Tile exports because the ImageServer
# limits image dimensions to 4097 pixels.
watershed_3857 <- st_transform(watershed, 3857)
bbox_raw <- st_bbox(watershed_3857)
cellsize <- 30
xmin <- floor(bbox_raw[["xmin"]] / cellsize) * cellsize
ymin <- floor(bbox_raw[["ymin"]] / cellsize) * cellsize
xmax <- ceiling(bbox_raw[["xmax"]] / cellsize) * cellsize
ymax <- ceiling(bbox_raw[["ymax"]] / cellsize) * cellsize
max_tile_cells <- 4000L

make_tile_grid <- function() {
  x_breaks <- seq(xmin, xmax, by = max_tile_cells * cellsize)
  y_breaks <- seq(ymin, ymax, by = max_tile_cells * cellsize)
  if (tail(x_breaks, 1) < xmax) x_breaks <- c(x_breaks, xmax)
  if (tail(y_breaks, 1) < ymax) y_breaks <- c(y_breaks, ymax)

  tidyr::crossing(
    ix = seq_len(length(x_breaks) - 1L),
    iy = seq_len(length(y_breaks) - 1L)
  ) |>
    mutate(
      tile_xmin = x_breaks[ix],
      tile_xmax = x_breaks[ix + 1L],
      tile_ymin = y_breaks[iy],
      tile_ymax = y_breaks[iy + 1L],
      nx = as.integer(round((tile_xmax - tile_xmin) / cellsize)),
      ny = as.integer(round((tile_ymax - tile_ymin) / cellsize)),
      tile_id = sprintf("x%02d_y%02d", ix, iy)
    )
}

tile_grid <- make_tile_grid()
stopifnot(all(tile_grid$nx <= 4097L), all(tile_grid$ny <= 4097L))

download_tile <- function(year, tile_row, tile_dir) {
  tile_file <- file.path(
    tile_dir, paste0("cdl_", year, "_", tile_row$tile_id, ".tif")
  )
  if (file.exists(tile_file) && file.info(tile_file)$size > 0) {
    return(tile_file)
  }

  mosaic_rule <- jsonlite::toJSON(
    list(where = paste0("Year = ", year)), auto_unbox = TRUE
  )
  query <- list(
    bbox = paste(
      tile_row$tile_xmin, tile_row$tile_ymin,
      tile_row$tile_xmax, tile_row$tile_ymax,
      sep = ","
    ),
    bboxSR = "3857",
    size = paste(tile_row$nx, tile_row$ny, sep = ","),
    imageSR = "3857",
    format = "tiff",
    pixelType = "U8",
    noData = "0",
    interpolation = "RSP_NearestNeighbor",
    mosaicRule = mosaic_rule,
    f = "image"
  )

  message("Downloading CDL ", year, " tile ", tile_row$tile_id)
  response <- httr::RETRY(
    "GET", service_url,
    query = query,
    httr::write_disk(tile_file, overwrite = TRUE),
    httr::timeout(180),
    times = 4,
    pause_min = 2,
    pause_cap = 20,
    terminate_on = c(400, 401, 403, 404)
  )
  httr::stop_for_status(response)

  tile <- terra::rast(tile_file)
  if (terra::ncell(tile) == 0 || all(is.na(terra::minmax(tile)))) {
    stop("Downloaded CDL tile is empty: ", tile_file)
  }
  tile_file
}

download_cdl_year <- function(year) {
  outfile <- file.path(cache_dir, paste0("cdl_eksrb_", year, ".tif"))
  if (file.exists(outfile) && file.info(outfile)$size > 0) {
    message("Using cached CDL ", year)
    return(outfile)
  }

  tile_dir <- file.path(cache_dir, paste0("tiles_", year))
  dir.create(tile_dir, recursive = TRUE, showWarnings = FALSE)
  tile_files <- purrr::map_chr(
    seq_len(nrow(tile_grid)),
    function(i) download_tile(year, tile_grid[i, ], tile_dir)
  )

  tile_rasters <- lapply(tile_files, terra::rast)
  mosaic_raster <- do.call(terra::merge, tile_rasters)
  terra::writeRaster(
    mosaic_raster, outfile, overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW"), datatype = "INT1U")
  )
  rm(tile_rasters, mosaic_raster)
  gc(verbose = FALSE)
  terra::tmpFiles(remove = TRUE)
  unlink(tile_files)
  unlink(tile_dir, recursive = TRUE)
  outfile
}

classify_cdl <- function(code) {
  dplyr::case_when(
    code %in% c(1, 12, 13, 251) ~ "Corn",
    code %in% 4 ~ "Sorghum",
    code %in% 5 ~ "Soybeans",
    code %in% c(23, 24) ~ "Wheat",
    code %in% c(2, 3, 6, 21, 25, 27:58, 67:77,
                205:224, 229, 242, 247, 251) ~ "Other Crops",
    code %in% c(59, 60, 152, 176) ~ "Grass/Pasture/Shrubland",
    code %in% c(26, 225:228, 235:241, 254) ~ "Double Crop",
    code %in% c(61, 131) ~ "Fallow/Barren",
    code %in% c(87, 190, 195) ~ "Wetlands",
    code %in% c(63, 141:143) ~ "Forest",
    code %in% 121:124 ~ "Developed",
    code %in% c(83, 111) ~ "Water",
    TRUE ~ NA_character_
  )
}

landcover_levels <- c(
  "Corn", "Developed", "Double Crop", "Fallow/Barren", "Forest",
  "Grass/Pasture/Shrubland", "Other Crops", "Sorghum", "Soybeans",
  "Water", "Wetlands", "Wheat"
)

extract_county_year <- function(year, raster_file) {
  message("Extracting clipped counties | ", year)
  cdl <- terra::rast(raster_file)
  counties_raster_crs <- st_transform(counties, terra::crs(cdl))
  extracted <- exactextractr::exact_extract(cdl, counties_raster_crs)

  purrr::map2_dfr(
    seq_len(nrow(counties)), extracted,
    function(i, values) {
      if (is.null(values) || nrow(values) == 0) {
        stop("No CDL pixels returned for FIPS ", counties$FIPS[[i]],
             " in ", year, ".")
      }
      value_col <- setdiff(
        names(values), c("coverage_fraction", "coverage_area")
      )[[1]]
      summarized <- values |>
        filter(!is.na(.data[[value_col]]), .data[[value_col]] != 0) |>
        transmute(
          CDLcode = as.integer(.data[[value_col]]),
          coverage_fraction
        ) |>
        group_by(CDLcode) |>
        summarise(weight = sum(coverage_fraction), .groups = "drop") |>
        mutate(LandCover = classify_cdl(CDLcode))

      unmapped <- summarized |>
        filter(is.na(LandCover), weight > 0)
      if (nrow(unmapped) > 0) {
        stop(
          "Unmapped CDL codes in ", year, " FIPS ", counties$FIPS[[i]],
          ": ", paste(unmapped$CDLcode, collapse = ", ")
        )
      }

      summarized |>
        group_by(LandCover) |>
        summarise(weight = sum(weight), .groups = "drop") |>
        complete(LandCover = landcover_levels, fill = list(weight = 0)) |>
        mutate(
          Year = year,
          FIPS = counties$FIPS[[i]],
          area_prc = 100 * weight / sum(weight),
          area_ha = counties$polygon_area_ha[[i]] * area_prc / 100
        ) |>
        select(Year, FIPS, LandCover, area_ha, area_prc)
    }
  )
}

raster_files <- purrr::map_chr(years, download_cdl_year)
county_cdl <- purrr::map2_dfr(years, raster_files, extract_county_year)

# Structural and spatial validation.
county_checks <- county_cdl |>
  group_by(Year, FIPS) |>
  summarise(
    area_ha = sum(area_ha),
    area_prc = sum(area_prc),
    .groups = "drop"
  ) |>
  left_join(
    st_drop_geometry(counties) |>
      select(FIPS, polygon_area_ha),
    by = "FIPS"
  )

stopifnot(n_distinct(county_cdl$Year) == length(years))
stopifnot(n_distinct(county_cdl$FIPS) == 17L)
stopifnot(nrow(county_checks) == length(years) * 17L)
stopifnot(all(abs(county_checks$area_prc - 100) < 1e-6))
stopifnot(all(abs(county_checks$area_ha - county_checks$polygon_area_ha) < 1e-4))

year_checks <- county_cdl |>
  group_by(Year) |>
  summarise(
    county_sum_ha = sum(area_ha),
    watershed_geometry_ha = watershed_area_ha,
    geometry_difference_ha = county_sum_ha - watershed_geometry_ha,
    .groups = "drop"
  )
stopifnot(all(abs(year_checks$geometry_difference_ha) < 1))

if (file.exists(watershed_reference_file)) {
  watershed_reference <- readr::read_csv(
    watershed_reference_file, show_col_types = FALSE
  ) |>
    group_by(Year) |>
    summarise(reference_cdl_ha = sum(area_ha), .groups = "drop")
  year_checks <- year_checks |>
    left_join(watershed_reference, by = "Year") |>
    mutate(
      reference_difference_ha = county_sum_ha - reference_cdl_ha,
      reference_difference_pct =
        100 * reference_difference_ha / reference_cdl_ha
    )
}

dir.create(dirname(audit_output), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(year_checks, audit_output)

if (file.exists(county_output) && !file.exists(county_backup)) {
  file.copy(county_output, county_backup, overwrite = FALSE)
}
readr::write_csv(county_cdl, county_output)

message(
  "Wrote watershed-clipped county CDL table: ", county_output,
  "\nAnnual area = ", round(watershed_area_ha, 2), " ha."
)
