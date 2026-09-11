# Figure 1: Eastern Kansas River Basin study area and land cover
#
# Produces a two-panel locator + land-cover map using a single USDA Cropland
# Data Layer (CDL) year. Downloads are cached under data/raw/figure1.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(sf)
  library(terra)
})

cdl_year <- 2024L
cache_dir <- file.path("data", "raw", "figure1")
output_dir <- "figures"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

watershed <- st_read(
  file.path("data", "Boundary_EKSRBwatershed.gpkg"), quiet = TRUE
) |> st_make_valid()
aquifer <- st_read(
  file.path("data", "Boundary_EKSRBalluvialCorridor.gpkg"), quiet = TRUE
) |> st_make_valid()
counties <- st_read(
  file.path("data", "Boundary_EKSRBcounties.gpkg"), quiet = TRUE
) |> st_make_valid()

# Download the CDL only over the study-area bounding box. The USDA service caps
# output dimensions, so a 90 m display grid keeps the request comfortably below
# the limit while retaining field-scale spatial texture.
cdl_file <- file.path(cache_dir, paste0("CDL_EKSRB_", cdl_year, "_90m.tif"))
if (!file.exists(cdl_file)) {
  watershed_3857 <- st_transform(watershed, 3857)
  bb <- st_bbox(watershed_3857)
  pad <- 1500
  bb[c("xmin", "ymin")] <- bb[c("xmin", "ymin")] - pad
  bb[c("xmax", "ymax")] <- bb[c("xmax", "ymax")] + pad
  resolution <- 90
  nx <- ceiling((bb[["xmax"]] - bb[["xmin"]]) / resolution)
  ny <- ceiling((bb[["ymax"]] - bb[["ymin"]]) / resolution)
  stopifnot(nx <= 4096, ny <= 4096)

  mosaic_rule <- URLencode(
    paste0('{"where":"Year = ', cdl_year, '"}'), reserved = TRUE
  )
  query <- paste0(
    "bbox=", paste(unname(bb), collapse = "%2C"),
    "&bboxSR=3857",
    "&size=", nx, "%2C", ny,
    "&imageSR=3857&format=tiff&pixelType=U8&noData=0",
    "&interpolation=RSP_NearestNeighbor&mosaicRule=", mosaic_rule,
    "&f=image"
  )
  url <- paste0(
    "https://pdi.scinet.usda.gov/image/rest/services/",
    "CDL_WM/ImageServer/exportImage?", query
  )
  download.file(url, cdl_file, mode = "wb", quiet = FALSE)
}

# Cache a Census cartographic-boundary state file for an accurate Kansas inset.
states_zip <- file.path(cache_dir, "cb_2024_us_state_20m.zip")
states_dir <- file.path(cache_dir, "cb_2024_us_state_20m")
states_shp <- file.path(states_dir, "cb_2024_us_state_20m.shp")
if (!file.exists(states_shp)) {
  if (!file.exists(states_zip)) {
    download.file(
      "https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_state_20m.zip",
      states_zip, mode = "wb", quiet = FALSE
    )
  }
  dir.create(states_dir, recursive = TRUE, showWarnings = FALSE)
  unzip(states_zip, exdir = states_dir)
}
kansas <- st_read(states_shp, quiet = TRUE) |> filter(STUSPS == "KS")

classify_cdl <- function(x) {
  case_when(
    x %in% c(1, 12, 13, 251) ~ 1L,
    x == 5 ~ 2L,
    x %in% c(23, 24) ~ 3L,
    x == 4 ~ 4L,
    x %in% c(2, 3, 6, 21, 25, 27:58, 67:77,
             205:224, 229, 242, 247, 251,
             26, 225:228, 235:241, 254) ~ 5L,
    x %in% c(59, 60, 152, 176) ~ 6L,
    x %in% c(63, 141:143) ~ 7L,
    x %in% c(121:124) ~ 8L,
    x %in% c(83, 87, 111, 190, 195) ~ 9L,
    x %in% c(61, 131) ~ 10L,
    TRUE ~ NA_integer_
  )
}

cover_names <- c(
  "Corn", "Soybeans", "Wheat", "Sorghum", "Other cultivated crops",
  "Grass / pasture", "Forest", "Developed", "Water", "Fallow / barren"
)
cover_colors <- c(
  "#F2CF1D", "#168B12", "#A77939", "#E88825", "#B78A4A",
  "#D9EFA3", "#7FBE7B", "#B7B7B7", "#3F7FBF", "#E8E1C9"
)

cdl <- rast(cdl_file)
watershed_raster_crs <- st_transform(watershed, crs(cdl))
aquifer_raster_crs <- st_transform(aquifer, crs(cdl))
counties_raster_crs <- st_transform(counties, crs(cdl))
cdl <- mask(cdl, vect(watershed_raster_crs))
cdl_lookup <- data.frame(code = 0:255)
cdl_lookup$group <- classify_cdl(cdl_lookup$code)
cover <- classify(cdl, as.matrix(cdl_lookup), others = NA)
levels(cover) <- data.frame(ID = seq_along(cover_names), land_cover = cover_names)

# Convert once for ggplot; at 90 m this remains manageable and renders crisply.
cover_df <- as.data.frame(cover, xy = TRUE, na.rm = TRUE)
names(cover_df)[3] <- "land_cover"
cover_df$land_cover <- factor(cover_df$land_cover, levels = cover_names)

watershed_ll <- st_transform(watershed, 4326)
aquifer_ll <- st_transform(aquifer, 4326)
kansas_ll <- st_transform(kansas, 4326)

# The basin spans roughly 96.9--94.6 degrees W, so 96 W and 95 W are the
# meridians that intersect it. Construct and label them explicitly.
basin_bbox_ll <- st_bbox(watershed_ll)
meridians_ll <- st_sf(
  longitude = c("96 W", "95 W"),
  geometry = st_sfc(lapply(c(-96, -95), function(lon) {
    st_linestring(matrix(
      c(lon, basin_bbox_ll[["ymin"]] - 0.05,
        lon, basin_bbox_ll[["ymax"]] + 0.05),
      ncol = 2, byrow = TRUE
    ))
  }), crs = 4326)
)
meridian_labels_ll <- st_sf(
  label = c("96 W", "95 W"),
  geometry = st_sfc(lapply(c(-96, -95), function(lon) {
    st_point(c(lon, basin_bbox_ll[["ymin"]] + 0.055))
  }), crs = 4326)
)
meridians_map <- st_transform(meridians_ll, crs(cdl))
meridian_labels_map <- st_transform(meridian_labels_ll, crs(cdl))

# Calculate a spatial mean annual precipitation surface from the local daily
# gridMET archive. Use complete years through the CDL year for consistency.
precip_daily <- rast(file.path("data", "climate", "precip.tif"))
precip_dates <- time(precip_daily)
precip_keep <- which(
  as.integer(format(precip_dates, "%Y")) >= 2006 &
    as.integer(format(precip_dates, "%Y")) <= cdl_year
)
precip_daily <- precip_daily[[precip_keep]]
precip_years <- as.integer(format(time(precip_daily), "%Y"))
precip_annual <- tapp(precip_daily, precip_years, sum, na.rm = TRUE)
precip_mean_annual <- mean(precip_annual, na.rm = TRUE)
watershed_precip_crs <- st_transform(watershed, crs(precip_mean_annual))
counties_precip_crs <- st_transform(counties, crs(precip_mean_annual))
precip_mean_annual <- mask(precip_mean_annual, vect(watershed_precip_crs))
precip_df <- as.data.frame(precip_mean_annual, xy = TRUE, na.rm = TRUE)
names(precip_df)[3] <- "precip_mm"

theme_map <- theme_void(base_size = 10, base_family = "Arial") +
  theme(
    text = element_text(color = "#202020"),
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, color = "#555555"),
    plot.margin = margin(4, 4, 4, 4),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    legend.key.height = grid::unit(3.5, "mm"),
    legend.key.width = grid::unit(5, "mm")
  )

p_locator <- ggplot() +
  geom_sf(data = kansas_ll, fill = "#F4F2ED", color = "#555555", linewidth = 0.45) +
  geom_raster(
    data = precip_df, aes(x = x, y = y, fill = precip_mm),
    inherit.aes = FALSE, interpolate = TRUE
  ) +
  geom_sf(data = watershed_ll, fill = NA, color = "#3F3F3F", linewidth = 0.55) +
  scale_fill_gradientn(
    colors = c(
      "#D73027", "#F46D43", "#F768A1", "#C51B8A",
      "#7A0177", "#54278F", "#2C7FB8", "#225EA8"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(-102, -100, -98, -96),
    labels = c("102 W", "100 W", "98 W", "96 W")
  ) +
  scale_y_continuous(
    breaks = c(37, 39), labels = c("37 N", "39 N")
  ) +
  coord_sf(
    xlim = c(-102.1, -94.5), ylim = c(36.9, 40.1), expand = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  theme_map +
  theme(
    axis.text = element_text(size = 7, color = "#333333"),
    axis.ticks = element_line(color = "#555555", linewidth = 0.25),
    axis.ticks.length = grid::unit(1.2, "mm"),
    panel.border = element_blank()
  )

p_landcover <- ggplot(cover_df, aes(x = x, y = y, fill = land_cover)) +
  geom_raster() +
  geom_sf(
    data = counties_raster_crs, inherit.aes = FALSE,
    fill = NA, color = "#555555", linewidth = 0.22, alpha = 0.52
  ) +
  geom_sf(
    data = aquifer_raster_crs, inherit.aes = FALSE,
    fill = NA, color = "white", linewidth = 1.35, linetype = "dashed"
  ) +
  geom_sf(
    data = aquifer_raster_crs, inherit.aes = FALSE,
    fill = NA, color = "#173F5F", linewidth = 0.65, linetype = "dashed"
  ) +
  geom_sf(
    data = watershed_raster_crs, inherit.aes = FALSE,
    fill = NA, color = "#202020", linewidth = 0.7
  ) +
  scale_fill_manual(values = setNames(cover_colors, cover_names), drop = FALSE) +
  coord_sf(crs = crs(cdl), datum = NA, expand = FALSE) +
  labs(fill = "Land cover") +
  guides(fill = "none") +
  theme_map +
  theme(legend.position = "none")

# A compact, fixed-layout key matches the locator-over-legend organization of
# the reference figure and keeps the map itself as large as possible.
key_df <- data.frame(
  label = cover_names,
  color = cover_colors,
  row = 10:1
)
p_key <- ggplot(key_df) +
  geom_segment(
    aes(x = 0.05, xend = 0.30, y = 11.25, yend = 11.25),
    inherit.aes = FALSE, color = "#173F5F", linewidth = 0.75,
    linetype = "dashed"
  ) +
  annotate("text", x = 0.38, y = 11.25, label = "Alluvial aquifer",
           hjust = 0, size = 2.85) +
  geom_tile(
    aes(x = 0.17, y = row, fill = label), width = 0.24, height = 0.58
  ) +
  geom_text(
    aes(x = 0.36, y = row, label = label), hjust = 0, size = 2.7
  ) +
  scale_fill_manual(values = setNames(key_df$color, key_df$label), guide = "none") +
  coord_cartesian(xlim = c(0, 1.55), ylim = c(0.45, 11.7), clip = "off") +
  theme_void(base_family = "Arial") +
  theme(plot.margin = margin(1, 3, 1, 3))

p_range <- range(precip_df$precip_mm, na.rm = TRUE)
p_breaks <- c(850, 950, 1050)
p_breaks <- p_breaks[p_breaks >= p_range[1] & p_breaks <= p_range[2]]
precip_bar_df <- data.frame(
  precip_mm = seq(p_range[1], p_range[2], length.out = 500),
  y = 1
)
p_precip <- ggplot(precip_bar_df, aes(x = precip_mm, y = y, fill = precip_mm)) +
  geom_tile(height = 0.18) +
  scale_fill_gradientn(
    colors = c(
      "#D73027", "#F46D43", "#F768A1", "#C51B8A",
      "#7A0177", "#54278F", "#2C7FB8", "#225EA8"
    ),
    guide = "none"
  ) +
  scale_x_continuous(breaks = p_breaks, expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.72, 1.28), expand = FALSE) +
  labs(
    x = paste0("Mean annual precipitation (mm)\ngridMET 2006-", cdl_year),
    y = NULL
  ) +
  theme_void(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 6.8, color = "#333333"),
    axis.title.x = element_text(size = 7, color = "#333333", margin = margin(t = 2)),
    axis.ticks.x = element_line(color = "white", linewidth = 0.5),
    axis.ticks.length.x = grid::unit(-3, "mm"),
    plot.margin = margin(0, 4, 2, 4)
  )

layout_design <- "
AB
DB
CB
"
figure1 <- p_locator + p_landcover + p_key + p_precip +
  plot_layout(
    design = layout_design,
    widths = c(0.46, 1.54),
    heights = c(0.32, 0.17, 1)
  )

ggsave(
  file.path(output_dir, "Figure1_StudyArea_LandCover.png"), figure1,
  width = 8.5, height = 4.95, units = "in", dpi = 400, bg = "white"
)
ggsave(
  file.path(output_dir, "Figure1_StudyArea_LandCover.pdf"), figure1,
  width = 8.5, height = 4.95, units = "in", device = cairo_pdf, bg = "white"
)

message("Wrote Figure 1 PNG and PDF to ", normalizePath(output_dir))
