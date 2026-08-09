suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(here)
})

county_file <- here("data", "Boundary_EKSRBcounties.gpkg")
full_county_file <- here("data", "Boundary_EKSRBcounties_full.gpkg")
watershed_file <- here("data", "Boundary_EKSRBwatershed.gpkg")

counties <- st_read(county_file, quiet = TRUE)
watershed <- st_read(watershed_file, quiet = TRUE) |>
  st_transform(st_crs(counties))

if (nrow(counties) != 17L) stop("Expected exactly 17 EKSRB counties.")

watershed_area_m2 <- sum(as.numeric(st_area(watershed)))
county_geometry_area_m2 <- sum(as.numeric(st_area(counties)))

if (abs(county_geometry_area_m2 - watershed_area_m2) < 1e4) {
  message("County boundary file is already clipped to the EKSRB.")
  quit(save = "no", status = 0)
}

if (!file.exists(full_county_file)) {
  st_write(counties, full_county_file, quiet = TRUE)
}

counties$fullCountyArea_ha <- as.numeric(st_area(counties) / 10000)
counties_full <- counties |>
  select(name, FIPS, fullCountyArea_ha)

counties_clipped <- st_intersection(
  counties_full,
  watershed |> select()
)
counties_clipped$area_ha <- as.numeric(st_area(counties_clipped) / 10000)
counties_clipped <- counties_clipped |>
  mutate(
    watershedOverlap_ha = area_ha,
    watershedOverlap_prc = watershedOverlap_ha / fullCountyArea_ha
  ) |>
  select(name, FIPS, area_ha, fullCountyArea_ha,
         watershedOverlap_ha, watershedOverlap_prc)

if (nrow(counties_clipped) != 17L) stop("Clipping did not return 17 counties.")
if (abs(sum(as.numeric(st_area(counties_clipped))) - watershed_area_m2) >= 1e4) {
  stop("Clipped county geometries do not sum to the watershed area.")
}

st_write(counties_clipped, county_file, delete_dsn = TRUE, quiet = TRUE)

message(
  "Saved 17 watershed-clipped county polygons; total area = ",
  round(sum(counties_clipped$area_ha), 2), " ha."
)
