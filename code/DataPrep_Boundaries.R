## DataPrep_Boundaries.R
# Author: Sam Zipper
# Date: 5/22/2025
#
# Purpose: This script pulls in shapefiles that are needed for subsequent DataPrep tasks
# and cleans them up for use in subsequent steps, and puts into a common projection.
# 
# Shapefiles created are:
#  - Boundary_EKSRBwatershed.gpkg = Boundary for EKSRB watershed extent.
#  - Boundary_EKSRBcounties.gpkg = County polygons clipped to the EKSRB watershed,
#        including both clipped and full-county areas.
#  - Boundary_EKSRBalluvialCorridor.gpkg = Boundary for alluvial corridor where most irrigated agriculture
#        is located in watershed.
# Output is saved in `data` folder.


# Set up workspace --------------------------------------------------------

source(file.path("code", "paths+packages.R"))

# load raw data from different sources ------------------------------------

sf_watershed_in <- st_read(file.path(path_data, "Boundaries", "WatershedBoundary_KN_20230113", "watershed_bndry.shp"))
sf_counties_in <- st_read(file.path(path_data, "Boundaries", "CountiesSelection_KN_20230113", "counties selection.shp"))
sf_corridor_in <- st_read(file.path(path_data, "Boundaries", "kaw_valley_dtf", "kaw_valley_dtf.shp"))
# NOTE ABOUT CORRIDOR: 
#  There is a 'kaw_valley_dtf.shp' that Shreya added to Drive based on depth-to-flood data, created by Geoff. 
#  This is very similar to the 'KRiver_AA' shapefile that Gaurav created, but does not extend as far west.
#  We will use the kaw_valley_dtf.shp for consistency wih Shreya and other KGS work.

# Process shapefiles -------------------------------------------------------

## watershed- desired attributes columns: area_ha
sf_watershed_out <- 
  sf_watershed_in |> 
  st_transform(domain_crs) |> 
  dplyr::select(geometry)
sf_watershed_out$area_ha <- as.numeric(st_area(sf_watershed_out)/10000) # convert m^2 to ha

## counties - retain full-county area, then clip geometry to the watershed
sf_counties_full <-
  sf_counties_in |> 
  # get rid of Jackson County MO and Dickinson County KS
  filter(!(FIPS %in% c(29095, 20041))) |> 
  st_transform(domain_crs) |> 
  dplyr::select(NAME, FIPS) |> 
  rename(name = NAME)
sf_counties_full$fullCountyArea_ha <- as.numeric(st_area(sf_counties_full) / 10000)

# The saved geometry is the county portion inside the EKSRB. Keeping the
# historical watershedOverlap_* fields preserves compatibility downstream,
# while area_ha now correctly describes the saved (clipped) geometry.
sf_counties_out <-
  st_intersection(sf_counties_full, sf_watershed_out |> dplyr::select())
sf_counties_out$area_ha <- as.numeric(st_area(sf_counties_out) / 10000)
sf_counties_out <- sf_counties_out |>
  mutate(
    watershedOverlap_ha = area_ha,
    watershedOverlap_prc = watershedOverlap_ha / fullCountyArea_ha
  ) |>
  dplyr::select(name, FIPS, area_ha, fullCountyArea_ha,
                watershedOverlap_ha, watershedOverlap_prc)

stopifnot(nrow(sf_counties_out) == 17)
stopifnot(abs(sum(sf_counties_out$area_ha) - sf_watershed_out$area_ha) < 1)

## corridor - desired attribute columns: area_ha
sf_corridor_out <- 
  sf_corridor_in[1,] |>  # there are a handful of tiny polygons - get rid of those
  st_transform(domain_crs) |> 
  dplyr::select(geometry)
sf_corridor_out$area_ha <- as.numeric(st_area(sf_corridor_out)/10000) # convert m^2 to ha


# visualize and save ------------------------------------------------------

ggplot() + 
  geom_sf(data = sf_counties_out, aes(fill = watershedOverlap_prc)) +
  geom_sf(data = sf_watershed_out, fill = NA, color = col.cat.red) +
  geom_sf(data = sf_corridor_out, fill = NA, color = col.cat.blu) +
  scale_fill_viridis_c()

st_write(sf_corridor_out, file.path("data", "Boundary_EKSRBalluvialCorridor.gpkg"))
st_write(sf_counties_out, file.path("data", "Boundary_EKSRBcounties.gpkg"))
st_write(sf_watershed_out, file.path("data", "Boundary_EKSRBwatershed.gpkg"))
