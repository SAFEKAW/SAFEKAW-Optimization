## DataPrep_Boundaries.R
# Author: Sam Zipper
# Date: 5/22/2025
#
# Purpose: This script pulls in shapefiles that are needed for subsequent DataPrep tasks
# and cleans them up for use in subsequent steps, and puts into a common projection.
# 
# Shapefiles created are:
#  - Boundary_EKSRBwatershed.gpkg = Boundary for EKSRB watershed extent.
#  - Boundary_EKSRBcounties.gpkg = Boundaries for each county within EKSRB focus domain, including
#        percent of county that is within EKSRB watershed.
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

## counties - desired attribute columns: name, FIPS, area_ha, watershedOverlap_prc
sf_counties_out <-
  sf_counties_in |> 
  # get rid of Jackson County MO and Dickinson County KS
  filter(!(FIPS %in% c(29095, 20041))) |> 
  st_transform(domain_crs) |> 
  dplyr::select(NAME, FIPS) |> 
  rename(name = NAME)
sf_counties_out$area_ha <- as.numeric(st_area(sf_counties_out)/10000) # convert m^2 to ha

# Calculate the intersection of the counties with the watershed
sf_intersection <- 
  st_intersection(sf_counties_out, sf_watershed_out) %>%
  mutate(watershedOverlap_ha = as.numeric(st_area(.)/10000)) |> 
  st_drop_geometry() |> 
  dplyr::select(name, watershedOverlap_ha)

# Join the intersection areas back to the original counties
sf_counties_out <- 
  sf_counties_out |> 
  left_join(sf_intersection, by = "name") |> 
  mutate(watershedOverlap_prc = watershedOverlap_ha/area_ha)

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
