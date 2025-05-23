## DataPrep_LandCover.R
# Author: Sam Zipper
# Date: 5/23/2025
#
# Purpose: This script processes annual land cover data from the USDA CDL to create
#   the following output CSV files:
#
#    EKSRB CSV file: LandCoverData-CDL_EKSRB.csv
#    | Year | LandCover | area_ha |
#      
#    Alluvial Corridor CSV file: LandCoverData-CDL_AlluvialCorridor.csv
#    | Year | LandCover | area_ha |
#
# Subdomain boundaries are created in `DataPrep_Boundaries.R` script and saved in `data` folder.
# 
# Output is saved in `data` folder.


# Set up workspace --------------------------------------------------------

source(file.path("code", "paths+packages.R"))

# load boundaries ------------------------------------

sf_corridor <- st_read(file.path("data", "Boundary_EKSRBalluvialCorridor.gpkg"))
sf_counties <- st_read(file.path("data", "Boundary_EKSRBcounties.gpkg"))
sf_watershed <- st_read(file.path("data", "Boundary_EKSRBwatershed.gpkg"))


# extract CDL data for county bbox ----------------------------------------

# years with CDL data for the state
years_all <- seq(2006, 2024)

# # get CDL data for state and save to google drive - only have to run this once, 
# # then can be loaded from Google Drive subsequent times
# cdl_data <-
#   getCDL("Kansas", years_all)
# for (y in years_all){
#   cdl_year <- cdl_data[[paste0("KS", y)]]
#   writeRaster(cdl_year,
#               filename = file.path(path_data, "Agriculture-Land", "CroplandDataLayer", paste0("CDL_KS_", y, ".tif")),
#               format = "GTiff",
#               overwrite = TRUE)
# }

# load CDL data and make calculations
start_loop <- T
for (y in years_all){
  cdl_year <- 
    rast(file.path(path_data, "Agriculture-Land", "CroplandDataLayer", paste0("CDL_KS_", y, ".tif")))
  
  # loop through domains
  for (d in c("watershed", "corridor")){
    # define boundary of domain
    if (d == "watershed") sf_boundary <- sf_watershed
    if (d == "corridor") sf_boundary <- sf_corridor
    
    # convert sf_boundary to same CRS as CDL data
    sf_boundary <- 
      st_transform(sf_boundary, crs = st_crs(cdl_year))
    
    # crop/mask cdl_year SpatRaster to extent of sf_boundary
    cdl_year_d <- 
      cdl_year |> 
      terra::crop(sf_boundary) |> 
      terra::mask(sf_boundary)
    
    # get count of each land cover type in cdl_year_d as table
    cdl_year_d_table <- 
      cdl_year_d |> 
      terra::as.data.frame(na.rm = TRUE) |> 
      set_names("CDLcode") |> 
      dplyr::group_by(CDLcode) |> 
      dplyr::summarise(n_pixels = n()) |> 
      dplyr::ungroup() |> 
      mutate(Year = y,
             domain = d,
             pixel_resolution = res(cdl_year_d)[1],
             coverArea_ha = n_pixels * pixel_resolution^2 / 10000) |> 
      subset(CDLcode != 0) # a couple random "background" pixels - remove
    cdl_year_d_table$CDLname <- updateNamesCDL(cdl_year_d_table$CDLcode)
    
    if (start_loop){
      cdl_all <- cdl_year_d_table
      start_loop <- F
    } else {
      cdl_all <- bind_rows(cdl_all, cdl_year_d_table)
    }
  }
  
  print(paste0(y, " complete"))
}

# create look-up table to make coarser land cover type groups

CDLLandCoverGroups <- unique(cdl_all[,c("CDLcode", "CDLname")])
CDLLandCoverGroups <-
  CDLLandCoverGroups |> 
  mutate(LandCover = case_when(
    CDLcode %in% c(1, 12, 13, 251) ~ "Corn",
    CDLcode %in% c(4) ~ "Sorghum",
    CDLcode %in% c(5) ~ "Soybeans",
    CDLcode %in% c(23, 24) ~ "Wheat",
    CDLcode %in% c(2, 3, 6, 21, 25, 27:58, 67:77, 205:224, 229, 242, 247, 251) ~ "Other Crops",
    CDLcode %in% c(59, 60, 152, 176) ~ "Grass/Pasture/Shrubland",
    CDLcode %in% c(26, 225:228, 235:241, 254) ~ "Double Crop",
    CDLcode %in% c(61, 131) ~ "Fallow/Barren",
    CDLcode %in% c(87, 190, 195) ~ "Wetlands",
    CDLcode %in% c(63, 141:143) ~ "Forest",
    CDLcode %in% c(121:124) ~ "Developed",
    CDLcode %in% c(83, 111) ~ "Water"
  ))

cdl_groups <- left_join(cdl_all, CDLLandCoverGroups, by = "CDLcode", "CDLname")

# plot area in each CDLLandCoverGRoups for each year faceted by domain
cdl_byGroup <- 
  cdl_all |> 
  left_join(CDLLandCoverGroups, by = "CDLcode") |> 
  group_by(Year, domain, LandCover) |> 
  summarise(area_ha = sum(coverArea_ha)) |> 
  ungroup()

# visualize/inspect
ggplot(cdl_byGroup, aes(x = Year, y = area_ha, fill = LandCover)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ domain, scales = "free_y") +
  #scale_fill_manual(values = c(col.cat.grn, col.cat.yel, col.cat.org, col.cat.red, col.cat.blu)) +
  labs(x = "Year", y = "Area (ha)", fill = "Land Cover") +
  theme(legend.position = "bottom")

# average area in each category
cdl_byGroup_avg <- 
  cdl_byGroup |> 
  group_by(domain, LandCover) |> 
  summarise(area_ha = mean(area_ha)) |>
  ungroup() |> 
  mutate(domainArea_ha = 
           case_when(
             domain == "corridor" ~ sf_corridor$area_ha,
             domain == "watershed" ~ sf_watershed$area_ha
           ),
         area_pct = area_ha / domainArea_ha * 100)

# inspect
ggplot(cdl_byGroup_avg, aes(x = LandCover, y = area_pct)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ domain, scales = "free_y") +
  labs(x = "Land Cover", y = "Area (%)", fill = "Land Cover") +
  theme(legend.position = "bottom") +
  coord_flip()

# separate into final output format and save ------------------------------

cdl_byGroup |> 
  subset(domain == "corridor") |>
  dplyr::select(-domain) |> 
  write_csv(file.path("data", "LandCoverData-CDL_AlluvialCorridor.csv"))


cdl_byGroup |> 
  subset(domain == "watershed") |>
  dplyr::select(-domain) |> 
  write_csv(file.path("data", "LandCoverData-CDL_EKSRB.csv"))
