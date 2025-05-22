## DataPrep_ClimateHistoric.R
# Author: Archi Howlader
# Date: 5/22/2025
#
# Purpose: This script gets monthly historic climate data, averaged over the following
#   subdomains: Counties, EKSRB, AlluvialCorridor.
# Subdomain boundaries are created in `DataPrep_Boundaries.R` script and saved in `data` folder.
# 
# CSV files created are:
#  County CSV file: ClimateData_County.csv
#  | Year | Month | FIPS | precip_mm | {otherClimateVars}
#  
#  EKSRB CSV file: ClimateData_EKSRB.csv
#  | Year | Month | precip_mm | {otherClimateVars}
#  
#  Alluvial Corridor CSV file: ClimateData_AlluvialCorridor.csv
#  | Year | Month | precip_mm | {otherClimateVars}
#
# Output is saved in `data` folder.

# Set up workspace --------------------------------------------------------

source(file.path("code", "paths+packages.R"))

# load boundaries ------------------------------------

sf_corridor <- st_read(file.path("data", "Boundary_EKSRBalluvialCorridor.gpkg"))
sf_counties <- st_read(file.path("data", "Boundary_EKSRBcounties.gpkg"))
sf_watershed <- st_read(file.path("data", "Boundary_EKSRBwatershed.gpkg"))

