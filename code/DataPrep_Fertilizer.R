## DataPrep_Fertilizer.R
# Author: Sam Zipper
# Date: 6/26/2025
#
# Purpose: This script creates a look-up table of fertilizer use by Crop and WaterManagement.
#  The data are compiled in the SAFEKAW google drive by Ikenna and Sam, so this 
#  script just gets the data and cleans it up for use in optimization.
# 
# Output is a CSV file: FertilizerData_EKSRB.csv
# | Crop | WaterManagement | FertilizerUse_kgHa |
# *WaterManagement = “Irrigated” or “Rainfed”
#
# Output is saved in `data` folder.

# Set up workspace --------------------------------------------------------

source(file.path("code", "paths+packages.R"))

# load data
df_in <- read_csv(file.path(path_data, "FertilizerData_EKSRB.csv"))