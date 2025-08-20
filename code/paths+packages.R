## paths+packages.R
# This script loads common packages, sets up plotting controls, etc. for use in other scripts.

# packages
library(tidyverse)
library(sf)
library(terra)
library(patchwork)
library(lubridate)
library(readxl)
library(cdlTools)
library(hydroGOF)
library(broom)  # for model fit tidying


# path to SAFEKAW google drive data folder
path_data <- file.path("G:/My Drive/Projects-Active/SAFEKAW/Data")

# common CRS for everything: EPSG:26915 - https://epsg.io/26915
#  domain spans border of UTM14 and UTM15 so use 15
domain_crs <- st_crs(26915)

# common unit conversions
# source (kg/bushel): 
#         Weights, Measures, and Conversion Factors for Agricultural Commodities and Their Products
#         https://www.ers.usda.gov/publications/pub-details?pubid=41881
# source (cal/tonne): Cassidy et al. (2013), SI table 2 https://iopscience.iop.org/article/10.1088/1748-9326/8/3/034015
ha_per_ac <- 0.40468564219999997311
yield_conversion <-
  data.frame(Crop = c("Wheat", "Corn", "Grain Sorghum", "Soybeans"),
             kg_per_bushel = c(27.2155, 25.4012, 25.4012, 27.2155),
             kcal_per_kg = c(3.284, 3.581, 3.430, 3.596)) # 1000 kg per tonne, 1000 cal per kcal


## plotting controls
# ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      plot.title=element_text(size=rel(1)),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank(),
      plot.margin=unit(c(1,1,1,1), "mm"),
      strip.background=element_blank())
}

theme_set(theme_scz())

## color palettes
# categorical color palette from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

