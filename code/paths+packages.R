## paths+packages.R
# This script loads common packages, sets up plotting controls, etc. for use in other scripts.

# packages
library(tidyverse)
library(sf)
library(terra)
library(patchwork)
library(lubridate)

# path to SAFEKAW google drive data folder
path_data <- file.path("G:/My Drive/Projects-Active/SAFEKAW/Data")

# common CRS for everything: EPSG:26915 - https://epsg.io/26915
#  domain spans border of UTM14 and UTM15 so use 15
domain_crs <- st_crs(26915)

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