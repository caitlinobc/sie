# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 9/16/21
# Create an RDS file of all maps
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(rgdal)
library(ggrepel)
library(maptools)
library(gridExtra)
# --------------------

# ----------------------------------------------
# set files and directories



dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps_new/shape_files/'

for (c in countries) {
  file = paste0(dir, c, 'regions_hdx.shp')
  map = shapefile(file)
  
  
  
}