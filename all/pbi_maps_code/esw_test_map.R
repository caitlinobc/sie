# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# Create a working Eswaitni map for use in PBI
# Create Admin 1 and 2 maps 
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(broom)
library(ggrepel)
library(gridExtra)
library(geojsonio)
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pbi_maps/'

# set the output directory
OutDir = paste0(dir, 'outputs/' )

# set the working directory for the shape file
setwd(paste0(dir, '/district_rds/'))

# --------------------
# load the function that fixes diacritical marks
source('C:/Users/ccarelli/Documents/GitHub/sie/all/fix_diacritics_function.r')

# --------------------
# import the data 

reg_dt = data.table(read.csv(paste0(dir, 'pepfar_test_data/ew_test_data.csv')))

# --------------------
# import the shape file, check, and fortify

# import and plot the shape file
shape = readRDS('gadm36_CMR_3_sp.rds') # district level
plot(shape)

# list the specific districts
shp = data.table(fortify(shape, region = 'GID_3'))

# --------------------
# determine the shape names for the merge

# create a list of names
names = data.table((cbind(district = shape@data$NAME_3, id = shape@data$GID_3)))
shp = merge(shp, names, by = 'id', all.x = TRUE)

# fix diacritical marks in the shape file 
shp[ , district:=fix_diacritics(district)]

# --------------------