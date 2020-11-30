# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/30/20
# Initial visualizations of Cameroon Attendiere 95 weekly data
# For testing and data quality checks
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
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/Documents/data/'

# set the working directory for the shape file
setwd('C:/Users/ccarelli/Documents/data/shape_files')

# set output director
OutDir = paste0(dir, 'outputs/')

# --------------------
# import the data 

# dt = data.table(read.csv(paste0(dir, 
#                                 'eswatini_map_test/Eswatini Peds Viral Suppression and Coverage Rates.csv')))
# 
# # rename the columns
# setnames(dt, c('country', 'region', 'district', 'vls', 'vlc'))

# --------------------
# import the shape file, check, and fortify

f = 'gadm36_CMR_3_sp.rds'

shape = readRDS(f) # regional level
plot(shape)

# list and keep the names
names = data.table(cbind(district = shape@data$NAME_2,
                              id = shape@data$GID_2))
# --------------------
# fortify the shape file

coord = data.table(fortify(shape, region = 'GID_2'))

# --------------------





# merge in the names of the districts
dist_coord = merge(dist_coord, dist_names, by = 'id', all.x = TRUE)

