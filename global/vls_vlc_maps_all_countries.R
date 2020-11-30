# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/30/20
# Visualize vira load coverage and suppression
# All SIE supported countries for 12/7 data review
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

# list the shape files
files = list.files(paste0(dir, 'shape_files'))

# loop through each shape file, fortify, and rbind
i = 1
for (f in files) {
  
shape = readRDS(f) # regional level
plot(shape)

# list and keep the names
names = data.table(cbind(district = shape@data$NAME_2,
                              id = shape@data$GID_2))
# add the country
country = trimws(sapply(strsplit(f ,"_"), "[", 2), "both")

# --------------------
# fortify the shape file

coord = data.table(fortify(shape, region = 'GID_2')) 
coord[ , country:=country] # label the country of the shape file 

# --------------------
# bind all of the shape files together into a mapping data set
if (i == 1) full_shape = coord
if (1 <= i) full_shape = rbind(full_shape, coord)
i = i+1
}

# --------------------





# merge in the names of the districts
dist_coord = merge(dist_coord, dist_names, by = 'id', all.x = TRUE)

