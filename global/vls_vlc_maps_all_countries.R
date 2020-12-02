# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/30/20
# Format shape files at the district level for mapping
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

# -----------------------------------------
# import the shape file, check, and fortify

# list the shape files
files = list.files(paste0(dir, 'shape_files'))

# loop through each shape file, fortify, and rbind
i = 1
for (f in files) {
  
shape = readRDS(f) # regional level
plot(shape)

# add the country
country = trimws(sapply(strsplit(f ,"_"), "[", 2), "both")

# list and keep the names
if (country %in% c('LSO', 'SWZ')) { 
  names = data.table(cbind(district = shape@data$NAME_1,
            id = shape@data$GID_1))
  } else names = data.table(cbind(district = shape@data$NAME_2,
                              id = shape@data$GID_2))
names[ ,country:=country]

# --------------------
# fortify the shape file

# lesotho and swaziland do not have regions, only districts (admin-1)
if (country %in% c('LSO', 'SWZ')) {region_code = 'GID_1'
 } else region_code = 'GID_2'

# fortify
coord = data.table(fortify(shape, region = region_code)) 
coord[ , country:=country] # label the country of the shape file 

# --------------------
# bind all of the shape files together into a mapping data set
if (i == 1) full_shape = coord
if (1 < i) full_shape = rbind(full_shape, coord)

# create a list of district names to accompany alphanumeric ids
if (i == 1) full_names = names
if (1 < i) full_names = rbind(full_names, names)

i = i+1
} # end of rbind loop

# --------------------

# -----------------------------------
# rename the countries

# names are in alphabetical order; convert to factor
# convert both the aggregate shape file and district names
full_shape$country = factor(full_shape$country, 
    c('CMR', 'CIV','COD', 'KEN', 'LSO', 'MOZ', 'MWI', 'SWZ', 'TZA', 'UGA'),                        
     c('Cameroon','Cote d\'Ivoire', 'DRC', 'Kenya', 'Lesotho', 
       'Mozambique', 'Malawi', 'Swaziland', 'Tanzania', 'Uganda'))

full_names$country = factor(full_names$country, 
   c('CMR', 'CIV','COD', 'KEN', 'LSO', 'MOZ', 'MWI', 'SWZ', 'TZA', 'UGA'),                        
    c('Cameroon','Cote d\'Ivoire', 'DRC', 'Kenya', 'Lesotho', 
    'Mozambique', 'Malawi', 'Swaziland', 'Tanzania', 'Uganda'))


# -----------------------------------

# --------------------
# export the district and country names

saveRDS(full_names, paste0(dir,'prepped/district_names_ids.rds'))

# export the combined shape fill
saveRDS(full_shape, paste0(dir,'prepped/shape_files_all_countries.rds'))

# --------------------

