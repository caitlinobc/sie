# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/16/21
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

# set the working directory to the shape files
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/'

# set the working directory for the shape file
setwd(paste0(dir, 'shape/'))

# set output directory
OutDir = paste0(dir)

# --------------------
# should you use the drc health zone level shape file?
drc_patch = FALSE

# subset the drc data to only kinshasa?
kin_only = TRUE

# choose which level to run the function at
# options: country, region, district
level = 'district'

# -----------------------------------------
# import the shape files, check, and fortify

# list the shape files
all_files = list.files(paste0(dir, 'shape/'))

# --------------------
# create function levels and choose which level to run the function 

# the country level is always 0
country_files = all_files[grepl("0", all_files)]

# at the regional level, all files use 1 except Cameroon/CDI (2)
# Cameroon and CDI have files at level 1 but 2 is more detailed
# Lesotho only had regions, so use 1 for both region and district
region_files = c(all_files[grepl("1", all_files)], "gadm36_CMR_2_sp.rds",
                 "gadm36_CIV_2_sp.rds")
region_files = region_files[!grepl("CIV_1", region_files) & !grepl('CMR_1', region_files)]

# most countries have admin-2 as 2, a fewas 3 or 1 (Lesotho)
district_files = c(all_files[grepl("_2", all_files)],"gadm36_CMR_3_sp.rds",
                   "gadm36_CIV_3_sp.rds", "gadm36_LSO_1_sp.rds")
district_files = district_files[!grepl("CIV_2", district_files) & !grepl('CMR_2', district_files)]

# set the country level to run the function 
if (level=='country') files = country_files
if (level=='region') files = region_files
if (level=='district') files = district_files
# --------------------

# -----------------------------------------
# loop through each shape file, fortify, and rbind
i = 1
for (f in files) {

shape = readRDS(f) # regional level
plot(shape)

# add the country
country = trimws(sapply(strsplit(f ,"_"), "[", 2), "both")

# test list of the admin level match
sort(unique(shape@data$NAME_1))

# list and keep the names and add region code
if (country %in% 'LSO') { 
  names = data.table(cbind(district = shape@data$NAME_1,
            id = shape@data$GID_1))
            region_code = 'GID_1'
  } else if (country %in% c('COD', 'SWZ', 'KEN', 'MOZ', 'UGA', 'MWI', 'TZA')) {
    names = data.table(cbind(district = shape@data$NAME_2,
            id = shape@data$GID_2))
            region_code = 'GID_2'} else {
    names = data.table(cbind(district = shape@data$NAME_3,
            id = shape@data$GID_3))
            region_code = 'GID_3'}

# label with a country code
names[ , country:=country]

# --------------------
# district labels
labels = data.table(coordinates(shape))
setnames(labels, c('long', 'lat'))
labels = cbind(labels, names)

# --------------------
# fortify the shape file
coord = data.table(fortify(shape, region = region_code)) 
coord[ , country:=country] # label the country of the shape file 

# --------------------
# bind all of the shape files together into a mapping data set
if (i == 1) full_shape = coord
if (1 < i) full_shape = rbind(full_shape, coord)

# create a list of district names to accompany alphanumeric ids
if (i == 1) full_names = names
if (1 < i) full_names = rbind(full_names, names)

# create a list of labels for labelling maps
if (i == 1) full_labels = labels
if (1 < i) full_labels = rbind(full_labels, labels)

i = i+1
} # end of rbind loop

# -----------------------------------------
# merge in the egpaf district name

write.csv(full_names, paste0(dir, 'shape_names/district_names_shape.csv'))



# -----------------------------------------
# substitute the DRC specific alternate shape file 

if(drc_patch==TRUE) source('drc_shape_patch.R')

# -----------------------------------------
# FINAL FORMATTING
# --------------------
# rename the countries

# names are in alphabetical order; convert to factor
# convert both the aggregate shape file and district names
full_shape$country = factor(full_shape$country, 
    c('CMR', 'CIV','COD', 'SWZ', 'KEN', 'LSO', 'MOZ', 'MWI', 'TZA', 'UGA'),                        
     c('Cameroon','Cote d\'Ivoire', 'DRC', 'Eswatini', 'Kenya', 'Lesotho', 
       'Mozambique', 'Malawi', 'Tanzania', 'Uganda'))

full_names$country = factor(full_names$country, 
       c('CMR', 'CIV','COD', 'SWZ', 'KEN', 'LSO', 'MOZ', 'MWI', 'TZA', 'UGA'),                        
       c('Cameroon','Cote d\'Ivoire', 'DRC', 'Eswatini', 'Kenya', 'Lesotho', 
        'Mozambique', 'Malawi', 'Tanzania', 'Uganda'))

# -----------------------------------

# --------------------
# export the district and country names

# export the list of names and ids
saveRDS(full_names, paste0(dir,'shape_names/district_names_ids.rds'))

# export the combined shape file
saveRDS(full_shape, paste0(dir,'shape_names/shape_files_all_countries.rds'))

# export the labels
saveRDS(full_labels, paste0(dir,'shape_names/labels_all_countries.rds'))

# ---------------
# -----------------------------------
