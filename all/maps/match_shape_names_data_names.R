# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/22/21
# Match districts in the shape file to districts in the data
# Start with egpaf facilities
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(openxlsx)
# --------------------
# Files and directories

# set the working directory to the location of the egpaf data names
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/prepped/'
setwd(dir)

# shortcut to shape file names directory
shape_dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/shape_names/'

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/prepped/'

# --------------------
# load the function that fixes diacritical marks
source('C:/Users/ccarelli/Documents/GitHub/sie/all/fix_diacritics_function.r')

# --------------------
# import the shape names for regions and districts
shp_reg = readRDS(paste0(shape_dir, 'region_names_ids.rds'))
shp_dist = readRDS(paste0(shape_dir, 'district_names_ids.rds'))

# fix the diacritics to improve the match
shp_dist[ , district:=fix_diacritics(district)]

# --------------------
# CAMEROON

dt = data.table(read.csv(paste0(dir, 'cmr_health_facilities.csv')))

# subset to the relevant variables for egpaf facilities
dt = dt[egpaf==TRUE, .(name, district, region, country)]

# subset to administrative units
dist = dt[ ,.(district = unique(district))]
regions = dt[ ,.(region = unique(region))]

# subset the shape file and check the merge
shp_dist = shp_dist[country=='Cameroon']
shp_dist[ , country:=NULL]

# merge in the matching names
dist = merge(dist, shp_dist, by = 'district', all.x=T)

View(shp_dist[order(district)])
View(dist[is.na(id)][order(district)])





