# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/10/20
# Eswatini test map
# Using both shape file and DATIM data 
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

dt = data.table(read.csv(paste0(dir, 
                                'eswatini_map_test/Eswatini Peds Viral Suppression and Coverage Rates.csv')))

# rename the columns
setnames(dt, c('country', 'region', 'district', 'vls', 'vlc'))

# --------------------
# import the shape file, check, and fortify

shape1 = readRDS('gadm36_SWZ_1_sp.rds') # regional level
shape2 = readRDS('gadm36_SWZ_2_sp.rds') # district level
plot(shape1)
plot(shape2)