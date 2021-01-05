# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/5/21
# Map health facilities in the AHD Study
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(broom)
library(ggrepel)
library(gridExtra)
library(readxl)
# --------------------

# ---------------------------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/AHD/'

# set the working directory for the shape file
setwd('C:/Users/ccarelli/Documents/data/prepped/')

# set output director
OutDir = paste0(dir, 'outputs/')

# ---------------------------------------
# import data and subset to malawi and tanzania

dt = readRDS(paste0(dir, 'data/prepped_geolocations_ahd.rds'))

# read in the shape file 
shape = readRDS('shape_files_all_countries.rds')
shape = shape[country=='Malawi' |country=='Tanzania']
# ---------------------------------------

# ---------------------------------------
# CREATE MAPS

# -------------------
# simple introductory map of malawi

ggplot(shape[country=='Malawi'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#f7fbff')+
  geom_point(data = dt[country=='Malawi'], aes(x = long, y =lat, group = country), 
             size = 2, colour = '#feb24c', alpha= 0.5)+
  theme_void(base_size = 14) +
  labs(title="Health facilities included in the AHD Study",
       subtitle = 'Malawi') +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6),
        text=element_text(size=16)) 




