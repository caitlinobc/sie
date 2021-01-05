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
library(grid)
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
  scale_fill_gradientn(colors = '#c7e9c0')+
  geom_point(data = dt[country=='Malawi'], aes(x = long, y =lat, group = country), 
             size = 3, colour = '#feb24c', alpha= 0.5)+
  theme_void(base_size = 14) +
  labs(title="AHD study facilities: Malawi",
       subtitle = expression(italic("n = 21"))) +
  theme(plot.title=element_text(vjust=-1),
        text=element_text(size=16),
        legend.position = 'none') 

# -------------------
# simple introductory map of tanzania

ggplot(shape[country=='Tanzania'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#7fcdbb')+
  geom_point(data = dt[country=='Tanzania'], aes(x = long, y =lat, group = country), 
             size = 4, colour = '#253494', alpha= 0.5)+
  theme_void(base_size = 14) +
  labs(title="AHD study facilities: Tanzania",
       subtitle = expression(italic("n = 9"))) +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6),
        text=element_text(size=16),
        legend.position = 'none') 

# -------------------
# study facilities side by side 


# malawi
f1 = ggplot(shape[country=='Malawi'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#c7e9c0')+
  geom_point(data = dt[country=='Malawi'], aes(x = long, y =lat, group = country), 
             size = 3, colour = '#feb24c', alpha= 0.5)+
  theme_void() +
  labs(title="Malawi") +
  theme(plot.title=element_text(hjust=0.2),
    legend.position = 'none') 

f2 = ggplot(shape[country=='Tanzania'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#c7e9c0')+
  geom_point(data = dt[country=='Tanzania'], aes(x = long, y =lat, group = country), 
             size = 3, colour = '#feb24c', alpha= 0.5)+
  theme_void() +
  labs(title="Tanzania") +
  theme(plot.title=element_text(hjust=0.2),
        legend.position = 'none') 

# plot the maps side by side
grid.arrange(f1, f2, nrow=1, top = textGrob("AHD Study Health Facilities",
        gp=gpar(fontsize=18,font=2)))

