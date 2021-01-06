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

# create labels from the data to add to the maps
labels = dt[ ,.(facility, country, lat, long, facility_type)]

# ---------------------------------------

# ---------------------------------------
# CREATE MAPS

# -------------------
# simple introductory map of malawi

p1 = ggplot(shape[country=='Malawi'], aes(x=long, y=lat, group=group, fill=1)) + 
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

p2 = ggplot(shape[country=='Tanzania'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#7fcdbb')+
  geom_jitter(data = dt[country=='Tanzania'], aes(x = long, y =lat, group = country), 
             size = 4, colour = '#253494', alpha= 0.5, width = 0.5)+
  theme_void(base_size = 14) +
  labs(title="AHD study facilities: Tanzania",
       subtitle = expression(italic("n = 9")), 
       caption = '*Locations jittered to show sites in similar locations') +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(face = "italic"),
        text=element_text(size=16),
        legend.position = 'none') 

# -------------------
# study facilities side by side 

# malawi
f1 = ggplot(shape[country=='Malawi'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#c7e9b4')+
  geom_point(data = dt[country=='Malawi'], aes(x = long, y =lat, group = country), 
             size = 3, colour = '#f16913', alpha= 0.5)+
  theme_void() +
  labs(title="Malawi") +
  theme(plot.title=element_text(hjust=0.2),
    legend.position = 'none') 

f2 = ggplot(shape[country=='Tanzania'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#c7e9b4')+
  geom_point(data = dt[country=='Tanzania'], aes(x = long, y =lat, group = country), 
             size = 3, colour = '#f16913', alpha= 0.5)+
  theme_void() +
  labs(title="Tanzania") +
  theme(plot.title=element_text(hjust=0.2),
        legend.position = 'none') 

# -------------------
# study facilities by type

# malawi
level_cols_m = c('#d53e4f', '#fed976')

p3 = ggplot(shape[country=='Malawi'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon(show.legend = FALSE)+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#c7e9b4')+
  geom_point(data = dt[country=='Malawi'], inherit.aes = FALSE,
          aes(x = long, y =lat, group = country, shape = level, 
                 color = facility_type), size = 4, alpha = 0.8)+
  scale_color_manual(values = level_cols_m )+
  theme_void() +
  labs(title="AHD study sites by level and type, Malawi", 
       color = 'Facility Type', shape = 'Facility Level') +
  theme(plot.title=element_text(hjust=0.2))

# tanzania
level_cols_t = c('#35978f', '#92c5de')

p4 = ggplot(shape[country=='Tanzania'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon(show.legend = FALSE, alpha = 0.7)+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#feb24c')+
  geom_jitter(data = dt[country=='Tanzania'], inherit.aes = FALSE,
             aes(x = long, y =lat, group = country, shape = level, 
                 color = facility_type), size = 5, alpha = 0.7, width = 1)+
  scale_color_manual(values = level_cols_t )+
  theme_void() +
  labs(title="AHD study sites by level and type, Tanzania", 
       color = 'Facility Type', shape = 'Facility Level',
       caption = '*Locations jittered (displaced) to show overlap') +
  theme(plot.title=element_text(hjust=0.2),
        plot.caption = element_text(face = "italic"))

# ---------------------------------------
# create labels to add to maps

# labeled map of Tanzania
p5 = ggplot(shape[country=='Tanzania'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#7fcdbb')+
  geom_point(data = dt[country=='Tanzania'], aes(x = long, y =lat, group = country), 
              size = 4, colour = '#253494', alpha= 0.7)+
  theme_void(base_size = 14) +
  labs(title="AHD study sites: Tanzania",
       subtitle = expression(italic("n = 9"))) +
  theme(plot.title=element_text(vjust=-1),
        text=element_text(size=16),
        legend.position = 'none')+
geom_label_repel(data = labels[country=='Tanzania'], aes(label = facility, 
          x=long, y=lat), inherit.aes=FALSE, size=3)

# labeled map of Malawi
p6 = ggplot(shape[country=='Malawi'], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#c7e9c0')+
  geom_point(data = dt[country=='Malawi'], aes(x = long, y =lat, group = country), 
             size = 3, colour = '#980043', alpha= 0.8)+
  theme_void(base_size = 14) +
  facet_wrap(~facility_type)+
  labs(title="AHD study sites: Malawi",
       subtitle = expression(italic("n = 21"))) +
  theme(plot.title=element_text(),
        text=element_text(size=16),
        legend.position = 'none')+
  geom_label_repel(data = labels[country=='Malawi'], aes(label = facility, 
          x=long, y=lat, group = facility_type), inherit.aes=FALSE, size=3)


# ---------------------------------------

# -------------------
# export pdf of maps

pdf(paste0(OutDir, 'ahd_facility_maps.pdf'), width = 12, height = 9)
p1
p2
# side by side facility maps
grid.arrange(f1, f2, nrow=1, 
        top = textGrob("AHD Study Sites",
        gp=gpar(fontsize=18,font=2)))
p3
p4
p5
p6

dev.off()

# -------------------

