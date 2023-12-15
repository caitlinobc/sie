# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/13/2023
# PrEP M&E Framework Graphs
# Updated to reflect Q4 FY23 data 
# ----------------------------------------------

# ------------------------
# load packages

rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(plyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(ggrepel)
# ------------------------

# ----------------------------------------------
# DIRECTORIES AND SHAPE FILES
# ----------------------------------------------
# ------------------------
# set working directories 
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/prep/prep_framework/'

# mapping directory
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/maps/shape_files/'
# ------------------------

# ----------------------------------------------
# IMPORT DATA 
# ----------------------------------------------
# --------------------
# import the mapping shape file and convert to a data table
map = shapefile(paste0(mapDir, 'continent/africa_admin0_hdx/africa_admin0_hdx.shp'))
plot(map)
coord = data.table(fortify(map))

# merge in the names of the countries to match the data 
names = data.table(cbind(country = c(map@data$COUNTRY), id =  seq(1:48)))
coord = merge(coord, names, by = 'id')

# fix the map by creating a South Africa specific map
s_africa_layer = geom_polygon(aes(x = long, y = lat, group = group),
                              data = coord[country=='South Africa' & hole==F], fill = NA, 
                              color = 'black')
# --------------------

# --------------------
# import the prep data and rename the columns
dt = data.table(read_excel(paste0(dir, 'prep_framework_data.xlsx')))
setnames(dt, c('country', 'fy', 'prep_new', 'prep_ct', 'dvr', 'cab_la'))

# drop out FY22 for now - it isn't needed - just use it for stats
fy22 = dt[fy==2022]
dt = dt[fy==2023]

# merge in the geospatial data 
dt$country %in% coord$country # check that all the countries matach the shape file
coord = merge(coord, dt, by = 'country', all.x=T)
# --------------------

# ----------------------------------------------
# CREATE THE LABELS
# ----------------------------------------------
# create the labels
labels = data.table(coordinates(map))
setnames(labels, c('long', 'lat'))
labels = cbind(labels, country = names$country)

# add specific variables by indicator
labels = merge(labels, dt, by = 'country', all = T)

# create labels for the values
labels[!is.na(prep_new) , pn_label:=paste0(country, ": ", prep_new)]
labels[!is.na(prep_ct) , ct_label:=paste0(country, ": ", prep_ct)]

# ----------------------------------------------
# MAKE THE MAPS
# ----------------------------------------------

# --------------------
# map of prep_new
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=prep_new)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Spectral'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="PREP_NEW")+
  theme(text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = pn_label, 
       x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
# --------------------

# --------------------
# map of prep_ct
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=prep_ct)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="PREP_CT")+
  theme(text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = ct_label, 
                                      x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
# --------------------

# --------------------
# regulatory map - DVR
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=factor(dvr))) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_manual(values = c('#ffc641', '#fc9272', '#a6bddb'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="DVR Regulatory Status")+
  theme(text=element_text(size=18))+
  s_africa_layer
# --------------------

# --------------------
# regulatory map - CAB-LA
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=factor(cab_la))) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_manual(values = c('#ffc641', '#fcbba1', '#9ecae1'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="CAB-LA Regulatory Status")+
  theme(text=element_text(size=18))+
  s_africa_layer
# --------------------




