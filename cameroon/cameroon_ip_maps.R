# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/10/2023
# Create maps of Cameroon Implementing Partners
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
library(scales)
# ------------------------

# ----------------------------------------------
# DIRECTORIES AND SHAPE FILES
# ----------------------------------------------
# ------------------------
# set working directories 
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/datim_units/prepped'

# mapping directory
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/maps/shape_files/country/'
# ------------------------

# --------------------
# import the mapping shape file and convert to a data table
map = shapefile(paste0(mapDir, 'cameroon/cameroon_admin1_hdx.shp'))
plot(map)
coord = data.table(fortify(map))

# merge in the names of the countries to match the data 
names = data.table(cbind(region = c(map@data$ADM1_FR), id =  seq(1:10)))
coord = merge(coord, names, by = 'id')

# --------------------

# --------------------
# merge in the pepfar data

# load the pepfar data 
dt = readRDS(paste0(dir, '/cameroon_pepfar_units.rds'))

# create a summary table to merge to the map
tot = dt[ ,.(sites = length(unique(org_unit_id))), by = .(region, ip)]

# merge it to the shape file
coord = merge(coord, tot, by = 'region', all = T)

# rename GU
coord[ip=='GU', ip:="Georgetown"]

# create the data set with the site information
points = dt[!is.na(latitude) ,.(site, ip, latitude, longitude)]

# --------------------

# --------------------
# create the map - no labels 

ggplot(coord, aes(x=long, y=lat, group=group, fill=sites)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), na.value='#ffffff',labels=comma) + 
  theme_void(base_size =16) +
  labs(fill="Number of PEPFAR-Supported Sites, FY23")+
  theme( text=element_text(size=18))
# --------------------

# --------------------
# create labels

# create the labels
labels = data.table(coordinates(map))
setnames(labels, c('long', 'lat'))
labels = cbind(labels, region = names$region)

# add specific variables by indicator
labels = merge(labels, tot, by = 'region', all = T)

# create the labels
labels[ , label:=paste0(region, " (", ip, "): ", sites)]

# --------------------

# --------------------
# create the map - labels 

ggplot(coord, aes(x=long, y=lat, group=group, fill=sites)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Reds'), na.value='#ffffff',labels=comma) + 
  theme_void(base_size =16) +
  labs(fill="Number of PEPFAR-Supported Sites, FY23")+
  theme( text=element_text(size=18)) +
  geom_label_repel(data = labels, aes(label = label, 
                                      x=long, y=lat, group=region), inherit.aes=FALSE, size=5)
# --------------------

# --------------------
# egpaf map - labels 

coord2 = copy(coord)
coord2[ip!="EGPAF", sites:=NA]

ggplot(coord2, aes(x=long, y=lat, group=group, fill=sites)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Greens'), na.value='#ffffff',labels=comma) + 
  theme_void(base_size =16) +
  labs(fill="Number of PEPFAR-Supported Sites, FY23")+
  theme( text=element_text(size=18)) +
  geom_label_repel(data = labels[ip=='EGPAF'], aes(label = label, 
                                      x=long, y=lat, group=region), inherit.aes=FALSE, size=5)
# --------------------
# create the map with the sites on it

# map of all pepfar sites - no labels
ggplot(coord, aes(x=long, y=lat, group=group, fill=factor(1))) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) +
  scale_fill_manual(values = '#e5f5e0')+
  geom_point(data = points, 
             aes(x = longitude, y = latitude), 
             size = 1.5, colour = '#feb24c', alpha = 0.9, 
             inherit.aes = FALSE) +
  theme_void(base_size =16)+
  guides(fill="none")

# distribution of egpaf sites only
ggplot(coord[ip=='EGPAF'], aes(x=long, y=lat, group=group, fill=factor(1))) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) +
  scale_fill_manual(values = '#ffffff')+
  geom_point(data = points[ip=='EGPAF'], 
             aes(x = longitude, y = latitude), 
             size = 1.5, colour = '#000000', alpha = 0.9, 
             inherit.aes = FALSE) +
  theme_void(base_size =16)+
  guides(fill="none")


ggplot(coord, aes(x=long, y=lat, group=group, fill=sites)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Greens'), na.value='#ffffff',labels=comma) + 
  geom_point(data = points, 
             aes(x = longitude, y = latitude), 
             size = 3, colour = '#feb24c', alpha = 0.9, 
             inherit.aes = FALSE) +
            theme_void(base_size =16) +
            labs(fill="PEPFAR Sites, FY23")+
            theme( text=element_text(size=18))






