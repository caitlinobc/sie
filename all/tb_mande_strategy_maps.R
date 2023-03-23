# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 3/6/2023
# Maps for Pediatric TB M&E Framework
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/tb/'

# mapping directory
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/maps/shape_files/'
# ------------------------

# --------------------
# import the mapping shape file and convert to a data table
map = shapefile(paste0(mapDir, 'continent/africa_admin0_hdx/africa_admin0_hdx.shp'))
plot(map)
coord = data.table(fortify(map))

# merge in the names of the countries to match the data 
names = data.table(cbind(country = c(map@data$COUNTRY), id =  c(0, seq(1:47))))
coord = merge(coord, names, by = 'id')

# fix the map by creating a South Africa specific map
s_africa_layer = geom_polygon(aes(x = long, y = lat, group = group),
                              data = coord[country=='South Africa' & hole==F], fill = NA, 
                              color = 'black')
# --------------------

# --------------------
# IMPORT THE DATA 

# import prep_new and rename the columns
dt = data.table(read.csv(paste0(dir, 'raw/tb_maps_strategy_fy22.csv')))

setnames(dt, c('country', 'tb_stat_n', 'tb_stat_d', 'tb_stat_pos_n'))
dt[ , tb_stat:=round((100*tb_stat_n/tb_stat_d), 1)]
dt[ , tb_stat_pos:=round((100*tb_stat_pos_n/tb_stat_n), 1)]
# --------------------

# ------------------------
# CREATE THE MAP 

coord = merge(coord, dt, by = 'country', all = T)

# --------------------
# create labels

# create the labels
labels = data.table(coordinates(map))
setnames(labels, c('long', 'lat'))
labels = cbind(labels, country = names$country)

# add specific variables by indicator
coord2 = coord[ ,.(country, id, tb_stat_n, tb_stat_d, tb_stat_pos_n, tb_stat, tb_stat_pos)]
labels = merge(labels, coord2, by = 'country', all = T)

# create a label
labels[!is.na(tb_stat) , tb_stat_label:=paste0(country, ": ", tb_stat, "%")]
labels[!is.na(tb_stat_pos) , tb_stat_pos_label:=paste0(country, ": ", tb_stat_pos, "%")]
labels = labels[!is.na(tb_stat)]
labels = labels[duplicated(labels)==FALSE]

# --------------------
# map of tb_stat
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=tb_stat)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Spectral'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="TB_STAT (%)")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels[!is.na(tb_stat_pos)], aes(label = tb_stat_label, 
    x=long, y=lat, group=country), inherit.aes=FALSE, size=5)

# ------------------------

# --------------------
# map of tb_stat_pos
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=tb_stat_pos)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Reds'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="TB_STAT_POS (%)")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels[!is.na(tb_stat_pos)], aes(label = tb_stat_pos_label, 
                                                           x=long, y=lat, group=country), inherit.aes=FALSE, size=5)

# ------------------------


