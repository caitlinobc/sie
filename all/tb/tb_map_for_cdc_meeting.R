# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/26/24
# TB Maps for CDC Meeting 
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

# remove scientific notation
options(scipen=999)
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
dt = data.table(read_excel(paste0(dir, 'tb_data.xlsx')))

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
labels[!is.na(tx_tb_d_q4_fy23_screened) , txtb_d_label:=paste0(country, ": ", tx_tb_d_q4_fy23_screened)]
labels[!is.na(tx_tb_n_fy23_treated) , txtb_n_label:=paste0(country, ": ", tx_tb_n_fy23_treated)]
labels[!is.na(tb_prev_d_fy23_tpt) , tbprev_label:=paste0(country, ": ", tb_prev_d_fy23_tpt)]
# ----------------------------------------------

# --------------------
# map of tb screening
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=tx_tb_d_q4_fy23_screened)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="Screened for TB")+
  theme(text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = txtb_d_label, 
          x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
# --------------------

# --------------------
# map of tb treatment among art patients
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=tx_tb_n_fy23_treated)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Greens'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill='Enrolled on TB Treatment')+
  theme(text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = txtb_n_label, 
                   x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
# --------------------

# --------------------
# map of initiation of tpt
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=tb_prev_d_fy23_tpt)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Purples'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill='Started TPT')+
  theme(text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = tbprev_label, 
                                      x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
# --------------------