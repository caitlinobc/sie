# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/12/2022
# map for MSA presentation
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/msa/'

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

# ----------------------------------------------
# IMPORT DATA 
# ----------------------------------------------
# import prep_new and rename the columns
dt = data.table(read_xlsx(paste0(dir, 'msa_conference.xlsx')))

# ----------------------------------------------
# MAP
# ----------------------------------------------


# ------------------------
# map of prep_new in FY22 by country (african continent)

# sum prep_new to the country level
coord = merge(coord, dt, by = 'country', all = T)

# --------------------
# create labels

# create the labels
labels = data.table(coordinates(map))
setnames(labels, c('long', 'lat'))
labels = cbind(labels, country = names$country)

# add specific variables by indicator
labels = merge(labels, dt, by = 'country', all = T)

# create a label
labels[!is.na(vls) , vls_label:=paste0(country, ": ", round(vls, 0), '%')]
labels[country=='DRC', vls_label:='DRC: Unknown']
labels[country=='Mozambique', vls_label:='Mozambique: PHIA ongoing']

# --------------------
# map of vls rates from phia
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Spectral'), na.value='#ffffff') + 
  theme_void(base_size =18) +
  labs(fill="Virally suppressed (%)")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = vls_label, 
             x=long, y=lat, group=country), inherit.aes=FALSE, size=3)

