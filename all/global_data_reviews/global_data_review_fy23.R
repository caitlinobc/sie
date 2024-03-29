# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/21/2023
# GDR presentation - December 2023
# Imports, cleans, and analyzes data for the GDR
# Created vector maps to be presented at the GDR
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/gdr/'

# mapping directory
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/maps/shape_files/'
# ------------------------

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

# ----------------------------------------------
# IMPORT & FORMAT DATA 
# ----------------------------------------------
# import prep_new and rename the columns
dt1 = data.table(read.csv(paste0(dir, 'gdr_dec23_maps_data.csv')))

# use this code if the .csv converts to a single line data set
# # split the single column
# setnames(dt1, 'col')
# 
# # unmess up the data 
# dt1[ , award:=unlist(lapply(str_split(col, '\\\t'), '[', 1))]
# dt1[ , country:=unlist(lapply(str_split(col, '\\\t'), '[', 2))]
# dt1[ , fq:=unlist(lapply(str_split(col, '\\\t'), '[', 3))]
# dt1[ , age:=unlist(lapply(str_split(col, '\\\t'), '[', 4))]
# dt1[ , hts_tst:=as.numeric(unlist(lapply(str_split(col, '\\\t'), '[', 5)))]
# 
# dt1[ , hts_tst_pos:=as.numeric(unlist(lapply(str_split(col, '\\\t'), '[', 6)))]
# dt1[ , tx_curr:=as.numeric(unlist(lapply(str_split(col, '\\\t'), '[', 7)))]
# dt1[ , col:=NULL]

setnames(dt1, c('award', 'country', 'fq', 'age', 'hts_tst', 'hts_tst_pos', 'tx_curr'))

# load in the kenya and drc lisanga data and format, merge 
plus = data.table(read_excel(paste0(dir, 'kenya_gdr_values.xlsx')))
dt1 = rbind(dt1, plus)

# create a data set at the country, not award, level
# ihap values for q2 fy23 added manually (sent via e-mail, not in datim)
dt = dt1[ ,.(hts_tst = sum(hts_tst, na.rm=T), hts_tst_pos = sum(hts_tst_pos, na.rm=T),
             tx_curr = sum(tx_curr, na.rm=T)), by = .(country, fq, age)]

# calculate test positivity at the country level
dt[ , hts_rate:=round(((hts_tst_pos/hts_tst)*100), 1)]

# create a data set at the country level without age disaggregations
dt_all = dt1[ ,.(hts_tst = sum(hts_tst, na.rm=T), hts_tst_pos = sum(hts_tst_pos, na.rm=T),
                 tx_curr = sum(tx_curr, na.rm=T)), by = .(country, fq)]

# calculate test positivity at the country level
dt_all[ , hts_rate:=round(((hts_tst_pos/hts_tst)*100), 1)]

# create a data set at the country level without age disaggregations for all of fy23
# do not include tx_curr as it is not additive
dt_all23 = dt1[grepl('23', fq),.(hts_tst = sum(hts_tst, na.rm=T),
                   hts_tst_pos = sum(hts_tst_pos, na.rm=T)), by = .(country)]

# tx_curr is not additive for the fy23 total data
# use the most recent total value - e.g. Q4
tx = dt_all[fq=='FY23 Q4', .(tx_curr)]
dt_all23 = cbind(dt_all23, tx)

# calculate test positivity at the country level
dt_all23[ , hts_rate:=round(((hts_tst_pos/hts_tst)*100), 1)]
# 
# #----------------------
# # ICT map
# 
# # import prep_new and rename the columns
# ind = data.table(read.csv(paste0(dir, 'hts_index_contrib.csv')))
# setnames (ind, 'col')
# 
# ind[ , award:=unlist(lapply(str_split(col, '\\\t'), '[', 1))]
# ind[ , country:=unlist(lapply(str_split(col, '\\\t'), '[', 2))]
# ind[ , pos:=as.numeric(unlist(lapply(str_split(col, '\\\t'), '[', 3)))]
# ind[ , index_pos:=as.numeric(unlist(lapply(str_split(col, '\\\t'), '[', 4)))]
# ind[ , col:=NULL]
# 
# setnames(ind, c('award', 'country', 'pos', 'index_pos'))
# 
# # add in the ihap and lisanga data
# 
# vector1 = c(award = 'IHAP', country = 'DRC', pos = 878, index_pos = 420)
# vector2 = c(award = 'Lisanga', country = 'DRC', pos = 1878, index_pos = 675)
# vector = data.table(rbind(vector1, vector2))
# ind = rbind(ind, vector)
# ind[ , pos:=as.numeric(pos)]
# ind[ , index_pos:=as.numeric(index_pos)]
# 
# # create a variable showing the contribution of index case testing
# ind = ind[ , .(pos = sum(pos), index_pos = sum(index_pos)), by = country]
# ind[ , contribution:=round(100*(index_pos/(pos+index_pos)), 1)]
# ind = ind[ ,.(country, contribution)]
# 
# # merge the contributions into the main data set
# dt_all23 = merge(dt_all23, ind, by = 'country')
# 
# #----------------------

# ----------------------------------------------
# MAPS
# ----------------------------------------------

# ------------------------
# maps for all of fy23
fy23_map = merge(coord, dt_all23, by = 'country', all = T)

# --------------------
# create labels

# create the labels
labels = data.table(coordinates(map))
setnames(labels, c('long', 'lat'))
labels = cbind(labels, country = names$country)

# add specific variables by indicator
labels = merge(labels, dt_all23, by = 'country', all = T)

# create the labels
labels[!is.na(hts_tst) , hts_tst_label:=paste0(country, ": ", hts_tst)]
labels[!is.na(hts_tst) , hts_tst_pos_label:=paste0(country, ": ", hts_tst_pos)]
labels[!is.na(hts_tst) , tx_curr_label:=paste0(country, ": ", tx_curr)]
labels[!is.na(hts_tst) , hts_rate_label:=paste0(country, ": ", hts_rate, '%')]
# labels[!is.na(contribution) , contribution_label:=paste0(country, ": ", contribution, '%')]

# --------------------
# map of hts_tst, fy23 

ggplot(fy23_map[country!='South Africa'], aes(x=long, y=lat, group=group, fill=hts_tst)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Reds'), na.value='#ffffff',labels=comma) + 
  theme_void(base_size =16) +
  labs(fill="Tested for HIV (HTS_TST), FY23")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = hts_tst_label, 
                  x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
# ------------------------

# --------------------
# map of hts_tst_pos, fy23 

ggplot(fy23_map[country!='South Africa'], aes(x=long, y=lat, group=group, fill=hts_tst_pos)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), na.value='#ffffff',labels=comma) + 
  theme_void(base_size =16) +
  labs(fill="Tested HIV+ (HTS_TST_POS), FY23")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = hts_tst_pos_label, 
                                      x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
# ------------------------

# --------------------
# map of test positivity, fy23 

ggplot(fy23_map[country!='South Africa'], aes(x=long, y=lat, group=group, fill=hts_rate)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Purples'), na.value='#ffffff',labels=comma) + 
  theme_void(base_size =16) +
  labs(fill="Test Positivity (%), FY23")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = hts_rate_label, 
                                      x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
# ------------------------

# --------------------
# map of art cohort, q4 fy23 

ggplot(fy23_map[country!='South Africa'], aes(x=long, y=lat, group=group, fill=tx_curr)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'RdYlBu'), na.value='#ffffff',labels=comma) + 
  theme_void(base_size =16) +
  labs(fill="ART Cohort (TX_CURR), Q4 FY23")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = tx_curr_label, 
                                      x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
# ------------------------http://127.0.0.1:11517/graphics/plot_zoom_png?width=1904&height=978

# --------------------
# map of ict contribution, fy23 
# 
# ggplot(fy23_map[country!='South Africa'], aes(x=long, y=lat, group=group, fill=contribution)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   scale_fill_gradientn(colors = brewer.pal(9, 'Purples'), na.value='#ffffff',labels=comma) + 
#   theme_void(base_size =16) +
#   labs(fill="ICT Contribution (%), FY23")+
#   theme( text=element_text(size=18))+
#   s_africa_layer+
#   geom_label_repel(data = labels, aes(label = contribution_label, 
#                 x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
# ------------------------

# -------------------------------------------
# facet wrapped pediatric map
# -------------------------------------------

# create the pediatric data set
dt_kids = dt[age=='Child (<15)']

# add a fiscal year variable
dt_kids[ , fy:=unlist(lapply(str_split(fq, ' +'), '[', 1))]

# sum to the fiscal year level 
dt_kids = dt_kids[ ,.(hts_tst = sum(hts_tst, na.rm=T), hts_tst_pos = sum(hts_tst_pos, na.rm=T)),
             by = .(country, fy)]

# merge the data set with the coordinates
coord1 = merge(coord, dt_kids[fy=='FY22'], by = 'country', all=T)
coord1[, fy:='FY22']
coord2 = merge(coord, dt_kids[fy=='FY23'], by = 'country', all=T)
coord2[, fy:='FY23']
coord_kids_fy = rbind(coord1, coord2)

# shape the data long
coord_kids_long = melt(coord_kids_fy, id.vars = c('country', 'id', 'long', 'lat', 'order',
                                'hole', 'piece', 'group', 'fy'))

# --------------------
# create labels

# create the labels
klabels = data.table(coordinates(map))
setnames(klabels, c('long', 'lat'))
klabels = cbind(klabels, country = names$country)

# add specific variables by indicator
klabels1 = merge(klabels, dt_kids[fy=='FY22'], by = 'country', all = T)
klabels2 = merge(klabels, dt_kids[fy=='FY23'], by = 'country', all = T)
klabels = rbind(klabels1, klabels2)

# create the labels
klabels[!is.na(hts_tst) , hts_tst_label:=paste0(country, ": ", hts_tst)]
klabels[!is.na(hts_tst) , hts_tst_pos_label:=paste0(country, ": ", hts_tst_pos)]
# --------------------

# -------------------------------------------
# create the map of hiv testing among kids by quarter
ggplot(coord_kids_long[country!='South Africa' & variable=='hts_tst'], 
       aes(x=long, y=lat, group=group, fill=value)) + 
  facet_wrap(~fy)+
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'YlOrRd'), na.value='#ffffff',labels=comma) + 
  theme_void(base_size =16) +
  labs(fill="Children tested for HIV (HTS_TST)")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = klabels, aes(label = hts_tst_label, 
              x=long, y=lat, group=country), inherit.aes=FALSE, size=5)

# -------------------------------------------
# create the map of hiv cases identified among kids by quarter
ggplot(coord_kids_long[country!='South Africa' & variable=='hts_tst_pos'], 
       aes(x=long, y=lat, group=group, fill=value)) + 
  facet_wrap(~fy)+
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'YlOrRd'), na.value='#ffffff',labels=comma) + 
  theme_void(base_size =16) +
  labs(fill="Pediatric HIV Cases (HTS_TST_POS)")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = klabels, aes(label = hts_tst_pos_label, 
                                       x=long, y=lat, group=country), inherit.aes=FALSE, size=5)

# -------------------------------------------
