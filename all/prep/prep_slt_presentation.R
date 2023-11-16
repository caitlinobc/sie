# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/15/2023
# PrEP SLT presentation
# imports, cleans, and analyzes data on prep_new and prep_ct
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/prep/prep_slt/'

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
# IMPORT DATA 
# ----------------------------------------------
# import prep_new and rename the columns
dt1 = data.table(read.csv(paste0(dir, 'raw/prep_new_fy23_q3_slt.csv')))

setnames(dt1, c('country', 'region', 'district', 'site', 'site2', 
                 'sex', 'age',  'value', 'fq'))
dt1[ , variable:='PREP_NEW']

# import prep_curr and rename the columns
dt2 = data.table(read.csv(paste0(dir, 'raw/prep_ct_fy23_q3_slt.csv')))
setnames(dt2, c('country', 'region', 'district', 'site', 'site2', 
                'sex', 'age',  'value', 'fq'))
dt2[ , variable:='PREP_CT']

# join them
dt = rbind(dt1, dt2)

# ----------------------------------------------
# PREP DATA SET AND EXPORT
# ----------------------------------------------
# ------------------------

# site levels differ by data set - some have sub-counties
# replace the sub-counties with sites and drop sub-counties
# some sites will be missing (military reporting)
dt[site2!="", site:=site2]
dt[ , site2:=NULL]

# add fiscal year
dt[grepl('20', fq), yr:='FY20']
dt[grepl('21', fq), yr:='FY21']
dt[grepl('22', fq), yr:='FY22']
dt[grepl('23', fq), yr:='FY23']

# arrange the order of the variables as preferred
dt = dt[ ,.(country, region, district, site, fq, yr,
            age, sex, variable, value)]

# export the full data set
write.csv(dt, paste0(dir, 'prepped/full_data_q3_fy23.csv'))

# ------------------------


# ----------------------------------------------
# DESCRIPTIVE STATISTICS
# ----------------------------------------------

# total new prep enrollments 2022
dt[variable=='PREP_NEW' & yr=='FY23', sum(value)]
dt[variable=='PREP_NEW' & yr=='FY23', sum(value), by = sex]

# percentage new prep enrollments by sex
dt[variable=='PREP_NEW' & yr=='FY23' & sex=='Female', sum(value)]/dt[variable=='PREP_NEW' & yr=='FY23', sum(value)]
dt[variable=='PREP_NEW' & yr=='FY23' & sex=='Male', sum(value)]/dt[variable=='PREP_NEW' & yr=='FY23', sum(value)]

# total cameroon enrollments (new program)
dt[variable=='PREP_NEW' & country=='Cameroon', sum(value, na.rm=T)]

# total re-initiations, Q4 FY22
dt[variable=='PREP_CT' & fq=='FY22 Q4', sum(value)]
dt[variable=='PREP_CT' & fq=='FY22 Q4', sum(value), by = sex]
dt[variable=='PREP_CT' & fq=='FY23 Q1', sum(value)]

# percentage re-initiations by sex
dt[variable=='PREP_CT' & fq=='FY22 Q4' & sex=='Female', sum(value)]/dt[variable=='PREP_CT' & fq=='FY22 Q4', sum(value)]
dt[variable=='PREP_CT' & fq=='FY22 Q4' & sex=='Male', sum(value)]/dt[variable=='PREP_CT' & fq=='FY22 Q4', sum(value)]
# ----------------------------------------------
# MAPS
# ----------------------------------------------

# ------------------------
# map of prep_new in FY23 by country (african continent)

# sum prep_new to the country level
pn_map = dt[yr=='FY23' & variable=='PREP_NEW', .(prep_new= sum(value)), by = country]
coord = merge(coord, pn_map, by = 'country', all = T)

# --------------------
# create labels

# create the labels
labels = data.table(coordinates(map))
setnames(labels, c('long', 'lat'))
labels = cbind(labels, country = names$country)

# add specific variables by indicator
labels = merge(labels, pn_map, by = 'country', all = T)

# create a label
labels[!is.na(prep_new) , pn_label:=paste0(country, ": ", prep_new)]

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

# ------------------------
# map of prep_CT in FY23 by country (african continent)

# sum prep_ct to the country level
pc_map = dt[fq=='FY23 Q3' & variable=='PREP_CT', .(prep_ct= sum(value)), by = country]
coord = merge(coord, pc_map, by = 'country', all = T)

# --------------------
# create labels

# add prep_ct to the labels
labels = merge(labels, pc_map, by = 'country', all = T)

# create a label
labels[!is.na(prep_ct) , pc_label:=paste0(country, ": ", prep_ct)]

# --------------------
# map of prep_ct
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=prep_ct)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Reds'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="PREP_CT")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = pc_label, 
    x=long, y=lat, group=country), inherit.aes=FALSE, size=5)

# --------------------

# ------------------------
# map of prep_new in Q1 FY23 by country (african continent)

# sum prep_new to the country level
pp_map = dt[fq=='FY23 Q1' & variable=='PREP_NEW', .(prep_new_q1= sum(value)), by = country]
coord = merge(coord, pp_map, by = 'country', all = T)

# --------------------
# create labels

# add specific variables by indicator
labels = merge(labels, pp_map, by = 'country', all = T)

# create a label
labels[!is.na(prep_new_q1) , pp_label:=paste0(country, ": ", prep_new_q1)]

# --------------------
# map of prep_new in fy23 q1
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=prep_new_q1)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Purples'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="PREP_NEW")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = pp_label, 
                                      x=long, y=lat, group=country), inherit.aes=FALSE, size=5)

# ------------------------


