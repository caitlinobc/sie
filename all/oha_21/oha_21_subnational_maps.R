# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/31/21
# Maps of Indicators for the 2021 OHA Meeting 
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(openxlsx)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(rgdal)
library(ggrepel)
library(maptools)
library(gridExtra)
# --------------------

# ----------------------------------------------
# set files and directories

# import the shape file to view the regions, districts
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps_new/shape_files/'

dataDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/oha_21/'

# output the meta data 
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Global/Presentations/OHA Meeting 2021/'

# ----------------------------------------------

# ----------------------------------------------
# import the maps for analysis 
# lesotho and tanzania are at the regional level; drc, eswatini, and uganda district level

# import drc
dc_map = shapefile(paste0(mapDir, 'district/kinshasa_districts_hdx/kinshasa_districts_hdx.shp'))
dc_coord = data.table(fortify(dc_map, region = 'Nom'))
dc_coord[ , country:='DRC']

# import eswatini
ew_map = shapefile(paste0(mapDir, 'district/eswatini_districts_hdx/eswatini_districts_hdx.shp'))
ew_coord = data.table(fortify(ew_map, region = 'ADM2_EN'))
ew_coord[ , country:='Eswatini']

# import lesotho
lt_map = shapefile(paste0(mapDir, 'region/lesotho_regions_hdx/lesotho_regions_hdx.shp'))
lt_coord = data.table(fortify(lt_map, region = 'ADM1_EN'))
lt_coord[ , country:='Lesotho']

#import tanzania
tz_map = shapefile(paste0(mapDir, 'region/tanzania_regions_hdx/tanzania_regions_hdx.shp'))
tz_coord = data.table(fortify(tz_map, region = 'ADM1_EN'))
tz_coord[ , country:='Tanzania']

# import uganda
ug_map = shapefile(paste0(mapDir, 'district/uganda_districts_hdx/uganda_districts_hdx.shp'))
ug_coord = data.table(fortify(ug_map, region = 'ADM2_EN'))
ug_coord[ , country:='Uganda']
# --------------------

# --------------------
# bind the maps into a single file
coord = rbind(dc_coord, ew_coord, lt_coord, tz_coord, ug_coord)

# add grant names
coord[country=='DRC', award:='IHAP']
coord[country=='Eswatini', award:='EHPCS']
coord[country=='Lesotho', award:='PUSH']
coord[country=='Tanzania', award:='Boresha Afya']
coord[country=='Uganda', award:='RHITES-SW']

# repeat for six quarters in order to bind with data 
coord1 = cbind(coord, fq = 'FY20 Q1')
coord2 = cbind(coord, fq = 'FY20 Q2')
coord3 = cbind(coord, fq = 'FY20 Q3')
coord4 = cbind(coord, fq = 'FY20 Q4')
coord5 = cbind(coord, fq = 'FY21 Q1')
coord6 = cbind(coord, fq = 'FY21 Q2')
coord7 = cbind(coord, fq = 'FY21 Q3')

# bind them together
coord = rbind(coord1, coord2, coord3, coord4, coord5, coord6, coord7)

# delete all the unecessary data tables
rm(coord1, coord2, coord3, coord4, coord5, coord6, coord7, dc_coord, 
   ew_coord, lt_coord, tz_coord, ug_coord)
# --------------------

# ----------------------------------------------
# read in the data sets and format

# -------------------------------
# read in the prep data 
pt = data.table(read.csv(paste0(dataDir, 'oha_21_prep_for_mapping.csv')))
setnames(pt, c('award', 'country', 'region', 'district', 'level6', 'level7',
               'PREP_NEW', 'PREP_CURR', 'fq'))

# sum to the district level
pt = pt[ , .(PREP_NEW = sum(PREP_NEW, na.rm = T), PREP_CURR = sum(PREP_CURR, na.rm = T)),
    by = .(award, country, region, district, fq)]

# sum to the appropriate level
pt_reg = pt[country %in% c('Lesotho', 'Tanzania')]
pt = pt[!(country %in% c('Lesotho', 'Tanzania'))]

# sum tanzania and lesotho to the regional level
pt_reg = pt_reg[ , .(PREP_NEW = sum(PREP_NEW, na.rm = T), PREP_CURR = sum(PREP_CURR, na.rm = T)),
         by = .(award, country, district = region, fq)]
pt_reg[ ,region:=NA]

# bind the data tables back together at the same level
pt = rbind(pt, pt_reg)

# rename district id in order to match with the shape files 
setnames(pt, 'district', 'id')

# drop award for the merge - already in the shape files
pt[ , award:= NULL]
rm(pt_reg)

# ----------------------------------------------
# merge the coordinates and the data 

# merge in the prep data at the quarter, district or region level
coord = merge(coord, pt, by = c('country', 'id', 'fq'), all.x = T)

# --------------------
# format the coordinates for mapping 

# drop out the FY21 quarters for Uganda
coord = coord[!(country=='Uganda' & grepl('21', fq))]

# factor the quarter names
coord$fq = factor(coord$fq, c('FY20 Q1', 'FY20 Q2', 'FY20 Q3', 'FY20 Q4',
                  'FY21 Q1', 'FY21 Q2', 'FY21 Q3'), 
                  c('Q1 FY20', 'Q2 FY20', 'Q3 FY20', 'Q4 FY20',
                    'Q1 FY21', 'Q2 FY21', 'Q3 FY21'))

# factor the data to match
pt$fq = factor(pt$fq, c('FY20 Q1', 'FY20 Q2', 'FY20 Q3', 'FY20 Q4',
                              'FY21 Q1', 'FY21 Q2', 'FY21 Q3'), 
                  c('Q1 FY20', 'Q2 FY20', 'Q3 FY20', 'Q4 FY20',
                    'Q1 FY21', 'Q2 FY21', 'Q3 FY21'))

# --------------------
# add labels 

# create the list of labels 
dc_labels = cbind(coordinates(dc_map), dc_map@data$Nom, country = 'DRC')
ew_labels = cbind(coordinates(ew_map), ew_map@data$ADM2_EN, country = 'Eswatini')
lt_labels = cbind(coordinates(lt_map), lt_map@data$ADM1_EN, country = 'Lesotho')
tz_labels = cbind(coordinates(tz_map), tz_map@data$ADM1_EN, country = 'Tanzania')
ug_labels = cbind(coordinates(ug_map), ug_map@data$ADM2_EN, country = 'Uganda')

# bind the labels together and format
labels = data.table(rbind(dc_labels, ew_labels, lt_labels, tz_labels, ug_labels))
setnames(labels, c('long', 'lat', 'id', 'country'))

# create seven quarters of labels
labels = rbind(labels, labels, labels, labels, labels, labels, labels)

# name the fiscal quarters - multiples of data table length
labels[1:266, fq := 'Q1 FY20']
labels[267:532, fq := 'Q2 FY20']
labels[533:798, fq := 'Q3 FY20']
labels[799:1064, fq := 'Q4 FY20']
labels[1065:1330, fq := 'Q1 FY21']
labels[1331:1596, fq := 'Q2 FY21']
labels[1597:1862, fq := 'Q3 FY21']

# format the data set
labels[ , long:=as.numeric(long)]
labels[ , lat:=as.numeric(lat)]
labels$fq = factor(labels$fq, c('Q1 FY20', 'Q2 FY20', 'Q3 FY20', 'Q4 FY20',
                  'Q1 FY21', 'Q2 FY21', 'Q3 FY21'), 
                   c('Q1 FY20', 'Q2 FY20', 'Q3 FY20', 'Q4 FY20',
                     'Q1 FY21', 'Q2 FY21', 'Q3 FY21'))

# check the values
labels[ ,.N, by = fq]

# drop unecessary data tabkes 
rm(dc_labels, ew_labels, lt_labels, tz_labels, ug_labels)

# --------------------
# merge in the value labels

labels = merge(labels, pt, by = c('country', 'id', 'fq'), all.x = T)

# format the labels
labels[!is.na(PREP_NEW), pn_label:=paste0(id, ': ', PREP_NEW)]
labels[!is.na(PREP_CURR), pc_label:=paste0(id, ': ', PREP_CURR)]

# ----------------------------------------------
# CREATE PREP MAPS 

# --------------------
# PREP_NEW MAPS (all quarters)

# set the index and create the list
prep_new_maps = NULL
i = 1

for (c in unique(coord$country)) {

  # set the country name
  country_name = as.character(c)

  # plot the map with a legend and labels for the regions
  # drop out the FY20 quarters that do not include PREP data 
  prep_new_maps[[i]] = ggplot(coord[country==c & fq!='Q1 FY20' & fq!='Q3 FY20'], 
          aes(x=long, y=lat, group=group, fill=PREP_NEW)) + 
  coord_fixed() +
  geom_polygon() + 
    geom_path(size = 0.01, color = '#ffffff')+
  scale_fill_gradientn(colors = brewer.pal(9, 'YlOrRd'), na.value = '#d9d9d9')+
    geom_label_repel(data = labels[country==c & fq!='Q1 FY20' & fq!='Q3 FY20'], 
      aes(label = pn_label, x=long, y=lat, group=region), size=2,
      inherit.aes = F, max.overlaps = 136)+
  facet_wrap(~fq)+
  theme_void(base_size =16)+
  labs(fill="PREP_NEW", 
       title = paste0('Total new enrollments on PrEP (PREP_NEW), ', 
                      country_name))
  # add to the index
  i = i+1
  }
# --------------------

# --------------------
# PREP_CURR MAPS (all quarters)

# set the index and create the list
prep_curr_maps = NULL
i = 1

for (c in unique(coord$country)) {
  
  # set the country name
  country_name = as.character(c)
  
  # plot the map with a legend and labels for the regions
  # drop out the FY20 quarters that do not include PREP data 
  prep_curr_maps[[i]] = ggplot(coord[country==c & fq!='Q1 FY20' & fq!='Q3 FY20'], 
                               aes(x=long, y=lat, group=group, fill=PREP_CURR)) + 
    coord_fixed() +
    geom_polygon() + 
    geom_path(size = 0.01, color = '#ffffff')+
    scale_fill_gradientn(colors = brewer.pal(9, 'PuBuGn'), na.value = '#737373')+
    geom_label_repel(data = labels[country==c & fq!='Q1 FY20' & fq!='Q3 FY20'], 
                     aes(label = pc_label, x=long, y=lat, group=region), size=2,
                     inherit.aes = F, max.overlaps = 136)+
    facet_wrap(~fq)+
    theme_void(base_size =16)+
    labs(fill="PREP_NEW", 
         title = paste0('Current PrEP clients (PREP_CURR), ', 
                        country_name))
  
  # add to the index
  i = i+1
}
# --------------------

# ----------------------------------------------
# print the pdfs of the maps

# prep maps, labelled by country (both indicators)
pdf(paste0(outDir, 'PrEP Indicator Maps.pdf') , width = 12, height = 10)
for (p in 1:length(prep_new_maps)) {
  print(prep_new_maps[[p]])
  print(prep_curr_maps[[p]])
} 
dev.off()


# ----------------------------------------------


