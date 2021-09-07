# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/26/21
# 2021 OHA Meeting Analysis 
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
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps_new/original_shape_files/'

# output the meta data 
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps_new/'

# input the edited shape file for the final check
mapFinal = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps_new/shape_files/'

# ----------------------------------------------
# --------------------
# export a list of names of the administrative units
# used in power BI to check against the EGPAF data names
# set arguments for the function

# select region or district level shape files
level = 'region'

# set the county
country  = 'Cameroon'

# are you running the raw or edited shape file?
file_type = 'edited'
# --------------------

# ----------------------------------------------
# read in the shape file and export a list of administrative units

# --------------------
# REGIONAL MAPS
# set the name of the file based on the country for the original shape files 
if (level=='region') {
if (file_type == 'raw') {
  if (country=='Cameroon') file = paste0(mapDir, 'region/cmr_admbnda_inc_20180104_SHP/cmr_admbnda_adm1_inc_20180104.shp')
  if (country=='DRC') file = paste0(mapDir, 'region/cod_admbnda_rgc_itos_20190911_shp/cod_admbnda_adm1_rgc_itos_20190911.shp')
  if (country=='Eswatini') file = paste0(mapDir, 'region/swz_admbnda_cso2007_shp/swz_admbnda_adm1_CSO_2007.shp')
  if (country=='Lesotho') file = paste0(mapDir, 'region/lso_adm_fao_mlgca_2019/lso_admbnda_adm1_FAO_MLGCA_2019.shp')
  if (country=='Malawi') file = paste0(mapDir, 'region/mwi_adm_nso_20181016_shp/mwi_admbnda_adm1_nso_20181016.shp')
  if (country=='Mozambique') file = paste0(mapDir, 'region/moz_adm_20190607b_shp/moz_admbnda_adm1_ine_20190607.shp')
  if (country=='Tanzania') file = paste0(mapDir, 'region/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp')
  # regional file for uganda not yet created 
}
# set the name of the file based on the country for the edited shape files 
if (file_type == 'edited') {
  if (country=='Cameroon') file = paste0(mapFinal, 'region/cameroon_regions_hdx/cameroon_regions_hdx.shp')
  if (country=='DRC') file = paste0(mapFinal, 'region/drc_regions_hdx/drc_regions_hdx.shp')
  if (country=='Eswatini') file = paste0(mapFinal, 'region/eswatini_regions_hdx/eswatini_regions_hdx.shp')
  if (country=='Lesotho') file = paste0(mapFinal, 'region/lesotho_regions_hdx/lesotho_regions_hdx.shp')
  if (country=='Malawi') file = paste0(mapFinal, 'region/malawi_regions_hdx/malawi_regions_hdx.shp')
  if (country=='Mozambique') file = paste0(mapFinal, 'region/mozambique_regions_hdx/mozambique_regions_hdx.shp')
  if (country=='Tanzania') file = paste0(mapFinal, 'region/tanzania_regions_hdx/tanzania_regions_hdx.shp')
  
}}
# --------------------

# --------------------
# DISTRICT MAPS 
# set the name of the file based on the country for the original shape files 
if (level=='district') {
if (file_type == 'raw') {
  if (country=='DRC') file = paste0(mapDir, 'district/rdc_zone_de_sante_09092019/RDC_Zone_de_sante_09092019.shp')
  if (country=='Eswatini') file = paste0(mapDir, 'district/swz_admbnda_cso2007_shp/swz_admbnda_adm2_CSO_2007.shp')
  if (country=='Lesotho') file = paste0(mapDir, 'district/lso_adm_fao_mlgca_2019/lso_admbnda_adm2_FAO_MLGCA_2019.shp')
  if (country=='Malawi') file = paste0(mapDir, 'district/mwi_adm_nso_20181016_shp/mwi_admbnda_adm2_nso_20181016.shp')
  if (country=='Mozambique') file = paste0(mapDir, 'district/moz_adm_20190607b_shp/moz_admbnda_adm2_ine_20190607.shp')
  if (country=='Uganda') file = paste0(mapDir, 'district/uga_admbnda_ubos_20200824_shp/uga_admbnda_adm2_ubos_20200824.shp')
}

# set the name of the file based on the country for the edited shape files 
if (file_type == 'edited') {
  if (country=='DRC') file = paste0(mapFinal, 'district/kinshasa_districts_hdx/kinshasa_districts_hdx.shp')
  if (country=='Eswatini') file = paste0(mapFinal, 'district/eswatini_districts_hdx/eswatini_districts_hdx.shp')
  if (country=='Malawi') file = paste0(mapFinal, 'district/malawi_districts_hdx/malawi_districts_hdx.shp')
  if (country=='Mozambique') file = paste0(mapFinal, 'district/mozambique_districts_hdx/mozambique_districts_hdx.shp')
  if (country=='Uganda') file = paste0(mapFinal, 'district/uganda_districts_hdx/uganda_districts_hdx.shp')
}}
# --------------------
# ----------------------------------------------

# ----------------------------------------------
# for either the original or edited file: 

# read in the file and fortify
map = shapefile(file)
coord = data.table(fortify(map))

# set the number of administrative boundaries to import
# this should be one less than the number of areas as IDs include 0
if (level=='region'){
  if (country=='Cameroon') x = 9
  if (country=='DRC') x = 25
  if (country=='Eswatini') x = 3
  if (country=='Lesotho') x = 9 
  if (country=='Malawi') x = 2 
  if (country=='Mozambique') x = 10 
  if (country=='Tanzania') x = 30
}

if (level=='district'){
  if (country=='DRC' & file_type == 'raw') x = 518
  if (country=='DRC' & file_type == 'edited') x = 34
  if (country=='Eswatini') x = 54
  if (country=='Lesotho') x = 77
  if (country=='Malawi') x = 31 
  if (country=='Mozambique') x = 157 
  if (country=='Uganda') x = 134 }

#--------------------------
# attach the names of the administrative districts 
if (country!='DRC' & country!='Mozambique' & country!='Cameroon') {
  if (level=='region') names = data.table(cbind(region = c(map@data$ADM1_EN), id =  c(0, seq(1:x))))
  if (level=='district') names = data.table(cbind(region = c(map@data$ADM2_EN), id =  c(0, seq(1:x))))
  } else { if (level=='region' & country!='Mozambique') names = data.table(cbind(region = c(map@data$ADM1_FR), id =  c(0, seq(1:x))))
  if (level=='district' & country=='DRC') names = data.table(cbind(region = c(map@data$Nom), id =  c(0, seq(1:x)))) }

# for mozambique - use the portugese names of regions and districts
if (country=='Mozambique'& level=='region' ) names = data.table(cbind(region = c(map@data$ADM1_PT), id =  c(0, seq(1:x)))) 
if (country=='Mozambique'& level=='district') names = data.table(cbind(region = c(map@data$ADM2_PT), id =  c(0, seq(1:x))))

#--------------------------

# finalize the file for mapping
coord = merge(coord, names, by = 'id')

# export the list of regions to check against the EGPAF data 
areas = data.table(areas = coord[ ,unique(region)])
write.csv(areas, paste0(outDir, 'admin_area_list.csv'))

# --------------------

# ----------------------------------------------
# test map to display regions

# factor the names of regions for accurate color display
coord[ ,region:=as.factor(region)]

# --------------------
# create region name labels
labels = data.table(coordinates(map))
labels = cbind(labels, names)
setnames(labels, c('long', 'lat', 'region', 'id'))

# fix some special character labels
if (country=='Mozambique') labels[grepl('Manhi', region), region:='Manhica']
if (country=='Cameroon') labels[grepl('Extr', region), region:='Extreme-Nord']
# --------------------

# --------------------
# plot the map with a legend and labels for the regions
ggplot(coord, aes(x=long, y=lat, group=group, fill=region)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) +
 theme_void(base_size =16) +
  labs(fill="Administrative Area", 
       title = country)+
  theme(legend.position = 'none')+
  geom_label_repel(data = labels, aes(label = region , 
     x=long, y=lat, group=region), inherit.aes=FALSE, size=3, max.overlaps = 135)
# ----------------------------------------------

#---------------------------------
# check if all of the pepfar regions appear on the map

# upload the data - downloaded from PBI by country
# data represents a full list of pepfar regions or districts
check = data.table(read.csv("C:/Users/ccarelli/Downloads/data.csv"))
setnames(check, 'areas')

# check if the region or district is present
x = !(check$areas %in% areas$areas)
check[x] # lists the regions/districts in the data that do not map

#---------------------------------

# ----------------------------------------------
# create the uganda regional shape file

# # import the list of districts and accompanying regions
# ut = data.table(read.csv(paste0(outDir, 'supporting_files/districts_regions_uganda.csv')))
# setnames(ut, c('region', 'district'))
# 
# # drop the files with only the regions
# ut = ut[district!=""] # 136 districts compared to 135 in shape
# 
# # create a list of unique 
# dists = coord[ ,.(district = unique(region))]
# 
# # check if all districts are included
# # terero is in the data but not the shape
# # terero became a part of Arua District in 2012; no values reported by EGPAF
# ut = ut[ut$district %in% dists$district]
# 
# # merge in the names of regions 
#  # setnames(coord, 'region', 'district')
#  # ut = merge(coord, ut, by = 'district', all.x = T)
# 
# x = unionSpatialPolygons(map, IDs = ut$region)

