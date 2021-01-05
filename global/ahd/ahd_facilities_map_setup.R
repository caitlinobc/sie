# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/4/21
# Prep data to map health facilities in the AHD Study
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
library(gridExtra)
library(readxl)
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/AHD/'

# set the working directory for the shape file
setwd('C:/Users/ccarelli/Documents/data/prepped/')

# set output director
OutDir = paste0(dir, 'data/')

# ---------------------------------------
# import data and subset to malawi and tanzania

dt = data.table(read_xlsx(paste0(dir, 'data/global_facility_inventory.xlsx')))

# rename variables to usable format
setnames(dt, new = c('country','admin1', 'facility', 'facility_type', 
                     'owner', 'lat', 'long', 'll_source'))

# subset to the relevant countries
dt = dt[country=='Tanzania' | country=='Malawi']

# --------------------
# read in the shape file 

shape = readRDS('shape_files_all_countries.rds')
shape = shape[country=='Malawi' |country=='Tanzania']

# --------------------
# import a list of the facilities included in the study and subset
facs_dt = data.table(read.csv(paste0(dir, 'data/facility_list_ahd.csv')))

# change abbreviations for the merge
facs_dt[, facility:=(gsub('RRH', 'Regional Referral Hospital', facility))]

# merge the facilities with their locations
fac = facs_dt$facility

# change names in the list of locations to match names in the data
# originally - 19 facilities are in the data, 11 are missing
dt[facility=='Nkinga Regional Referral Hospital', facility:='Nkinga Mission Hospital']
dt[facility=='Singida Regional Referral Hospital', facility:='Singida Regional Hospital']
dt[facility=='Mt. Meru Regional Referral Hospital', facility:='MT Meru']

dt[facility=='Bvumbwe Research Health Centre', facility:='Bvumbwe Health Centre']
dt[facility=='Pirimiti Hospital', facility:='Pirimiti Rural Hospital']
dt[facility=='Arusha Lutheran Centre Referral Hospital', facility:='ALMC']

# after changes only 5 facilities are missing (of 30)
fac[!(fac %in% dt$facility)] 

# --------------------
# subset the coordinates file and merge
dt = dt[facility %in% fac]

# drop excess information from sheets and merge
facs_dt[ , c('country'):=NULL]
dt[ , c('facility_type'):=NULL]

# merge the data 
dt = merge(dt, facs_dt, all.y=TRUE, by = 'facility')
         
# rename and organize columns
dt = dt[ ,. (facility, facility_type, level, owner, lat, long, 
        region = admin1, district, country, ll_source)] 

# --------------------
# fill in the coordinates for the 5 missing facilities

# coordinates easily found on google maps
bil = c('Bilila Health Centre', -14.822695902838463, 34.85192442399777, 'Malawi')
kcmc = c('KCMC', -3.319598579977617, 37.327466601969284, 'Tanzania')
kibong = c('Kibong\'oto Hospital', -3.196300775208977, 37.10558461858568, 'Tanzania')
lim = c('Limbe Health Centre', -15.793034358214141, 35.04985481655105, 'Malawi')

# kapili hospital is missing a location
# this represents the district hospital coordinates in Mchinji (where it is located)
kap = c('Kapili Hospital',-13.8027173758062, 32.88847636531416, 'Malawi')

# create a data table of the data for the missing facilities
missing_facs = data.table(rbind(bil, kcmc, kap, kibong, lim))
setnames(missing_facs, c('facility', 'lat1', 'long1', 'country1'))

missing_facs[ ,ll_source1:='Google Earth']
missing_facs[ , lat1:=as.numeric(lat1)] # convert lat/long to numeric for merge
missing_facs[ , long1:=as.numeric(long1)]

# merge in the additional data
dt = merge(dt, missing_facs, by = 'facility', all = TRUE)

# fill in missing variables - missing owner means not in original locations
dt[is.na(owner), lat:=lat1]
dt[is.na(owner), long:=long1]
dt[is.na(owner), ll_source:=ll_source1]
dt[is.na(owner), country:=country1]
dt[ , c('lat1', 'long1', 'll_source1', 'country1'):=NULL]
# --------------------

# ---------------------------------------
# map the points as a test

# set map parameters
c = 'Tanzania'
country_name = as.character(c)

# sample map - subset by country, sites
ggplot(shape[country==c], aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#f7fbff')+
  geom_point(data = dt[country==c], aes(x = long, y =lat, group = country), 
             size = 2, colour = '#feb24c', alpha= 0.5)+
  theme_void(base_size = 14) +
  labs(title="Health facilities included in the AHD Study",
       subtitle = country_name) +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6),
        text=element_text(size=16)) 

# ---------------------------------------

# --------------------
# export the file with the locations 

saveRDS(dt, paste0(OutDir, 'prepped_geolocations_ahd.rds'))

# --------------------

# ---------------------------------------


