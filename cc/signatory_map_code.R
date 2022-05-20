# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 5/20/2022
# Prep data and create maps
# Use addresses to find geolocations
# Map using county and district level shape files
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
library(tools)
library(ggmap)
library(sf)
# ------------------------

# ----------------------------------------------
# IMPORT THE DATA AND SET UP COLORS
# ----------------------------------------------

# ------------------------
# set working directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/cc/'

# ------------------------
# read in the data 
dt = readRDS(paste0(dir, 'cc_signers_prepped.rds'))

# drop coordinates that are not in new york state
dt = dt[!(lon < -80) & !(lat < 40) & !(-72 < lon)]
# ------------------------
# create custom color palettes

# 21st and not the 21st - green/grey
pal21 = c('#addd8e', '#f0f0f0')

funs = brewer.pal(9, 'RdYlBu')
blues = brewer.pal(9, 'Blues')
dists = c(funs, blues)


# ----------------------------------------------
# MAKE SOME MAPS
# ----------------------------------------------

# ------------------------
# COUNTY MAP
# ------------------------
# read in the county shape file
# source: https://gis.ny.gov/gisdata/inventories/details.cfm?DSID=927
county_map = st_read(paste0(dir, '/nys_civil_counties/Counties.shp'))

# convert to standard lat/long coordinates
county_map = st_transform(county_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")
coord2 = data.table(st_coordinates(county_map))
coord2[ , L2:=NULL] # drop this value - not sure what it represents
setnames(coord2, c('long', 'lat', 'state', 'id'))

# add in the names of the counties
names = data.table(cbind(county = c(county_map$NAME), id = seq(1:62)))
names[ ,id:=as.numeric(id)]

# merge in the names
coord2 = merge(coord2, names, by = 'id')

# create a variable identifying only counties in the 21st
counties = c('Clinton', 'Essex', 'Franklin', 'Fulton', 'Hamilton', 'Jefferson',
             'Lewis', 'St Lawrence', 'Warren', 'Washington', 'Herkimer', 'Saratoga' )
coord2[county %in% counties, county_name:=county]
# ------------------------

# ------------------------
# Locations of signatiories by county
ggplot(coord2, aes(x=long, y=lat, group = id, fill=county_name)) + 
  geom_polygon() + 
  geom_path(size=0.01) +
  scale_fill_discrete(type = c(brewer.pal(9, 'Blues'), '#41b6c4',
                               '#1d91c0', '#ccebc5' ), na.value = '#f0f0f0') +
  geom_point(data = dt, aes(x = lon, y = lat, group = volunteer), 
             size = 2, colour = '#081d58', alpha= 0.2, pch = 21,
             inherit.aes = F)+
  theme_void()+
  labs(title = "Distribution of Signatories by County",
       fill = "County")

# ------------------------

# ------------------------
# count of signatories by county

# count up the number of signatures
signats = dt[ , .(value= length(unique(name))), by = county]

# ------------------------
# label each county
centers = data.table(st_centroid(county_map))
centers = centers[ ,.(county = NAME, geometry)]
centers[ , long:=trimws(unlist(lapply(str_split(geometry, ','), '[', 1)))]
centers[ , lat:=trimws(unlist(lapply(str_split(geometry, ','), '[', 2)))]
centers[ , long:=gsub("c\\(", "", long)]
centers[ , lat:=gsub("\\)", "", lat)]
centers[ , geometry:=NULL]
centers[ , long:=as.numeric(long)]
centers[ , lat:=as.numeric(lat)]

# merge the data with the centroids
centers = merge(centers, signats, by = 'county', all.x=T)
centers[!is.na(value) , label:=paste0(county, ': ', value)]

# merge the data with the map
coord3 = merge(coord2, signats, by = 'county', all.x = TRUE)
total = sum(signats$value)

# total signatures by county
ggplot(coord3, aes(x=long, y=lat, group = id, fill=value)) + 
  geom_polygon() + 
  geom_path(size=0.01) +
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues') , na.value = '#f0f0f0') +
  theme_void()+
  labs(title = "Number of Signatures by County",
       subtitle = paste0("Currently entered into database: ", total),
       fill = "Signatures") +
  geom_label_repel(data = centers, aes(x = long, y = lat,
                              group = county, label = label), inherit.aes=FALSE)


# ------------------------
# CONGRESSIONAL DISTRICT MAP
# ------------------------
# read in the March 2022 congressional district shape file
# source: https://gis.ny.gov/gisdata/inventories/details.cfm?DSID=1360
dist_map = st_read(paste0(dir, '/nys_cong_dist/NYS-Congressional-Districts.shp'))

# convert to standard lat/long coordinates
dist_map = st_transform(dist_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")
coord = data.table(st_coordinates(dist_map))
coord[ , L2:=NULL] # drop this value - not sure what it represents
setnames(coord, c('long', 'lat', 'state', 'district'))

# add a 21st district binary
coord[district==21, tfirst:='21st District']
coord[district!=21, tfirst:='Other Congressional District']
coord[ , tfirst:=as.factor(tfirst)]

# ------------------------
# Locations of signatiories by March 2022 Congressional District
ggplot(coord, aes(x=long, y=lat, group = district, fill=tfirst)) + 
  geom_polygon() + 
  geom_path(size=0.01) +
  scale_fill_manual(values = pal21, na.value = '#f0f0f0') +
  geom_point(data = dt, aes(x = lon, y = lat, group = volunteer), 
       size = 2, colour = '#feb24c', alpha= 0.2, inherit.aes = F)+
  theme_void()+
  labs(title = "Signatories by Congressional District",
       subtitle = "Reflects March 2022 Congressional Districts",
       fill = "")

# ------------------------




# ------------------------
# accurate geographic map of location s
nys = get_googlemap(center = c(lon = -74.2179, lat = 43.2994), zoom = 6, 
                    maptype = "roadmap")

