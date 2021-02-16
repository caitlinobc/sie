# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/8/21
# Create an accurate list of health facilities
# Using the API call
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(jsonlite)
# --------------------
# Files and directories

# set the working directory to the location of the raw data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/raw_data/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/prepped/'

# --------------------
# pull geographic information for each country

# country ids for all countries
af =  fromJSON(paste0(dir, 'africa_country_ids.json'))
af = data.table(af$children)

# fill in the names of the countries by code
af[id=='ds0ADyc9UCU', country := 'Cote dIvoire']
af[id=='V0qMZH29CtN', country:= 'Eswatini']
af[id=='bQQJe0cC1eD', country:='Cameroon']


# api call link (store all files): 
# 'https://www.datim.org/api/organisationUnits/bQQJe0cC1eD.json?includeDescendants=true&paging=false&fields=id,name,parent,level,geometry'

# --------------------

# ----------------------------------------------
# JSON facilities and locations

# --------------------
country = 'cdi'

ll = fromJSON(paste0(dir, country, '_lat_long.json'))

# --------------------
# import and format the data 

# grab each row in the list and bind into a data table
level = ll$organisationUnits[1]
facility = ll$organisationUnits[2]
id = ll$organisationUnits[3]

# grab the 'parent' (next org unit up) for each admin unit
if (country %in% c('cdi', 'cmr')) parent_id = ll$organisationUnits[5]
if (country=='esw') parent_id = ll$organisationUnits[4]

# bind and create a data table
dt = data.table(cbind(facility, level, id, parent_id))
dt[ , country:=country]
# --------------------

# ----------------------------------------------
# Name the admin levels: ESWATINI

# eswatini country id: 'V0qMZH29CtN'
dt[level==3, type:='country']

# level 4 in eswatini is region - 4 regions and one military zone
dt[level==4 & name!='_Military Eswatini', type:='region']
dt[name=='_Military Eswatini', type:='military']

# level 5 in eswatini is district - 60 districts
dt[level==5, type:='district']

# level 6 in eswatini is facility - 466 facilities/sites
dt[level==6, type:='facility']








# --------------------
# add districts to facilities using parenti Id for the health facility
dist = dt[type=='district', .(district = name, parent = id, district_parent = parent)]
dt = merge(dt, dist, by = 'parent', all.x = TRUE)

# --------------------
# add regions to facilities using district

reg = dt[type=='region', .(region = name, district_parent = id)]
dt = merge(dt, reg, by = 'district_parent', all.x = TRUE)

# --------------------
# drop unnecessary variables and format

dt = dt[ ,.(name, level, type, id, parent, district, region, country)]

# --------------------
# add coordinates

# create a data table of the coordinates
geom_type = ll$organisationUnits$geometry[1]
coords = ll$organisationUnits$geometry[2]
geom = data.table(cbind(geom_type, coords))

# fix the structure of the table and remove polygons
geom[ , coord:=as.character(coordinates)]
geom[type=='Polygon', coord:=NA]
geom[ , c('type', 'coordinates'):=NULL]
geom[coord=='NULL', coord:=NA]

# convert to lat long
geom[ , coord:=gsub("c\\(", "", coord)]
geom[ , coord:=gsub("\\)", "", coord)]
geom[ , lat:=sapply(strsplit(coord,","), "[", 1)]
geom[ , long:=sapply(strsplit(coord,","), "[", 2)]
geom[ , coord:=NULL]

# --------------------
# bind in latitude and longitude of health facilities

dt = cbind(dt, geom)

# --------------------
# export the file with all administrative units

saveRDS(dt, paste0(outDir, 'full_admin_lists/esw_lat_long_full.rds'))

# --------------------
# export the file with health facilities only 

dt_fac = dt[type=='facility']
saveRDS(dt_fac, paste0(outDir, 'health_facility_lists/esw_lat_long.rds'))

# ----------------------------------------------










