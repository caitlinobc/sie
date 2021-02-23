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
library(openxlsx)
# --------------------
# Files and directories

# set the working directory to the location of the raw data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/raw_data/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/prepped/'

# set the main directory where all files are located
main_dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/'

# --------------------
# load the function that fixes diacritical marks
source('C:/Users/ccarelli/Documents/GitHub/sie/all/fix_diacritics_function.r')

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
# JSON facilities and locations - LOOP

#
# --------------------
# loop through files, import, and format

# create a list of country coces
countries = c('cmr', 'cdi', 'esw')

# set the index
i = 1

# run the loop!
for (c in countries) {

# set the country name to import correctly
country = c

# import the data 
ll = fromJSON(paste0(dir, country, '_lat_long.json'))

# --------------------
# # NAME THE ADMIN LEVELS IN THE DATA SETS

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
# country and higher level region are the same for all countries
dt[level==3, type:='country']
dt[level==4,type:='region']

# --------------------
# COUNTRY SPECIfIC LEVELS

# countries have different levels for distinct admin units
if (country %in% c('cmr', 'esw')) {
    dt[level==5, type:='district']
    dt[level==6, type:='facility']
    dt[country=='cmr' & level==7, type:='facility']
} else { # cdi only right now
  dt[level==5, type:='district']
  dt[level==6, type:='sub-district']
  dt[level==7, type:='facility']
  
}

# change the "military" region "military" type
dt[grepl('Military', name), type:='military']

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
geom[ , long:=sapply(strsplit(coord,","), "[", 1)]
geom[ , lat:=sapply(strsplit(coord,","), "[", 2)]
geom[ , coord:=NULL]

# --------------------
# bind the coordinates for the facilities to the data

# check that the lengths are the same
if (nrow(geom) != nrow(dt)) print("Your lat long has a problem!")

# bind in the facility coordinates
dt = cbind(dt, geom)

# --------------------
# rbind all of the country-specific data sets together
# creates a complete data set of pepfar org units
if (i == 1) fd = dt
if (1 < i) fd = rbind(fd, dt)

i = i+1 # reset the index each time the loops runs
}

# --------------------

# ----------------------------------------------
# format the data wide for ease of use (puts data at the facility level

# --------------------
# add district names to associated facilities using the facility parent id

# for countries with sub-districts, merge in sub-district names
# these include: cdi
sub_dist = fd[type=='sub-district', .(sub_dist = name, parent = id, 
    sub_dist_parent = parent)]
fd = merge(fd, sub_dist, by = 'parent', all.x = TRUE)

# facilities with sub districts have sd parents ids
# replace with district parent ids (the parent ids of the sds)
fd[!is.na(sub_dist), parent:=sub_dist_parent] 
fd[ , sub_dist_parent:=NULL]

# add the names of the districts
dist = fd[type=='district', .(district = name, parent = id, 
          district_parent = parent)]
fd = merge(fd, dist, by = 'parent', all.x = TRUE)

# --------------------
# add regions to facilities and districts using district parents

# districts only have parents, not district parents (add)
fd[type=='district', district_parent:=parent]

# merge in theregions 
reg = fd[type=='region', .(region = name, district_parent = id)]
fd = merge(fd, reg, by = 'district_parent', all.x = TRUE)

# --------------------
# drop unnecessary variables and format the data set
# drops district parents 

fd = fd[ ,.(name, level, type, id, parent, sub_dist, 
            district, region, country, lat, long )]
fd[ , country:=toupper(country)]

# ----------------------------------------------
# create a binary for egpaf health facility

# list the facilities specific to egpaf 
e_dt = data.table(read.csv(paste0(main_dir, 'pepfar_org_units/egpaf_facilities_list.csv')))
setnames(e_dt, c('country','org_unit_id'), c('country_name', 'id'))
e_dt[ , hts_tst:=NULL]
e_dt[ , egpaf:=TRUE]

# merge in the ids of the egpaf facilities; binary for 'is egpaf ip?'
fd = merge(fd, e_dt, all.x=TRUE)
fd[is.na(egpaf), egpaf:=FALSE]

# check that the facilities matched in the correct countries
fd[egpaf==TRUE, unique(country), by = country_name]
fd[ , country_name:=NULL] # drop the country name check

# --------------------
# export the file with all administrative units

saveRDS(fd, paste0(outDir, 'all_datim_org_units.rds'))

# --------------------
# export the file with health facilities only 

fd_fac = fd[type=='facility']
saveRDS(fd_fac, paste0(outDir, 'datim_health_facilities.rds'))

# ----------------------------------------------

# ----------------------------------------------
# for each country, export a list of health facilities for use in PBI

# fix the diacritical marks for facility names to export as csv
fd_fac[ , name:=fix_diacritics(name)]

# print a csv for each country with a list of facility names
for (c in countries){
  country_name = toupper(c)
  print_dt = fd_fac[country==country_name]
  
  # add country names
  print_dt[country=='CMR', country:='Cameroon']
  print_dt[country=='ESW', country:='Eswatini']
  print_dt[country=='CDI', country:='Cote d\'Ivoire']
  
  # change egpaf to y/n
  print_dt[egpaf==TRUE, egpaf_facility:='Yes']
  print_dt[egpaf==FALSE, egpaf_facility:='No']
  
  # save a file for each country
  write.csv(print_dt, paste0(outDir, c, '_health_facilities.csv'))
}

# ----------------------------------------------





