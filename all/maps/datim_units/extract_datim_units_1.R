# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/8/2023
# Extract the DATIM units from all EGPAF countries
# Using the API call
# currently: not an automatic download
# visit the link with the appropriate id to download
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
library(RJSONIO)
library(RCurl)
library(rjson)
# --------------------

# --------------------
# Files and directories

# set the working directory to the location of the raw data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/datim_units/raw/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/datim_units/prepped/'

# set the main directory where all files are located
main_dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/'

# --------------------

# ---------------------------------------------
# EXTRACT THE COUNTRIES
# ---------------------------------------------

# --------------------
# source link for all files 
# download the api from the link above and process

# api call link (store all files): 
# 'https://www.datim.org/api/organisationUnits/bQQJe0cC1eD.json?includeDescendants=true&paging=false&fields=id,name,parent,level,geometry'

# country ids for all african countries
# af =  fromJSON(paste0(main_dir, 'all/datim_units/africa_country_ids.json'))
# af = data.table(af$children)
# setnames(af, 'id')
# --------------------

# --------------------
# create a list of country ids for egpaf countries
countries = data.table(c('cameroon', 'cdi', 'drc', 'eswatini', 'kenya', 
                       'lesotho', 'malawi', 'moz', 'tanzania', 'uganda'))

# create a list of their associated org unit ids              
orgUnitID = c('bQQJe0cC1eD', 'ds0ADyc9UCU', 'ANN4YCOufcP', 'V0qMZH29CtN', 'HfVjCurKxh2',
              'qllxzIjjurr', 'lZsCb6y0KDX', 'h11OyvlPxpJ', 'mdXu6iCbn2G', 'FETQ6OmnsKB')

# bind the names of the countries to their associated ids
egpaf_country_list = cbind(countries, orgUnitID)
setnames(egpaf_country_list, c('country', 'orgUnitID'))

# save the africa ids to prepped data 
saveRDS(egpaf_country_list, paste0(outDir, 'egpaf_country_ids.rds'))
# --------------------

# --------------------
# country organizational unit ids

# 1: qllxzIjjurr - lesotho 
# 2: cDGPF739ZZr - south africa
# 3: mdXu6iCbn2G - tanzania
# 4: a71G4Gtcttv - zimbabwe
# 5: l1KFEXKI4Dg - botswana
# 6: FFVkaV9Zk1S - namibia
# 7: f5RoebaDLMx - zambia
# 8: ds0ADyc9UCU - cdi
# 9: lZsCb6y0KDX - malawi
# 10: IH1kchw86uA - ethiopia
# 11: FETQ6OmnsKB - uganda
# 12: PqlFzhuPcF1 - nigeria
# 13: HfVjCurKxh2 - kenya
# 14: G0BT4KrJouu - west africa region
# 15: WLG0z5NxQs8 - south sudan
# 16: ANN4YCOufcP - drc
# 17: h11OyvlPxpJ - mozambique
# 18: V0qMZH29CtN - eswatini
# 19: XtxUYCsDWrR - rwanda
# 20: Qh4XMQJhbk8 - burundi
# 21: bQQJe0cC1eD - cameroon
# 22: XOivy2uDpMF - angola
# --------------------

# ---------------------------------------------
# EXTRACT THE ASSOCIATED SITES
# ---------------------------------------------

# pull the organisation unit meta data for every country in datim

# set the index to 1
dex = 1

# test line for 
countries = countries[[1]][[1]]

# loop through each country's file for the total units
for (x in countries) {
  
  # -------------------
  
  # test line for cameroon only - ensure the loop is working
  
  # load a list of every unit in datim
  json_file = paste0(dir, x, '.json')
  dt = fromJSON(paste(readLines(json_file), collapse=""))

  # --------------------
  # loop to get the entire list of org units for the country
  
  # loop through each unit
  i = 1
  for (o in seq(1:length(dt$organisationUnits))) {
    
    org = dt$organisationUnits[i][[1]]
    lt = c(level = org$level, name = org$name, id = org$id, parent = org$parent, 
           geo_type = org$geometry$type, coord = org$geometry$coordinates)
    
    if (i == 1) org_list = lt
    if (1 < i) org_list = rbind(org_list, lt)
    i = i+1}
  
  # --------------------
  # convert to a data table for ease of use
  org_list = data.table(org_list)  
  
  # drop the multi-polygons and missing coordinates from the geo data
  org_list[geo_type=='Point', geo_type:='point']
  org_list[geo_type!='point', coord1:=NA]
  org_list[geo_type!='point', coord2:=NA]
  org_list[geo_type!='point', geo_type:=NA]
  
  # add the country name to the data set
  x_name = as.character(x)
  org_list[ , country:=as.character(x_name)]
  
  # reset the names of the files
  setnames(org_list, c('level', 'org_unit', 'org_unit_id', 'parent_id',
                       'geotype', 'longitude', 'latitude', 'country'))
  
  # --------------------
  # combine all countries 
  if(dex==1) full_data = org_list
  if(1 < dex) full_data = rbind(full_data, org_list)
  dex = dex+1
  
  # --------------------
  
}

# ---------------------------------------------
# FORMAT THE LIST OF ALL SITES
# ---------------------------------------------

# convert the full file to a data table
# some variables may be exported as lists - unlist the variables
full_data[ , level:=as.numeric(level)] # running these functions unlists the vars
full_data[ , org_unit:=as.character(org_unit)]
full_data[ , org_unit_id:=as.character(org_unit_id)]
full_data[ , parent_id:=as.character(parent_id)]
full_data[ , geotype:=as.character(geotype)]
full_data[ , latitude:=as.numeric(latitude)]
full_data[ , longitude:=as.numeric(longitude)]

# save the file
saveRDS(full_data, paste0(outDir, 'all_datim_units.rds'))

# delete unnecessary files from the environment
remove(dt, org, org_list, lt)

# ---------------------------------------------
# PREP THE DATA 
# ---------------------------------------------

# --------------------
# rename the data set for ease of coding
dt = copy(full_data)
remove(full_data)

# country and higher level region are the same for all countries
dt[level==3, type:='country']
dt[level==4, type:='region']
dt[level==5, type:='district']

# level 7 is always site
dt[level==7, type:='site']

# some countries have site at level 6, some at 7
level_6_countries = c('cameroon')
dt[country %in% level_6_countries & level==6, type:='site']
# --------------------

# --------------------
# merge in the EGPAF sites 
# this relies on a proxy measure - any site reporting HTS 

# read in the egpaf sites using the proxy and format
egpaf = data.table(read.csv(paste0(dir, 'fy23_egpaf_sites.csv')))
egpaf[ , HTS_TST:=NULL]
setnames(egpaf, c('level6', 'level7', 'org_unit_id'))

# determine if the sites are at level 7 or level 6
if (all(is.na(egpaf$level7))==TRUE) {
  egpaf[, level7:=NULL]
} else { egpaf[ , level6:=level7]}
setnames(egpaf, 'level6','egpaf_site')

# merge in the EGPAF information based on org_unit_id
dt = merge(dt, egpaf, by = 'org_unit_id', all = T)

# check that the site names are the same in DATIM/PBI
dt[!is.na(egpaf_site), .(org_unit, egpaf_site)]
dt[!is.na(egpaf_site) & org_unit!=egpaf_site] #in cameroon, all ok (just capitalization)

# fix CSIU Number One in Cameroon
dt[org_unit_id=='nXrhnc7vTDj', org_unit:='CSIU Number One']

# add a logical for EGPAF site
dt[!is.na(egpaf_site), egpaf:=TRUE]
dt[is.na(egpaf), egpaf:=FALSE]

# --------------------

# ---------------------------------------------
# ADD THE METADATA - REGIONS AND DISTRICTS FOR SITES
# ---------------------------------------------

# check: should all be regions - parent is country
dt[parent_id %in% orgUnitID, unique(type)] 

# create data tables of just the regions to merge in
regions = dt[type=='region', .(region = org_unit, parent_id = org_unit_id)]

# merge in the regions for the districts
# the region 'Military Cameroon" does not match in
dt = merge(dt, regions, by = 'parent_id', all.x = T)

# create data tables of just the districts to merge in
# this is iterative, now includes the regions associated with districts
districts = dt[type=='district', .(district = org_unit, parent_id = org_unit_id,
                                   region)]

# merge in the districts for the sites
# the regions will come along with the districts
dt = merge(dt, districts, by = 'parent_id', all = T)

# eliminate the second regions - no idea why this happens
dt[region.x!=region.y] # check that the regions are all correct (should be 0)
dt[is.na(region.x), region.x:=region.y]
setnames(dt, 'region.x', 'region')
dt[ , region.y:=NULL]

# for maps with subdistricts, include subdistricts
# iterative process - subdistricts will bring districts and regions

# ---------------------------------------------
# EXPORT THE DATA
# ---------------------------------------------

# --------------------
# format for an intuitive order

dt = dt[ ,.(org_unit, org_unit_id, type, level, egpaf, egpaf_site, 
       country, region, district, latitude, longitude,
       parent_id)]
# --------------------

# --------------------
# export an RDS
saveRDS(dt, paste0(outDir, 'all_datim_units.rds'))
# --------------------

# --------------------
# Create fomatted names for CSVs, PBI 

setnames(dt, c('Org Unit', 'Org Unit ID', 'Unit Type', 'Level', 
               'EGPAF Site', 'EGPAF Site Name', 'Country', 
               'Region', 'District', 'Latitude', 'Longitude', 
               'Parent ID'))

write.csv(dt, paste0(outDir, 'all_datim_units.csv'))
# --------------------



