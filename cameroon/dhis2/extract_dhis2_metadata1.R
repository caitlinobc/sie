# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/19/2022
# Cameroon API Model - DHIS2 aggregate data
# Pull the meta data, including health facilities
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(readxl)
library(xlsx)
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(RJSONIO)
# --------------------

# --------------------
# Files and directories

# set the working directory to the location of the raw data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/cameroon/dhis2/'
setwd(dir)

# set the output directory
outDir = paste0(dir, 'prepped/')

# set the main directory where all files are located
main_dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/'

# --------------------
# load the function that fixes diacritical marks
source('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Documents/GitHub/sie/all/fix_diacritics_function.R')

# --------------------
# list of API links

# table of contents - all API links
'https://cmrdhis.eastus.cloudapp.azure.com/pldcare/api/resources'

# full list of org units
org_url = "https://Caitlin:User001*@cmrdhis.eastus.cloudapp.azure.com/pldcare/api/organisationUnits.json?includeDescendants=true&paging=false&fields=id,name,parent,level,geometry"


# -----------------------------------------
# DOWNLOAD THE FILES
# -----------------------------------------

# list of org units
download.file(org_url, 
        destfile = paste0(dir, 'raw/organisationUnits.json'))





# -----------------------------------------
# PARSE THE PAGES
# -----------------------------------------

# --------------------
# extract a list of all org units and health facilities

# load the json file
orgUnitList = jsonlite::fromJSON(paste0(dir, 'raw/organisationUnits.json'))

# unlist the list/parse the file
org_units = data.table(cbind(orgUnitID = orgUnitList$organisationUnits$id,
                 orgUnit = orgUnitList$organisationUnits$name,
                 level = orgUnitList$organisationUnits$level,
                 parentID = orgUnitList$organisationUnits$parent$id))

# four org units are duplicated with no associated name
org_units = org_units[!(orgUnit=='.' | orgUnit=='-')]

# export all org units as a csv
write.csv(org_units, paste0(outDir, 'all_org_units.csv'))
saveRDS(org_units, paste0(outDir, 'all_org_units.rds'))

# create a list of just health facilities
units = org_units[level==6]
parents = org_units[level!=6]
setnames(parents )



merge(units, parents)












url = "https://cmrdhis.eastus.cloudapp.azure.com/pldcare/api/organisationUnits.xml"
userID = "caitlin"
password = "user001*"

parse_page(url, userID, password)



# ---------------------------------------------
# pull the organisation unit meta data for every country in datim

# set the index to 1
dex = 1

# loop through each country's file for the total units
for (c in countries) {
  
  # -------------------
  
  # load a list of every unit in datim
  dt = RJSONIO::fromJSON(paste0(dir, c, '.json'))
  
  # --------------------
  # loop to get the entire list of org units for the country
  
  # loop through each unit, leaving off lat/long
  i = 1
  for (o in seq(1:length(dt$organisationUnits))) {
    
    org = dt$organisationUnits[i][[1]]
    lt = c(level = org$level, name = org$name, id = org$id, parent = org$parent)
    
    if (i == 1) org_list = lt
    if (1 < i) org_list = rbind(org_list, lt)
    i = i +1}
  
  # --------------------
  # convert to a data table for ease of use
  org_list = data.table(org_list)  
  org_list[ , country:=as.character(c)]
  
  # --------------------
  # combine all countries 
  if(dex==1) full_data = org_list
  if(1 < dex) full_data = rbind(full_data, org_list)
  dex = dex+1
  
  # --------------------
  
}
