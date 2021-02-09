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

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/facilities/'

# --------------------
# loop through and pull geographic information

# country ids for all countries
af =  fromJSON(paste0(dir, 'africa.json'))
af = data.table(af$children)

# fill in the names of the countries by code
af[id=='ds0ADyc9UCU', country := 'Cote dIvoire']
af[id=='V0qMZH29CtN', country:= 'Eswatini']
af[id=='bQQJe0cC1eD', country:='Cameroon']


# api call link (store all files): 
# 'https://www.datim.org/api/organisationUnits/bQQJe0cC1eD.json?includeDescendants=true&paging=false&fields=id,name,parent,level,geometry'

# --------------------
# read in the json file

# ----------------------------------------------
# JSON facilities and locations

# --------------------
# ESWATINI

# import the file
ll = fromJSON(paste0(dir, 'esw_lat_long.json'))

# grab each row in the list and bind into a data table
level = ll$organisationUnits[1]
facility = ll$organisationUnits[2]
id = ll$organisationUnits[3]
parent_id = ll$organisationUnits[4]
  
dt = data.table(cbind(facility, level, id, parent_id))
dt[ , country:='Eswatini']

# --------------------
# name the admin levels for eswatini

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



