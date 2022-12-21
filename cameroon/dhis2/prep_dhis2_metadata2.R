# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/19/2022
# Cameroon API Model - DHIS2 aggregate data
# Cleans and preps the meta data for analysis
# Sources files downloaded in extract_dhis2_metadata1.R
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


# -----------------------------------------
# PARSE THE PAGES
# -----------------------------------------

# --------------------
# extract a list of all org units and health facilities

# load the json file
orgUnitList = jsonlite::fromJSON(paste0(dir, 'raw/organisationUnits2.json'))

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










