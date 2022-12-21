# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/19/2022
# Cameroon API Model - DHIS2 aggregate data
# Download all of the meta data using an API call
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
outDir = paste0(dir, 'raw/')

# --------------------
# load the function that fixes diacritical marks
source('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Documents/GitHub/sie/all/fix_diacritics_function.R')

# --------------------
# list of API links

# table of contents - all API links
'https://cmrdhis.eastus.cloudapp.azure.com/pldcare/api/resources'

# specific org unit example 
'https://cmrdhis.eastus.cloudapp.azure.com/pldcare/api/organisationUnits/ME6mMdBOq6r'

# for an example of all fields
'https://cmrdhis.eastus.cloudapp.azure.com/pldcare/api/dataSets.json?fields=:all'

# --------------------
# links for downloading data - includes all essential attributes

# full list of org units - does not include ancestors
org_url = "https://Caitlin:User001*@cmrdhis.eastus.cloudapp.azure.com/pldcare/api/organisationUnits.json?includeDescendants=true&paging=false&fields=id,name,parent,level,geometry"

sets_url = 'https://Caitlin:User001*@cmrdhis.eastus.cloudapp.azure.com/pldcare/api/dataSets.json?fields=:all'

cat_combo_url = 'https://cmrdhis.eastus.cloudapp.azure.com/pldcare/api/categoryCombos'# edit this

# -----------------------------------------
# DOWNLOAD THE FILES
# -----------------------------------------

# download the list of organisational units
download.file(org_url, 
              destfile = paste0(dir, 'raw/organisationUnits.json'))

# download the list of data sets
download.file(sets_url, 
              destfile = paste0(dir, 'raw/data_sets.json'))


# attributes

# category combos

# category options

# indicator group sets/indicators

# categories

# data elements/program data elements



# test code to see if the download worked
jsonlite::fromJSON(paste0(dir, 'raw/data_sets.json'))







