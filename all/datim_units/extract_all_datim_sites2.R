# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/22/21
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
library(RJSONIO)
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
# load the function that fixes diacritical marks
source('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Documents/GitHub/sie/all/fix_diacritics_function.R')

# --------------------
# set the country argument

country = 'cameroon'

# ---------------------------------------------
# source link for all files 
# download the api from the link above and process

# api call link (store all files): 
# 'https://www.datim.org/api/organisationUnits/bQQJe0cC1eD.json?includeDescendants=true&paging=false&fields=id,name,parent,level,geometry'

# ---------------------------------------------
# pull the organisation unit meta data for every country in datim

# -------------------

# load a list of every unit in datim
dt = RJSONIO::fromJSON(paste0(dir, country, '.json'))

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

# ---------------------------------------------
# match up the parent ids with the sites







