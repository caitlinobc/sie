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
# --------------------

# ----------------------------------------------
# set working directories 

dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/site_count_check/'

# ----------------------------------------------
# read in the files to check

# read in the list of sites that reported on key indicators
dt = data.table(fread(paste0(dir, 'cam_sites_any_ki.csv')))
setnames(dt, c('site', 'variable', 'value'))


# reshape the data wide for comparison 
dt = dcast(dt, site~variable)

# import the list of sites that reported on testing (official list)
lt =  data.table(fread(paste0(dir, 'cam_sites_hts_tst.csv')))
setnames(lt, c('site', 'hts_tst', 'fq'))
lt[ , fq:=NULL]

# check for sites with a similar name
dt[grepl("leon", tolower(site))]

# ----------------------------------------------
# EXAMINE THE SITES 

# 72 key indicator sites are in the key indicator data 
# one is not - which site is it?
dt[!(site %in% lt$site)]







