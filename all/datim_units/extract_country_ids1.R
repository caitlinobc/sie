# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/22/21
# Extract the IDs of African countries
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
# source link for all files 
# download the api from the link above and process

# api call link (store all files): 
# 'https://www.datim.org/api/organisationUnits/bQQJe0cC1eD.json?includeDescendants=true&paging=false&fields=id,name,parent,level,geometry'

# country ids for all african countries
# af =  fromJSON(paste0(main_dir, 'all/datim_units/africa_country_ids.json'))
# af = data.table(af$children)
# setnames(af, 'id')

# create a list of country ids 
country = data.table(c('cameroon', 'cdi', 'drc', 'eswatini', 'kenya', 
                       'lesotho', 'malawi', 'moz', 'tanzania', 'uganda'))

# create a list of their associated org unit ids              
orgUnitID = c('bQQJe0cC1eD', 'ds0ADyc9UCU', 'ANN4YCOufcP', 'V0qMZH29CtN', 'HfVjCurKxh2',
              'qllxzIjjurr', 'lZsCb6y0KDX', 'h11OyvlPxpJ', 'mdXu6iCbn2G', 'FETQ6OmnsKB')

# bind the names of the countries to their associated ids
clist = cbind(country, orgUnitID)
setnames(clist, c('country', 'orgUnitID'))

# save the africa ids to prepped data 
saveRDS(paste0(outDir, 'african_country_ids.rds'))

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