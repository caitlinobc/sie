# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/9/2023
# Merge in Cameroon partner information

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

# --------------------
# Files and directories

# set the working directory to the location of the raw data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/datim_units/raw/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/datim_units/prepped/'

# --------------------

# --------------------
# IMPORT DATA 

# read in the pepfar sites received from egpaf-cameroon
cam = data.table(read_excel(paste0(dir, 'cameroon_pepfar_sites.xlsx')))
setnames (cam, c('no', 'region', 'district', 'org_unit', 'ip', 'org_unit_id'))
cam[org_unit_id=='NA', org_unit_id:=NA]
cam[ , no:=NULL]

# read in the list of all DATIM units and subset to cameroon
dt = readRDS(paste0(outDir, 'all_datim_units.rds'))
dt = dt[country=='cameroon']
# --------------------

# --------------------
# descriptive statistics per ip
cam[ ,length(unique(org_unit)), by = ip]
cam[ ,length(unique(org_unit)), by = .(region, ip)]

# --------------------

# --------------------
# check for duplicate org unit ids - should be an empty data table
cam[ , count:=.N, by = org_unit_id]
cam[1 < count & !is.na(org_unit_id) ]
# --------------------

# --------------------
# check whether the merge works and add org_unit ids

# cam[!(cam$org_unit %in% dt$org_unit)]
# exp = merge(cam, dt, by = 'org_unit', all.x=T)
# write.csv(paste0(dir, 'cam_edits2.csv'))

# ed = data.table(read.csv(paste0(outDir, 'cam_edits.csv')))
# ed[ , count:=.N, by = org_unit_id]
# ed[1 < count & !is.na(org_unit_id) ]
# --------------------

# --------------------
# edit the cameroon pepfar file to be as you like

# change the regions to match pepfar data and shape file
cam[region=="EXTREME-NORD", region:='Extreme Nord']
cam[region=="ADAMAOUA", region:='Adamaoua']
cam[region=="North West Region", region:='Nord Ouest']
cam[region=="Southwest", region:='Sud Ouest']
cam[region=="West", region:='Ouest']
cam[region=="Center", region:='Centre']
cam[region=="SUD", region:='Sud']
cam[region=="East", region:='Est']

# --------------------
# merge in the geographic information
geo = dt[ , .(org_unit_id, egpaf, latitude, longitude)]
cam = merge(cam, geo, by = 'org_unit_id', all.x=T)

# fix the order
cam = cam[ ,.(site = org_unit, org_unit_id, region, district, egpaf, 
        ip, latitude, longitude)]

# replace missings with false - thes eunits did not merge on org_unit_id
cam[is.na(egpaf) & ip!='EGPAF', egpaf:=FALSE]

# --------------------

# --------------------
# EXPORT DATA 

saveRDS(cam, paste0(outDir, 'cameroon_pepfar_units.rds'))
write.csv(cam, paste0(outDir, 'cameroon_pepfar_units.csv'))
# --------------------








