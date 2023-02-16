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
main_dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/cameroon/dhis2/'

# --------------------
# load the function that fixes diacritical marks
source('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Documents/GitHub/sie/all/fix_diacritics_function.R')

# --------------------

# -----------------------------------------
# PREP THE FILES FOR ANALYSIS AND MODELING
# -----------------------------------------

# -------------------
# ORG UNITS LIST
# -------------------
# -------------------------
# load the org units data 
org_units = readRDS(paste0(main_dir, 'raw/organisationUnits.rds'))

# create a list of just health facilities
units = org_units[level==6]
setnames(units, 'parentID', 'parent')

# merge in the districts
districts =  org_units[level==5]
setnames(districts, c('parent', 'district', 'district_level', 'parentID'))
units = merge(units, districts, by = 'parent')
setnames(units, 'parent', 'site_parent')

# merge in the clusters
clusters = org_units[level==4 & grepl("Cluster", orgUnit)]
setnames(clusters, c('parentID', 'cluster', 'cluster_level', 'parent'))
units = merge(units, clusters, by = 'parentID')

# merge in the regions
regions = org_units[level==3]
setnames(regions, c('parent', 'region', 'region_level', 'country_id'))
units = merge(units, regions, by = 'parent')
# level 2 is country and level 1 is EGPAF
# drop excess variables and add country
units[ ,c('parent', 'parentID', 'district_level', 'cluster_level', 'region_level',
          'country_id'):=NULL]
units[, country:='Cameroon']

# add a variable for org unit name without diacritical marks
units[ , orgUnitClean:=fix_diacritics(orgUnit)]

# format CSIU number one 
units[orgUnitID=='mcxNt5Ri37j', orgUnitClean:='CSIU Number 1']

# convert to title case
units[ ,  orgUnitClean:=str_to_title(orgUnitClean)]

# acronyms should not be in title case
units[ , orgUnitClean:=gsub('Csiu', 'CSIU', orgUnitClean)]
units[ , orgUnitClean:=gsub('Csm', 'CSM ', orgUnitClean)]
units[ , orgUnitClean:=gsub('Csi', 'CSI ', orgUnitClean)]
units[ , orgUnitClean:=gsub('Cs', 'CS', orgUnitClean)]
units[ , orgUnitClean:=gsub('Hd', 'HD', orgUnitClean)]
units[ , orgUnitClean:=gsub('Hr', 'HR', orgUnitClean)]
units[ , orgUnitClean:=gsub('Cma', 'CMA ', orgUnitClean)]
units[ , orgUnitClean:=gsub('Cms', 'CMS ', orgUnitClean)]
units[ , orgUnitClean:=gsub('Cm', 'CM ', orgUnitClean)]
units[ , orgUnitClean:=gsub('Cmes', 'CMES ', orgUnitClean)]

# re-arrange to an intuitive order
units = units[ ,.(orgUnit, orgUnitClean, orgUnitID, parentID = site_parent, level,
          district, cluster, region, country)]

# check for duplicates
duplicated(units)
units[ , length(unique(orgUnitID))] # currently 75 units
units[ , length(unique(orgUnit))]

# -------------------------

# -------------------------
# export the RDS file for R analysis
saveRDS(units, paste0(outDir, 'org_units.RDS'))

# export all org units as a csv fro Power BI
write.csv(units, paste0(outDir, 'org_units.csv'))
# -------------------------

# ----------------------------------------
# -------------------
# DATA ELEMENTS LIST
# -------------------

# load the data elements and format
data_elements = readRDS(paste0(main_dir, 'raw/data_elements.rds'))

elements = data.table(name = unlist(data_elements$name),
                      id = unlist(data_elements$element_id),
                      displayName = unlist(data_elements$displayName),
                      shortName = unlist(data_elements$shortName),
                      displayShortName = unlist(data_elements$displayShortName),
                      lastUpdated = unlist(data_elements$lastUpdated))

# drop cm from short name
elements[ , shortName:=gsub('CM.', '', shortName)]

# check for duplicates
duplicated(elements)
elements[, length(unique(id))] # 591 elements at last check

# -------------------------
# order is already intuitive/ok
# export the RDS file for R analysis
saveRDS(elements, paste0(outDir, 'data_elements.RDS'))

# export all org units as a csv fro Power BI
write.csv(elements, paste0(outDir, 'data_elements.csv'))
# -------------------------

# -----------------------------------------
# -------------------
# DATA SETS LIST
# -------------------

# load the data elements and format
data_sets = readRDS(paste0(main_dir, 'raw/data_sets.rds'))

# place the variables in an intuitive order
data_sets = data_sets[ ,.(name, id, shortName, displayFormName, lastUpdated)]

# -------------------------
# export the RDS file for R analysis
saveRDS(data_sets, paste0(outDir, 'data_sets.RDS'))

# export all org units as a csv fro Power BI
write.csv(data_sets, paste0(outDir, 'data_sets.csv'))
# -------------------------

# -------------------
# SET ELEMENT LINKAGE
# -------------------





