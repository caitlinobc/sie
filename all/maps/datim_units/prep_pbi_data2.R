# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/3/2023
# Prep sites from PBI Data Model 2.0
# Extract all org_units that report HTS_TST
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/maps/org_units/'
setwd(dir)

# set the output directory for prepped data
prepDir = paste0(dir, 'prepped/')

# set the output directory for visualizations and other products
outDir = paste0(dir, 'outputs/')

# change the value for the most recent quarter to accurately import
quarter = 'q2'
fy = 'fy23'
# --------------------

# ----------------------------------------------
# IMPORT AND CLEANING
# ----------------------------------------------

# --------------------
# read in and format the site names

# read in the csv with all sites
dt = data.table(read.csv(paste0(dir, 'raw/active_sites_', quarter, '_', fy, '.csv')))

# format the sites to be at the same level - pbi as level 6 or level 7
setnames(dt, c('country', 'region', 'district', 'level6', 'level7', 
               'hts', 'tx_curr', 'fq', 'orgunit_id',
               'lat', 'long'))

# Cameroon, DRC, Eswatini, Lesotho, Moz never have subdistrict (no Level 7 in Model)
dt[level7=="", site:=level6]
dt[level7!="", site:=level7]
dt[ , level7:=NULL] # if site is level 7, level 7 imported to sites and deleted
dt[level6==site, level6:=NA] # if site is level 6, level 6 is site - empty level 6
setnames(dt, 'level6', 'subdist')

# for drc military sites, no geographic information - reformat with region as "Military"
dt[orgunit_id=="MTlUjdz0HYY", region:="Military"]
dt[orgunit_id=="MTlUjdz0HYY", district:=NA]
dt[orgunit_id=="MTlUjdz0HYY", site:=NA]
dt[orgunit_id=="MTlUjdz0HYY", country:="DRC"]

# re-arrange the columns to an intuitive order
dt = dt[ ,.(country, region, district, subdist, site, orgunit_id, 
            fq, hts, tx_curr, lat, long)]
# --------------------

# --------------------
# country-specific data checks 

# one site in cameroon in fy19 has an orgunitid but no site name
dt[country=="Cameroon" & site=="", site:=NA]

# some data are reported at the district level in CdI, DRC, and Moz
dt[is.na(subdist) & grepl('Cote', country), site:=NA]


dt[country=="Cote d\'Ivoire", length(unique(site)), by = fq]


# eswatini is aok - no missing data 




# ----------------------------------------------
# DATA VALIDATION AND LISTS
# ----------------------------------------------

# --------------------
# CAMEROON

# cameroon had 72 sites throughout FY21 and FY22, but 74/75 in FY23
cam = dt[(fq=="FY22 Q4" | fq=="FY23 Q1" | fq=="FY23 Q2") & country=="Cameroon"]

# check that all the fy22 sites are kept in fy23
table(cam[fq=="FY23 Q1"]$orgunit_id %in% cam[fq=="FY22 Q4"]$orgunit_id)
table(cam[fq=="FY23 Q2"]$orgunit_id %in% cam[fq=="FY22 Q4"]$orgunit_id)

# what are the additional sites added in fy23?
subset(cam[fq=="FY23 Q2"], !(cam[fq=="FY23 Q2"]$orgunit_id %in% cam[fq=="FY22 Q4"]$orgunit_id))
# --------------------

# --------------------
# ESWATINI 

# eswatini had 66 sites until fy23, 78 sites
dt[country=="Eswatini", length(unique(site)), by = fq]
esw = dt[(fq=="FY23 Q1" | fq=="FY22 Q4") & country=="Eswatini"]

# list of new sites in fy23 (12 new sites)
subset(esw[fq=="FY23 Q1"], !(esw[fq=="FY23 Q1"]$orgunit_id %in% esw[fq=="FY22 Q4"]$orgunit_id))
# --------------------


