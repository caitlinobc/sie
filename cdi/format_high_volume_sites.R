# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/6/21
# Format the high volume sites for PBI
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
library(ggplot2)

# --------------------

# --------------------
# import the data 

# set the working directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/CDI/data/'
         
# import the high volume sites 
dt = data.table(read.csv(paste0(dir, 'newsletter_q1/high_volume_sites.csv' )))      

# import the DATIM api site list
fac = read.csv('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps/pepfar_org_units/prepped/cdi_health_facilities.csv')
fac = data.table(fac)

# --------------------
# check how many high impact sites match the datim list
dt[Site %in% fac$name] # 46 of 47 are in the list 

# fix the errant site name
dt[grepl('Catherine', Site), Site:='Dispensaire Sour Catherine']

# check the match - note two sites are not listed in datim as egpaf sites
dt[!(Site %in% fac$name)] 

# --------------------
# format and combine the lists

#format the names in the datim facilities list
fac[ , c('X', 'egpaf_facility', 'egpaf'):=NULL]

#subset the list of datim facilities to only high impact sites
fac = fac[name %in% dt$Site]


setnames(fac, c('orgUnit id', 'Site', 'Level', 'Type', 'Parent ID', 'Sub-District',
                'District', 'Region', 'Country', 'Latitude', 
                'Longitude'))


# --------------------
# export the combined list of sites with metadata 

write.csv(fac, paste0(dir, 'newsletter_q1/high_impact_sites_final.csv'))


# --------------------















