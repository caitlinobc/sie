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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Cote d\'Ivoire/data/'
         
# import the high volume sites 
dt = data.table(read.csv(paste0(dir, 'newsletter_q1/high_volume_sites.csv' )))      

# import the DATIM api site list
fac = read.csv('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/prepped/cdi_health_facilities.csv')
fac = data.table(fac)

# --------------------
# subset the DATIM api list 

#  egpaf administered facilities
fac = fac[egpaf==TRUE]
fac [ ,c('X', 'country', 'egpaf', 'egpaf_facility',
         'level', 'type', 'district'):=NULL]

# --------------------
# check how many match

dt[Site %in% fac$name] #44 in the list

# fix the errant site names
dt[grepl('Catherine', Site), Site:='Dispensaire Sour Catherine']

# two sites continue not to match - ask CDI team
dt[!(Site %in% fac$name)] 

# --------------------
# export the list of sites for the question

write.csv(fac, paste0(dir, 'newsletter_q1/datim_site_list.csv'))

# --------------------
# format and combine the lists

#format the names
setnames(fac, c('orgUnit id', 'Site', 'Parent ID', 'Sub-District',
                'Region', 'Latitude', 'Longitude'))

# merge in the detailed metadata
dt = merge(dt, fac, by='Site', all.x=TRUE)

# --------------------
# export the combined list of sites with metadata 

write.csv(fac, paste0(dir, 'newsletter_q1/high_impact_sites_final.csv'))


# --------------------















