# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/8/21
# Create an accurate list of health facilities
# As of February 2021, there are 72 implementing
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
library(jsonlite)
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Cameroon/data/facilities/'

# --------------------
# import the data 
dt = data.table(read.csv((paste0(dir, 'facilities_list_raw.csv'))))

# --------------------
# use counts to determine admin type

# drop the country total and create an admin type column
dt = dt[-1]

# name regions
dt[admin=='Littoral' | admin=='Sud', type:='region']

# name facilities
dt[site_count==1, type:='facility']

# name districts
dt[is.na(type), type:='district']

# --------------------
# there are 11 districts with only on facility - change to district
lonely_districts = c('Loum','Manjo', 'Mbanga', 'Melong', 'Njombe Penja',
                     'Nkongsamba', 'Pouma', 'Yabassi', 'Djoum', 'Lolodorf',
                     'Mvangan')
dt[admin %in% lonely_districts, type:='district']

# --------------------
# now each admin is labeled - add its geographic information

# add regions
dt[ , count:=seq(1:.N)]
limit = dt[admin=='Sud', count]
dt[count < limit, region:='Littoral']
dt[limit <= count, region:='Sud']

# --------------------
# check that there are the correct number of facilities in each region
# 51 facilities in Littoral; 21 in Sud
dt[type=='facility', length(unique(admin)), by=region]
dt[type=='facility',length(unique(admin))] # 72 facilities

# drop regional lines
dt = dt[admin!='Littoral' & admin!='Sud']

# --------------------
# save district totals to check later
dist_tot = dt[type=='district', .(admin, site_count)]

# --------------------
# add district names
dt[type=='district', district:=admin]
dt[type=='district', dist1:=count]

# match using a numeric code
dt[ , dist1:=nafill(dist1, "locf")]
match = dt[type=='district', .(district, dist1)]
dt = dt[type!='district']
dt[ , district:=NULL] # drop so the columns do not repeat

dt = merge(dt, match, by = 'dist1')
dt[ ,site_count:=NULL]

# --------------------
# check that the number of facilities per district matches the district total

counts = dt[ ,.(total = length(unique(admin))), by = district]
setnames(counts, 'district', 'admin')
check = merge(dist_tot, counts, by = 'admin')

# the number of facilities in each district should match original counts
if (nrow(check[total!=site_count])!=0) print("Processing issue!")

# --------------------
# final clean up
dt[ ,c('dist1', 'count'):=NULL]
setnames(dt,'admin', 'facility')

# --------------------

# ----------------------------------------------
# save the file
write.csv(dt, paste0(dir, 'prepped_facilities_list.csv'))

# ----------------------------------------------
