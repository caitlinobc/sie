# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 5/20/2022
# Prep data and create maps for Castelli Campaign
# Use addresses to find geolocations
# Map using county and district level shape files
# ----------------------------------------------

# ------------------------
# load packages

rm(list=ls()) # clear the workspace
library(readxl)
library(xlsx)
library(data.table)
library(lubridate)
library(plyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(ggrepel)
library(tools)
library(ggmap)
library(sf)
# ------------------------

# ----------------------------------------------
# DIRECTORIES AND SHAPE FILES
# ----------------------------------------------
# ------------------------
# set working directories 
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/cc/'

# ------------------------

# ----------------------------------------------
# IMPORT AND AGGREGATE THE DATA 
# ----------------------------------------------

# ------------------------
# four tabs in the data set - clean separately and aggregate

# ------------------------
# create a function that cleans each tab and saves the volunteer name
volunteer_function = function(x) {
  # import the volunteer name, page range, delete first row
  entrant = x[ ,1][1] # preserve page range and volunteer name
  x = x[-1,] # delete first row
  x$volunteer = str_to_title(trimws(unlist(lapply(str_split(entrant, '-'), '[', 1))))
  x$start_page = gsub("pg. ", "", trimws(unlist(lapply(str_split(entrant, '-'), '[', 2))))
  x$end_page = trimws(unlist(lapply(str_split(entrant, '-'), '[', 3)))
  return(x)}
# ------------------------

# ------------------------
# tab 1 - import the data set and convert to a data table
dt1 = data.table(read_xlsx(paste0(dir, 'Dem Petition Signers.xlsx'), sheet = 1))

tab1 = dt1[ , 1:3]
tab2 = dt1[ , 6:8]
tab3 = dt1[ , 11:13]
tab4 = dt1[ , 17:19]

setnames(tab1, c('name', 'address', 'county'))
setnames(tab2, c('name', 'address', 'county'))
setnames(tab3, c('name', 'address', 'county'))
setnames(tab4, c('name', 'address', 'county'))

tab1 = volunteer_function(tab1)
tab2 = volunteer_function(tab2)
tab3 = volunteer_function(tab3)
tab4 = volunteer_function(tab4)

dt1 = rbind(tab1, tab2, tab3, tab4)

# ------------------------
# tab 2 - import the data set and convert to a data table
dt2 = data.table(read_xlsx(paste0(dir, 'Dem Petition Signers.xlsx'), sheet = 2))

tab1 = dt2[ , 1:3]
tab2 = dt2[ , 5:7]
tab3 = dt2[ , 10:12]

setnames(tab1, c('name', 'address', 'county'))
setnames(tab2, c('name', 'address', 'county'))
setnames(tab3, c('name', 'address', 'county'))

tab1 = volunteer_function(tab1)
tab2 = volunteer_function(tab2)
tab3 = volunteer_function(tab3)

dt2 = rbind(tab1, tab2, tab3)

# ------------------------
# tab 3 - import the data set and convert to a data table
dt3 = data.table(read_xlsx(paste0(dir, 'Dem Petition Signers.xlsx'), sheet = 3))

tab1 = dt3[ , 1:3]
tab2 = dt3[ , 6:8]
tab3 = dt3[ , 11:13]
tab4 = dt3[ , 26:28]
# will include 16 - 18 and 21 - 23 - not yet entered

setnames(tab1, c('name', 'address', 'county'))
setnames(tab2, c('name', 'address', 'county'))
setnames(tab3, c('name', 'address', 'county'))
setnames(tab4, c('name', 'address', 'county'))

tab1 = volunteer_function(tab1)
tab2 = volunteer_function(tab2)
tab3 = volunteer_function(tab3)
tab4 = volunteer_function(tab4)

dt3 = rbind(tab1, tab2, tab3, tab4)

# ------------------------
# tab 4 - import the data set and convert to a data table
dt4 = data.table(read_xlsx(paste0(dir, 'Dem Petition Signers.xlsx'), sheet = 4))

tab1 = dt4[ , 1:3]
tab2 = dt4[ , 6:8]

setnames(tab1, c('name', 'address', 'county'))
setnames(tab2, c('name', 'address', 'county'))

tab1 = volunteer_function(tab1)
tab2 = volunteer_function(tab2)

dt4 = rbind(tab1, tab2)

# ------------------------
# bind the data together
dt = rbind(dt1, dt2, dt3, dt4)

# ----------------------------------------------
# CLEAN THE DATA AND EXPORT
# ----------------------------------------------

# ------------------------
# drop signatures in which the name was not found in the database
dt = dt[!is.na(name)] # name not found in database

# drop names written as a hyphen or question mark indicating not in database
dt = dt[name!='-' & name!='?']
dt = dt[!grepl("\\?", name)] # any question marks in name excluded

# ------------------------

# ------------------------
# one volunteer wrote COMPLETED - drop and convert to numeric
dt[ , start_page:=as.numeric(start_page)]
dt[ , end_page:=as.numeric(trimws(gsub("\\(COMPLETED)", "", end_page)))]

# ------------------------
# add name variables for analysis
# ------------------------

# extract the first name
dt[ , first:=trimws(unlist(lapply(str_split(name, ' '), '[', 1)))]

# determine the length of the name
for (i in seq(1:nrow(dt))){
x = length(str_split(dt$name, ' ')[[i]])
if (i==1) lengths = x
if (1 < i) lengths = rbind(lengths, x)
i = i+1 }
dt[ , name_length:=lengths]

# add the middle initial
dt[2 < name_length, middle:=trimws(unlist(lapply(str_split(name, ' '), '[', 2)))]

# add the last names
dt[name_length==2, last:=trimws(unlist(lapply(str_split(name, ' '), '[', 2)))]
dt[name_length==3, last:=trimws(unlist(lapply(str_split(name, ' '), '[', 3)))]
dt[name_length==4, last:=trimws(unlist(lapply(str_split(name, ' '), '[', 4)))]

# if the voter has a distinguishing junior/senior, add it
dt[middle=="II", suffix:="II"]
dt[grepl("Jr", name), suffix:="Jr"]
dt[grepl("Sr", name), suffix:="Sr"]

# correct errant middle initials (in fact a suffix)
dt[middle=="II", middle:=NA] # used to demarcate second, not middle name

# drop name length - only used to generate variables
dt[ , name_length:=NULL]

# ------------------------
# clean up geographic information
# ------------------------
# specify the mailing address town
dt[ , mail_town:=trimws(unlist(lapply(str_split(address, ','), '[', 2)))]

# ------------------------
# if the mail town is missing, likely due to no comma 
# choose the final name of the town
# dt[is.na(mail_town), mail_town2:=word(address, -1)]
# ------------------------

# add NY to the addresses to map them
dt[ , addresses:=paste0(address, ', NY')]

# if a hashtag is in the adress, delete it - it misses with the API
dt[ , addresses:=gsub("#", "", addresses)]

# ----------------------------------------------
# EXTRACT GEOGRAPHIC COORDINATES
# ----------------------------------------------

# ------------------------
# secret google that no one must ever discover
# register_google(key = "AIzaSyBxI3udOdHwJPFP4elRMCY3d5UUXYNH5tY", write = TRUE)
# ------------------------

# ------------------------
# download the coordinates from google maps
for(i in 1:nrow(dt)) {
result = data.table(geocode(dt$addresses[i]))
if (i==1) codes = result
if (1 < i) codes = rbind(codes, result)
i = i+1}
# ------------------------

# ------------------------
# add them to the data set
dt = cbind(dt, codes)

# drop addressesn duplicate variable
dt[ , addresses:=NULL]
# ------------------------

# ----------------------------------------------
# EXPORT THE DATA SETS
# -------------------------------------------

# ------------------------
# export an RDS file to use
saveRDS(dt, paste0(dir, 'cc_signers_prepped.RDS'))

# ------------------------

# ------------------------
# export the cleaned xlsx file
dt_copy = copy(dt)
setnames(dt_copy, c('Name', 'Address', 'County', 'Data Entry Volunteer',
                    'Start Page', 'End Page', 
                    'First Name', 'Middle Name','Last Name', 'Suffix', 
                    'Mailing Address Town','Longitude', 'Latitude'))
write.csv(dt_copy, paste0(dir, 'Dem Petition Signers Cleaned.csv'))

# ------------------------


