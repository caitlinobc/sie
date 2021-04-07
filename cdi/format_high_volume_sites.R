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
fac [ ,c('X', 'country', 'egpaf', 'egpaf_facility'):=NULL]

# --------------------
# check how many match

dt[Site %in% fac$name] #44 in the list

dt[!(Site %in% fac$name)] 


# fix the errant site names
dt[Site=='Dispensaire SÏur Catherine', Site:='Dispensaire Sour Catherine']













