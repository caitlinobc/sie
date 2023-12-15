# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/10/2023
# Process Implementing Partner Data
# Source Link: https://data.pepfar.gov/additionalData#PMD
# Uses the age and sex disaggregated file 

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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/partners/raw/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/partners/prepped/'

# --------------------

# --------------------
# IMPORT DATA 

# read in the pepfar sites received from egpaf-cameroon, both total and disagg
dt_as = data.table(read.delim(paste0(dir, 'Public_Partner_MER_AgeSex.txt')))
dt = data.table(read.delim(paste0(dir, 'Public_Partner_MER_TopLine.txt')))

# change the names for coding ease
setnames(dt_as, c('unit', 'country', 'partner', 'funding', 'mech', 'ip', 
               'indicator', 'num_denom', 'disagg', 'age', 'sex', 'fy', 'target',
               'q1', 'q2', 'q3', 'q4'))
setnames(dt, c('unit', 'country', 'partner', 'funding', 'mech', 'ip', 
                  'indicator', 'num_denom', 'disagg', 'age', 'sex', 'fy', 'target',
                  'q1', 'q2', 'q3', 'q4'))
dt_as[ , c('unit', 'mech'):=NULL]
dt[ , c('unit', 'mech'):=NULL]

# fix the data types
dt_as[ , fy:=as.numeric(fy)]
dt_as[ , target:=as.numeric(target)]
dt_as[ , q1:=as.numeric(q1)]
dt_as[ , q2:=as.numeric(q2)]
dt_as[ , q3:=as.numeric(q3)]
dt_as[ , q4:=as.numeric(q4)]

dt[ , fy:=as.numeric(fy)]
dt[ , target:=as.numeric(target)]
dt[ , q1:=as.numeric(q1)]
dt[ , q2:=as.numeric(q2)]
dt[ , q3:=as.numeric(q3)]
dt[ , q4:=as.numeric(q4)]
# --------------------

# --------------------
# subset to the useful data 

# subset to egpaf countries
egpaf_countries = c("Cameroon", "Cote d'Ivoire", "Democratic Republic of the Congo",
                    "Eswatini", "Kenya", "Lesotho",
                    "Malawi", "Mozambique", "Nigeria", "Tanzania", "Uganda")
dt_as = dt_as[dt_as$country %in% egpaf_countries]
dt = dt[dt$country %in% egpaf_countries]

# subset to appropriate fiscal years
dt_as = dt_as[fy==2022 | fy==2023]
dt = dt[fy==2022 | fy==2023]

# --------------------
# export the data in excel files 
write.csv(dt, paste0(outDir, 'pepfar_partner_totals.csv'))
write.csv(dt_as, paste0(outDir, 'pepfar_partner_disaggregated_totals.csv'))
# --------------------


