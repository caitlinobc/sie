# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/1/22
# Malawi data set
# Run descriptive statistics for the technical report
# Majority of descriptiv stats are from the tables
# Sources the data prepped in ahd_mwi_prep1.R
# ----------------------------------------------

# ------------------------
# load packages

rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(plyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)
# ------------------------

# ------------------------
# files and directories

# set the working directory to the ahd data sets
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/malawi/'
setwd(dir)

# set the output directory for prepped data 
prepDir = paste0(dir, 'prepped/')

# set the output directory for tables
outDir = paste0(dir, 'outputs/tables/')

# ------------------------
# import the data
dt = readRDS(paste0(prepDir, 'full_data.RDS'))
# ------------------------

# ----------------------------------------------
# HIV STATUS
# ----------------------------------------------
 # almost exclusively uses tables/calculations in Excel

# calculate range of ages at which both sexes tested HIV+ by cohort
dt[!is.na(dtpos) & !is.na(dob), hiv_diag_age:=as.numeric(floor((dtpos - dob)/365))]
dt[hiv_diag_age < 0, hiv_diag_age:=NA ]

# run stats on it 
dt[ ,mean(hiv_diag_age, na.rm=T), by = .(sex, period)]
dt[ ,mean(hiv_diag_age, na.rm=T), by = period]
dt[ ,median(hiv_diag_age, na.rm=T), by = .(sex, period)]
dt[ ,median(hiv_diag_age, na.rm=T), by = period]

dt[sex=='Male' ,range(hiv_diag_age, na.rm=T), by = period]
dt[sex=='Female' ,range(hiv_diag_age, na.rm=T), by = period]
# ----------------------------------------------
# CD4 CELL COUNT
# ----------------------------------------------
dt[!(is.na(cd4_after_ahdelig_result)), cd4_after_ahdelig_result]

# counts of cd4 below 200
dt[!(is.na(cd4_after_ahdelig_result)), range(cd4_after_ahdelig_result), by = sex]
dt[!(is.na(cd4_after_ahdelig_result)) & sex=='Male' & age < 5, cd4_after_ahdelig_result]
dt[cd4_after_ahdelig_result < 200]

# time to diagnosis
ah = dt[!is.na(cd4_after_ahdelig_dt)]

# number of people with the same diagnosis date and cd4
ah[ahd_dt == cd4_after_ahdelig_dt ]
ah[ahd_dt==cd4_after_ahdelig_dt & cd4_after_ahdelig_result < 200]

# two patients are missing an AHD diagnosis date
ah[ahd_dt!=cd4_after_ahdelig_dt, length(unique(pid))]
ah[ahd_dt!=cd4_after_ahdelig_dt]
ah[is.na(ahd_dt)]

# histogram for excel - output the data
cd4_hist = dt[!is.na(cd4_after_ahdelig_result), cd4_after_ahdelig_result]

hist(cd4_hist, xlab = 'CD4 cell count', main = NULL)
# ----------------------------------------------
# WHO CLINICAL STAGING 
# ----------------------------------------------

# ------------------------
# was who staging performed?
dt[!is.na(whostage1_done)]
dt[is.na(whostage1_done), length(unique(pid)), by = period]
dt[, sum(whostage1_done, na.rm=T)]

# documented who stage
dt[!is.na(whostage1st)]
# ------------------------

# ----------------------------------------------
# TB SCREENING AND TESTING 
# ----------------------------------------------

# TB screening done through tables

# genexpert testing - add positivity by sex
dt[gxresult==TRUE & period=='b']
dt[gxresult==TRUE & period=='e']

# ----------------------------------------------
# TPT
# ----------------------------------------------

# calculate the extent of missingness
dt[!is.na(tptstart)]
dt[!is.na(tptstart), length(unique(pid)), by = period]
dt[!is.na(tptstart), unique(tptstart)]

dt[tptstart==T & tbsympscrn_result==TRUE]

dt[tbsympscrn_result==FALSE, length(unique(pid))]
dt[tbsympscrn_result==FALSE & tptstart==T, length(unique(pid))]

# ----------------------------------------------
# CRYPTO
# ----------------------------------------------

dt[, sum(csf_result, na.rm=T)]
dt[!is.na(crypto_regimen)]

dt[csf_result==T]
# ----------------------------------------------

