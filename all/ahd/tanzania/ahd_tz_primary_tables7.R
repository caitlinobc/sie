# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/25/2021
# Tanzania Baseline Cohort Primary Data
# Uses data from patient files, registers, and master cards
# Create and export tables for analysis
# Sources the data prepped in ahd_tz_prep1.R
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/tanzania/'
setwd(dir)

# set the output directory for prepped data 
prepDir = paste0(dir, 'prepped/')

# set the output directory for tables
outDir = paste0(dir, 'outputs/tables/')

# ------------------------
# import the data
dt = readRDS(paste0(prepDir, 'full_primary_data.RDS'))
# ------------------------

# ----------------------------------------------
# merge in demographic variables from the full data set

# import the full data set and subset to the variables you need
full = readRDS(paste0(prepDir, 'full_data.RDS'))
full = full[ ,.(pid, period, dob, age, age_cat, sex, 
                site, ahd_dt, tbsympscrn)]

# merge in the data sets - 53 entries drop out
# check duplication issue later on
dt = merge(dt, full, by = c('pid', 'period'))
# ------------------------

# ----------------------------------------------
# TB TABLES
# ----------------------------------------------

# ------------------------
# tb symptom screening results
# 134 positive screenings
dt[ , unique(tbsympscrn_result)]
dt[ , sum(tbsympscrn_result, na.rm=T)]

# ------------------------

# ------------------------


# there was only one tb lam test performed
dt[ , unique(lamtest)]
dt[ , sum(lamtest, na.rm=T)]
# ------------------------

# ----------------------------------------------
# CRYPTO CASCADE TABLES
# ----------------------------------------------

# 132 positive crypto screenings
dt[ , unique(screenedfor_crypto)]
dt[ , sum(screenedfor_crypto, na.rm=T)]






