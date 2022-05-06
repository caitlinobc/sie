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
dt = merge(full, dt, by = c('pid', 'period'), all.x = TRUE)
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

# ------------------------
# crag serum screened 
# 131 positive crypto screenings
dt[ , unique(screenedfor_crypto)]
dt[ , sum(screenedfor_crypto, na.rm=T)]

cs = dt[ , length(unique(pid)), by = .(period, age_cat, screenedfor_crypto)]
cs = dcast(cs, age_cat~period+screenedfor_crypto)
cs = cs[ ,.(age_cat, b_TRUE, b_FALSE, b_NA, e_TRUE, e_FALSE, e_NA)]

# export the table
write.csv(cs, paste0(outDir, 'crypto_screened_age.csv'))
# ------------------------

# ------------------------
# crag screening result
# 39 positive results
cs1 = dt[screenedfor_crypto==T & period=='e', .(tested = length(unique(pid))), by = .(age_cat, sex)]
cs2 = dt[screenedfor_crypto==T & period=='e', .(tested_pos = sum(crag_result, na.rm = T)),
         by = .(age_cat, sex)]
cs3 = merge(cs1, cs2, by = c('age_cat', 'sex'), all.x = T)

# reshape the results table
cs3 = melt(cs3, id.vars = c('age_cat', 'sex'))
cs3 = dcast(cs3, age_cat~sex+variable)

# export the table
write.csv(cs3, paste0(outDir, 'crypto_result_age_sex.csv'))
# ------------------------

# ------------------------
# lumbar puncture referred and completed

dt[!is.na(lumbar_referred), length(unique(pid)), by = period]
dt[,sum(lumbar_referred, na.rm=T)]


# ----------------------------------------------
# PRIMARY DATA T-TESTS
# ----------------------------------------------

# separate out the binary variables to run t-tests
tt = dt[ ,.(period, screenedfor_crypto, crag_result, lumbar_referred,
            lumbar_done, csf_cragperformed, csf_result)]

# t-test: crypto screening
ct = dt[!is.na(screenedfor_crypto), .(screenedfor_crypto, period)]
t.test(ct[period=='b']$screenedfor_crypto, ct[period=='e']$screenedfor_crypto,
       var.eqal = FALSE)

# t-test: crypto screening result
crt = dt[!is.na(crag_result), .(crag_result, period)]
t.test(crt[period=='b']$crag_result, crt[period=='e']$crag_result,
       var.eqal = FALSE)

# t-test: referred for a lumbar puncture
# only one patient referred; no patients received a lumbar puncture
lt = dt[!is.na(lumbar_referred), .(lumbar_referred, period)]
t.test(lt[period=='b']$lumbar_referred, lt[period=='e']$lumbar_referred,
       var.eqal = FALSE)

# t-test: csf crag performed
cst = dt[!is.na(csf_cragperformed), .(csf_cragperformed, period)]
t.test(cst[period=='b']$csf_cragperformed, cst[period=='e']$csf_cragperformed,
       var.eqal = FALSE)




