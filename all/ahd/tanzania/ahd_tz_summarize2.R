# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/14/2021
# Tanzania Baseline Cohort Data
# Summarize the data set for the technical report outline
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
dt = readRDS(paste0(prepDir, 'full_data.RDS'))
# ------------------------

# -------------------------------------
# DEMOGRAPHICS AND STU
# -------------------------------------
# summarize the eligibility data 

# -------------
# size of the data set
nrow(dt)
dt[,length(unique(pid))]

# use tables to determine sex ratios for text
# -------------
# summarize age
dt[, mean(age, na.rm=TRUE), by = period]
dt[, median(age, na.rm=TRUE), by = period]
dt[, range(age, na.rm=TRUE), by = period]

# summarize age by sex
dt[period=='b', mean(age, na.rm=TRUE), by = sex]
dt[period=='e', mean(age, na.rm=TRUE), by = sex]

# count of patients under 5 by period
dt[under5==TRUE, length(unique(pid)), by = period]

# summarize the patients under 5
dt[under5==TRUE, mean(age, na.rm=TRUE)]
dt[under5==TRUE, median(age, na.rm=TRUE)]
dt[under5==TRUE, range(age, na.rm=TRUE)]


# how many patients are <1?
dt[age==0, .(dob, ahd_dt)]

# summarize hiv status
dt[knwstat==F]
dt[is.na(knwstat)]
dt[dtpos < as.Date("2020-10-06 UTC")]
dt[hivresult==TRUE]

# study eligibility
dt[ ,table(ahd_elig)]

# get the percentage


dt[ ,table(whostage1_done)]
dt[ ,table(whostage1st)]
dt[ ,table(whostage1st)/2457]

dt[ , table(tbsympscrn)]

# -------------------------------------
# summarize the cd4 data 

# count of unique rows, sites, patients
nrow(cd4)
cd4[, length(unique(siteid))]
cd4[, length(unique(pid))]

# add a variable for the number of cd4 tests received
cd4[is.na(cd4_after_ahdelig_result1)] # everyone had a first cd4
cd4[is.na(cd4result5) & is.na(cd4result4) & is.na(cd4result3) & is.na(cd4result2),
    cd4_tests:=1]
cd4[is.na(cd4result5) & is.na(cd4result4) & is.na(cd4result3) & !is.na(cd4result2),
    cd4_tests:=2]
cd4[is.na(cd4result5) & is.na(cd4result4) & !is.na(cd4result3) & !is.na(cd4result2),
    cd4_tests:=3]
cd4[is.na(cd4result5) & !is.na(cd4result4) & !is.na(cd4result3) & !is.na(cd4result2),
    cd4_tests:=4]
cd4[!is.na(cd4result5),
    cd4_tests:=5]

# number of patients who received each number of tests
# divide by nrow for percentage 
cd4[ , table(cd4_tests)]
cd4[ , range(cd4_after_ahdelig_result1)]
cd4[ , median(cd4_after_ahdelig_result1)]
cd4[ , IQR(cd4_after_ahdelig_result1)]

# counts below 200 and 500 and percentages
cd4[cd4_after_ahdelig_result1 < 200, length(unique(pid))]
cd4[cd4_after_ahdelig_result1 < 500,  length(unique(pid))]
cd4[cd4_after_ahdelig_result1 < 200, length(unique(pid))/nrow(cd4)]
cd4[cd4_after_ahdelig_result1 < 500,  length(unique(pid))/nrow(cd4)]

# comparing second to  cd4 test
cd4[!is.na(cd4result2), length(unique(pid))]
cd4[!is.na(cd4result2) & cd4_after_ahdelig_result1 < cd4result2, 
    length(unique(pid))]
cd4[!is.na(cd4result2) & cd4result2 < cd4_after_ahdelig_result1, 
    length(unique(pid))]

# calculating mean and median differences between the two tests
cd4[!is.na(cd4result2) & cd4_after_ahdelig_result1 < cd4result2, 
    mean(cd4result2 - cd4_after_ahdelig_result1)]
cd4[!is.na(cd4result2) & cd4_after_ahdelig_result1 < cd4result2, 
    median(cd4result2 - cd4_after_ahdelig_result1)]

cd4[!is.na(cd4result2) & cd4result2 < cd4_after_ahdelig_result1, 
    mean(cd4result2 - cd4_after_ahdelig_result1)]
cd4[!is.na(cd4result2) & cd4result2 < cd4_after_ahdelig_result1, 
    median(cd4result2 - cd4_after_ahdelig_result1)]


# number of days between the first and second test
cd4[!is.na(cd4result2), mean(as.numeric(cd4dt2 - cd4_after_ahdelig_dt1))]
cd4[!is.na(cd4result2), median(as.numeric(cd4dt2 - cd4_after_ahdelig_dt1))]
cd4[!is.na(cd4result2), range(as.numeric(cd4dt2 - cd4_after_ahdelig_dt1))]

# more than six months from first to second test
cd4[!is.na(cd4result2) & 180 < (as.numeric(cd4dt2 - cd4_after_ahdelig_dt1)), 
    length(unique(pid)) ]
cd4[!is.na(cd4result2) & 183 < (as.numeric(cd4dt2 - cd4_after_ahdelig_dt1)), 
    length(unique(pid)) ] # conservative - 6 months without february

# -------------------------------------
# summarize the tb data 


# -------------------------------------
# merge and save one large rds with data set listed


