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
# DEMOGRAPHICS AND STUDY ELGIBILITY
# -------------------------------------
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

# -------------------------------------
# HIV STATUS
# -------------------------------------
# summarize hiv status
dt[knwstat==F]
dt[is.na(knwstat)]
dt[dtpos < as.Date("2020-10-06 UTC")]
dt[hivresult==TRUE]

# study eligibility
dt[ ,table(ahd_elig)]

# -------------------------------------
# WHO STAGING
# -------------------------------------

dt[ ,table(whostage1_done)]
dt[ ,table(whostage1st)]
dt[ ,table(whostage1st)/2457]

dt[ , table(tbsympscrn)]

# -------------------------------------
# CD4 CELL COUNT
# -------------------------------------
# summarize the cd4 data 

cd[is.na(cd4_after_ahdelig_result) & !is.na(ahd_cd4result), cd4_after_ahdelig_result:=ahd_cd4result]
cd[ , ahd_cd4result:=NULL]
cd[is.na(cd4_after_ahdelig_result) & !is.na(ahd_new_cd4result), cd4_after_ahdelig_result:=ahd_new_cd4result]



# -------------------------------------
# TB SCREENING AND TESTING 
# -------------------------------------

# false is missing (indicates no outcome recorded)
dt[!is.na(tbsympscrn)]
dt[ ,.(sum(tbsympscrn, na.rm=T)), by = period]

