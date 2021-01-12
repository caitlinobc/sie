# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/11/2021
# Import and clean the AHD study data
# For now: only Tanzania Baseline Cohort Data
# Run descriptive statistics, including account of missingness
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
library(epitools)
# ------------------------

# --------------------
# files and directories

# set the working directory to the cameroon data
#dir = 'C:/Users/ccarelli/Documents/data/raw_att95_weekly/'
#OutDir = 'C:/Users/ccarelli/Documents/data/prepped/'
dir = 'C:/Users/Caitlin/OneDrive/Documents/'
setwd(dir)

# --------------------
# load the study data 

# load the patient study eligibility and service provision data 
dt = data.table(read_xlsx(paste0(dir,
    'Tanzania Baseline Data Abstraction - October 6th, 2020.xlsx'), 
     sheet = 'flat(leys_siteid,pid)', skip = 3))

# load the tb data 
tb = data.table(read_xlsx(paste0(dir,
      'Tanzania Baseline Data Abstraction - October 6th, 2020.xlsx'), 
       sheet = 'Tbvis_long (siteid,pid,dov)', skip = 1))

# load the baseline data - CD4 testing
cd4 = data.table(read_xlsx(paste0(dir,
    'Tanzania Baseline Data Abstraction - October 6th, 2020.xlsx'), 
    sheet = 'allCD4_flat(siteid,pid)', skip = 1))

# -------------------------------------
# prep the eligibility and service provision data

# drop all the variables beginning with "x" (x1, x2, etc.)
# these variables do not contain data - likely commodity missing
drop_names = data.table(names(dt))
drop_names = drop_names[grepl('^x', drop_names$V1), V1]
dt[ , c('dov', drop_names):=NULL] #dov also has no data 

# calculate age


# recode 1s and 0s as yeses and nos
# in the future use lapply; for today mess around

dt[ , knwstat:=as.logical(knwstat)]
dt[ , hivtest:=as.logical(hivtest)]
dt[ , hivresult:=as.logical(hivresult)]
dt[ , cd4done_after_ahdelig:=as.logical(cd4done_after_ahdelig)]
dt[ , whostage1_done:=as.logical(whostage1_done)]

dt[ , tbsympscrn:=as.logical(tbsympscrn)]
dt[ , tptstart:=as.logical(tptalready)]
dt[ , sstest:=as.logical(sstest)]
dt[ , gxtest:=as.logical(gxtest)]
dt[ , tbtxstart:=as.logical(tbtxstart)]

# more logicals in the data set - add later


# -------------------------------------

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


