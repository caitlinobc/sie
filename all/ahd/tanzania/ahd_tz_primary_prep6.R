# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/25/2021
# Tanzania Baseline Cohort Data - primary data from files
# Prep the primary data for analysis
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
outDir = paste0(dir, 'outputs/')

# ------------------------
# import the data

# choose which period's data set to process and whether to append
period = 'baseline'
append = TRUE

# read in the Excel file
dt = data.table(read_xlsx(paste0(dir, 'raw/tanzania_ahd_primary.xlsx'), sheet = period))
# ------------------------

# ----------------------------------------------
# PREP THE DATA FOR ANALYSIS
# ----------------------------------------------

# ------------------------
# rename the columns

# drop comments variable
dt[ , comments:=NULL]

# rename x titled variables
setnames(dt, c("pid", "siteid",
    "cd4_return_dt", "tbsympscrn_dt", "tbsympscrn_result",
        "sstest_return_dt", "gxtest_return_dt", 
            "lamtest", "lamtest_dt", "lamreturn_dt", "lamresult",
                "screenedfor_crypto", "crag_dt", "crag_result_dt", "crag_result", 
         "lumbar_referred", "lumbarreferred_dt", "lumbar_done", "lumbar_done_dt",           
              "csf_cragperformed", "csf_cragperformed_dt", "csfcragresultsreturned_dt", "csf_result",
                "fluconazole", "flucon_dt", "ampb_flucon", "ampb_flucon_dt", 
    "ampb_flucyt", "ampb_flucyt_dt", "flucyt_flucon",  "flucyt_flucon_dt", 
      "complete_cryptoindcuti2weeks", "complete_cryptoindcuti2weeks_dt"))
# ------------------------

# ------------------------
# check for any data 

# these variables have no data in either period: 
# one lam result is listed as negative, but false for 'received a test' (error)
dt[ , c('lamtest_dt', 'lamreturn_dt', 'lamresult'):=NULL]

# contain data - very limited
dt[!(is.na(sstest_return_dt))]
dt[!(is.na(gxtest_return_dt))]
dt[!(is.na(crag_dt))]
dt[!(is.na(crag_result_dt))]
dt[!(is.na(crag_result))]
dt[!(is.na(lumbar_done))]

# ------------------------
# format the date variables 

# all date names include dt 
# some dates with all missing data import as logicals
dates = c(names(dt)[ grepl("dt", names(dt))])

# convert the date variables to date type variables
date_byVars = names(dt)[!(names(dt) %in% dates)] # list of not dates
dt = dt[ , lapply(.SD, as.Date), by = date_byVars]
# ------------------------

# ------------------------
# format the logicals 

# ------------------------
# some logicals are coded as characters (results - negative/positive)
# tb symptom screening result is formatted as a character variable
dt[tbsympscrn_result=="negative", tbsympscrn_result:=FALSE]
dt[tbsympscrn_result=="positive", tbsympscrn_result:=TRUE]

dt[crag_result=="negative", crag_result:=FALSE]
dt[crag_result=="positive", crag_result:=TRUE]

dt[csf_result=="negative", csf_result:=FALSE]
dt[csf_result=="positive", csf_result:=TRUE]
# ------------------------

# ------------------------
# recode 2s as 0s
dt[lamtest==2, lamtest:=0]
dt[screenedfor_crypto==2, screenedfor_crypto:=0]
dt[lumbar_referred==2, lumbar_referred:=0]
dt[lumbar_done==2, lumbar_done:=0]
dt[csf_cragperformed==2, csf_cragperformed:=0]

dt[fluconazole==2, fluconazole:=0]
dt[ampb_flucon==2, ampb_flucon:=0]
dt[ampb_flucyt==2, ampb_flucyt:=0]
dt[flucyt_flucon==2, flucyt_flucon:=0]
dt[complete_cryptoindcuti2weeks==2, complete_cryptoindcuti2weeks:=0]
# ------------------------

# ------------------------
# convert logicals to logicals from 0/1
logicals = c('tbsympscrn_result', 'lamtest', 
             'screenedfor_crypto', 'crag_result',
             'lumbar_referred', 'lumbar_done', 
             'csf_cragperformed', 'csf_result',
             'fluconazole', 'ampb_flucon', 'ampb_flucyt', 'flucyt_flucon',
             'complete_cryptoindcuti2weeks')

TF_byVars = names(dt)[!(names(dt) %in% logicals)] # list of not logicals
dt = dt[ , lapply(.SD, as.logical), by = TF_byVars]
# ------------------------

# ------------------------
# add a variable distinguishing baseline from endline
if (period=='baseline') dt[ , period:='b']
if (period=='endline') dt[ , period:='e']
# ------------------------

# ------------------------
# rearrange the data in the order preferred for analysis (original order) 

dt = dt[ ,.(pid, period, siteid, 
   cd4_return_dt, tbsympscrn_dt, tbsympscrn_result,
        sstest_return_dt, gxtest_return_dt, lamtest,
               screenedfor_crypto, crag_dt, crag_result_dt, crag_result, 
    lumbar_referred, lumbarreferred_dt, lumbar_done, lumbar_done_dt,           
        csf_cragperformed, csf_cragperformed_dt, csfcragresultsreturned_dt, csf_result,
          fluconazole, flucon_dt, ampb_flucon, ampb_flucon_dt, 
              ampb_flucyt, ampb_flucyt_dt, flucyt_flucon,  flucyt_flucon_dt, 
               complete_cryptoindcuti2weeks, complete_cryptoindcuti2weeks_dt)]

# ------------------------

# ------------------------
# drop the duplicates

# ------------------------
# create a count of duplicate ids
dt[ , count:=.N, by = pid]
dt[count!=1, ][order(pid)]
# ------------------------

# ------------------------
# there are 4 duplicate ids in the baseline, 8 entries 
# use the main data set to see who was dropped and replicate - baseline 
dt = dt[!(pid=='01-04-0100-009528' & siteid=='14216' )]
dt = dt[!(pid=='03-02-0300-001821' & siteid=='11296' )]
dt = dt[!(pid=='03-05-0100-003460' & siteid=='2569' )]

# receives a b in the baseline - different people (v different dobs)
dt[pid=='03-04-0300-000140' & siteid=='2569', pid:=paste0(pid, 'b')]
# ------------------------

# ------------------------
# there are 5 duplicate ids in the endline, 11 entries 
dt = dt[!(pid=='01-04-0100-010291' & siteid=='2628' )]

# receives a b in the endline - different people (different sexes, dobs)
dt[pid=='02-03-0100-006660' & siteid=='2657', pid:=paste0(pid, 'b')]

# two patient files with same results - take first entry
dt = dt[!(pid=='03-04-0100-010304' & cd4_return_dt=='2020-11-16')]
dt = dt[!(pid=='03-04-0100-001973' & tbsympscrn_dt=='2021-07-02' )]

# three entries, same facility 
dt = dt[!(pid=='02-03-0200-000713' & tbsympscrn_dt=='2020-11-17' )]
dt = dt[!(pid=='02-03-0200-000713' & cd4_return_dt=='2021-05-18' )]

# drop the count variable
dt[ , count:=NULL]
# ------------------------

# -------------------------------------
# OUTPUT THE DATA SETS AND APPEND
# -------------------------------------

# ------------------------
# output the data sets to append 
if (period=='baseline') { saveRDS(dt, paste0(prepDir, 'baseline_primary_to_append.rds'))
} else {saveRDS(dt, paste0(prepDir, 'endline_primary_to_append.rds'))}
# ------------------------

# ------------------------
# bind the data sets together
if (append==TRUE) {
  
  # read in the data sets
  base = readRDS(paste0(prepDir, 'baseline_primary_to_append.rds'))
  end = readRDS(paste0(prepDir, 'endline_primary_to_append.rds'))
  
  # rbind them together
  full_data = rbind(base, end)}
# ------------------------

# -------------------------------------
# REMOVE DUPLICATE PATIENTS - FULL DATA
# -------------------------------------

# ------------------------
# drop duplicate ids across cohorts

# quantify the duplicates
full_data[,  count:=.N, by = pid]
full_data[count!=1] # 272 duplicate patient entries
full_data[count!=1, unique(pid)] # 136 ids
# ------------------------

# ------------------------
# if the entry is in the baseline, do not include it in the endline
full_data = full_data[!(count==2 & period=='e')]
full_data[ ,count:=NULL]
# ------------------------

# -------------------------------------
# SAVE THE FINAL DATA SET
# -------------------------------------

saveRDS(full_data, paste0(prepDir, 'full_primary_data.RDS'))
write.csv(full_data, paste0(prepDir, 'full_primary_data.csv'))
# ------------------------

# -------------------------------------







