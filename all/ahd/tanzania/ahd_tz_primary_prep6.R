# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/14/2021
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
cohort = 'baseline'
append = FALSE 

# read in the Excel file
dt = data.table(read_xlsx(paste0(dir, 'raw/tanzania_ahd_primary.xlsx'), sheet = cohort))
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

# these variables have no data in the baseline: 
# lamtest_dt, lamreturn_dt, lamresult

# contain data - very limited
dt[!(is.na(sstest_return_dt))]
dt[!(is.na(gxtest_return_dt))]
dt[!(is.na(crag_dt))]
dt[!(is.na(crag_result_dt))]
dt[!(is.na(crag_result))]
dt[!(is.na(lumbar_done))]

# all missing 
dt[all(is.na(lamtest_dt))]
dt[all(is.na(lamreturn_dt))]
dt[all(is.na(lamresult))]


# ------------------------
# format the date variables 

# all date names include dt 
# some dates with all missing data import as logicals
dates = c(names(dt)[ grepl("dt", names(dt))])

# convert the date variables to date type variables
date_byVars = names(dt)[!(names(dt) %in% dates)] # list of not dates
dt = dt[ , lapply(.SD, as.Date), by = date_byVars]
# ------------------------





