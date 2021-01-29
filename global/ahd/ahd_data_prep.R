# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/28/21
# Prep data from the AHD study for visualization
# Check for errors or quality concerns in the data 
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(eeptools) 
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/AHD/'

# set working directory to source other scripts
setwd('C:/Users/ccarelli/Documents/GitHub/sie/all/ahd/')

# --------------------
# source additional code files 

#run plots?
run_plots = TRUE

# run data quality checks? 
dq_checks = TRUE
#---------------------------------------
# TANZANIA BASELINE DATA PREP

# --------------------
# First tab - enrollment indicators

# upload the data 
dt = data.table(read_xlsx(paste0(dir, 
        '/data/Tanzania Baseline Data Abstraction - October 6th, 2020.xlsx'), 
         sheet = 2, skip = 3))

# drop empty column names - most begin with x (e.g. x1)
drop_names = names(dt)[ grepl("^x", names(dt))]
dt[ ,c(drop_names, 'dov', 'gxtest_dt', 'gxpos'):=NULL]

# --------------------
# check and convert data types for ease of analysis

# --------------------
# convert dates to date types

# all date names include dt except dob and firstvis
dates = c(names(dt)[ grepl("dt", names(dt))], 'dob', 'firstvis')

# convert the date variables to date type variables
date_byVars = names(dt)[!(names(dt) %in% dates)] # list of not dates
dt = dt[ , lapply(.SD, as.Date), by = date_byVars]

# the following variable appears as POSIX CT but is an import error
setnames(dt, 'ahd_whostage_incwho', 'ahd_whostage_incwho1' )
dt[grepl(3, ahd_whostage_incwho1), ahd_whostage_incwho:=3]
dt[grepl(4, ahd_whostage_incwho1), ahd_whostage_incwho:=4]
dt[ , ahd_whostage_incwho1:=NULL] #after this, should be no more POSIXCT in data

# --------------------
# convert1/0s to logicals

idVars = c('pid', 'siteid', 'dhisid', 'dhisname', 'region', 'district')

# create a list of variables that should not be converted to logicals
log_byVars = c(idVars, dates, 'ahd_elig', 'cd4_after_ahdelig_result', 
            'whostage1st', 'hvl6mcat', 'hvl6mresult', 'whostage_fvis', 
            'transferinid', 'cd4_fvis', 'ahd_new_cd4result', 'ahd_new_whostage', 
            'ahd_oclin_whostage', 'ahd_hvlresult', 'ahd_hvlcat', 'ahd_cd4result')

#convert remaining variables to logicals
dt = dt[ , lapply(.SD, as.logical), by = log_byVars]

# --------------------
# convert eligibility classification to a factor

dt$ahd_elig = factor(dt$ahd_elig, levels = c(1:6), labels = c('Newly diagnosed',
      'LTFU and returned', 'On ART vir failure', 'On ART AIDS ill',
      'Under 5', 'CD4 < 200'))
# --------------------
# calculate age 

# age represents age at enrollment in the study
# treat like an age - all ages rounded down to the closest year
dt[!is.na(dob), age:=floor((age_calc(dob, enddate = ahd_dt, 
             units = 'years', precise = TRUE)))]

# if a patient is under 5, their eligibility reason should be under 5
# priority ranking lists under 5 as top (if more than one eligibility)
dt[age < 5, ahd_elig:='Under 5']

# --------------------
# add categories to subset while doing analysis


#-------------------------------------------------


if (run_plots==T) source('/ahd_diagnostic_plots.R')
#-------------------------------------------------
# DATA QUALITY

# --------------------
# examine the data quality challenges

# export a list of sites to compare to study criterion
sites = dt[ ,.(siteid = unique(siteid)), by = .(dhisid, dhisname, region, district)]
sites = sites[!is.na(dhisid)]
sites = dt[ ,.(siteid, dhisid, dhisname, region, district)] # put siteid first
write.csv(sites, paste0(dir, '/data/prepped/sites_included.csv'))

# patient ids: these counts should be the same (length of sheet and length of data)
dt[ ,length(unique(pid))]
nrow(dt)

# export a list of repeat patient identifiers
dt[ , pid_count:=.N, by = pid]
pids = dt[1 < pid_count]
pids = pids[order(pid)]
write.csv(pids, paste0(dir, '/data/prepped/repeat_pt_ids.csv'))

# export the rows with substantial missing data 
missing_rows = dt[is.na(ahd_dt)]
write.csv(missing_rows, paste0(dir, '/data/prepped/rows_with_missing_data.csv'))

# --------------------


#WHY ISNT THIS CHANGING