# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/25/2021
# Tanzania data 
# Run t-tests on major variables
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

# --------------------
# subset to the viral suppression data 

v_dt = dt[ ,.(ahd_hvl, suppressed) , by =.(sex, age_cat, period, site)]

# --------------------
# run a series of binomial models - received a viral load test
















# ----------------------------------------------
# T-TESTS ON BINARY OUTCOMES: TANZANIA
# ----------------------------------------------

# ------------------------
# run a loop that t-tests every binary variable

# create a data set for only binary variables and cohort
tt = dt[ ,c('period',
            'cd4done_after_ahdelig', 'cd4_after_ahdelig_result',
            'whostage1_done',
            'tbsympscrn',  'sstest','gxtest',
            'tptstart', 'tptcplt',
            'everart', 'art6m')]

# rename cd4 variables because the variable names are long 
setnames(tt, c('cd4done_after_ahdelig', 'cd4_after_ahdelig_result'),
         c('cd4_done', 'cd4_result'))

# ------------------------
# in Tanzania, some variables were coded as missing rather than false

# 9 patients in TZ did not have documented WHO staging, coded as NA
# recode as false
tt[is.na(whostage1_done), whostage1_done:=FALSE]

# tb symptom screening was coded T if any outcome recorded in the period
# all other already coded FALSE - indicates no screen OR no outcome recorded

# sstest and gxtest ok - small numbers
# consider a different denominator - e.g. screened positive

# ------------------------
# shape the data long
tt = melt(tt, id.vars='period')

# drop missing data 
tt = tt[!is.na(value)]
# ------------------------

# ------------------------
# test variable 
table(tt[variable=='whostage1_done']$value)
t.test(tt[period=='b' & variable=='whostage1_done']$value, tt[period=='e' & variable=='whostage1_done']$value,
       var.eqal = FALSE)
# ------------------------

# write a loop that runs a Welch's t-test on every binary variable
for (v in unique(tt$variable)) {
  
  # format the variable name 
  var_name = as.character(v)
  
  # some variables do not have enough data and generate errors - exclude these
  excl_vars = c('cd4done_after_ahdelig')
  if (var_name %in% excl_vars) next
  
  # print the variable name so that the output is easy to recognize in a Word doc
  print(paste0('Variable: ', var_name))
  
  # run t-tests for samples with unequal variance
  print( t.test(tt[period=='b' & variable==var_name]$value, tt[period=='e' & variable==var_name]$value,
                var.eqal = FALSE)) 
}

# ------------------------

# ------------------------
# check that some variables are the same even with a subset denominator

# compare the proportion of patients with symptoms of tb out of those screened
ts = dt[tbsympscrn==T & !is.na(tbsympscrn_result), .(tbsympscrn_result, period)]
t.test(ts[period=='b']$tbsympscrn_result, ts[period=='e']$tbsympscrn_result,
       var.eqal = FALSE)

# compare the proportion of patients who are virally suppressed out of those tested
sup = dt[ahd_vl==T & !is.na(suppressed), .(suppressed, period)]
t.test(sup[period=='b']$suppressed, sup[period=='e']$suppressed,
       var.eqal = FALSE)


# ----------------------------------------------
# T-TESTS ON NUMERICAL OUTCOMES: TANZANIA
# ----------------------------------------------

# ------------------------
# viral load testing - result for suppression ran above
t.test(dt[period=='b']$ahd_vl_result, dt[period=='e']$ahd_vl_result,
       var.eqal = FALSE)

# ----------------------------------------------






