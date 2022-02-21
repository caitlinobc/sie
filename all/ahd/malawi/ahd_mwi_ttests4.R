# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/20/2021
# Malawi data set
# Run a series of t-tests to examine changes in key outcome variables
# check samples for unequal variance
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
# T-TESTS ON BINARY OUTCOMES: MALAWI
# ----------------------------------------------

# ------------------------
# run a loop that t-tests every binary variable

# create a data set for only binary variables and cohort
tt = dt[ ,c('period','knwstat', 'hivtest', 'hivresult', 'cd4done_after_ahdelig', 'whostage1_done',
  'tbsympscrn', 'tbsympscrn_result', 'tptstart', 'tptcplt', 'sstest',
  'gxtest', 'lamtest', 'tbtx_start', 'tb_tx_cplt', 'screenedfor_crypto',
  'lumbar_referred', 'csf_cragperformed', 'complete_cryptoindcuti2weeks', 'everart',
  'restarted_art', 'art6m', 'ahd_vl', 'suppressed',
  'ssresult', 'gxresult', 'lamresult', 
  'crag_result','csf_result', 'lumbar_done')]

# shape the data long
tt = melt(tt, id.vars='period')

# drop missing data 
tt = tt[!is.na(value)]

# ------------------------
# test variable - tb lam tests performed go from 155 to 403
table(tt[variable=='lamtest']$value)
t.test(tt[period=='b' & variable=='lamtest']$value, tt[period=='e' & variable=='lamtest']$value, var.eqal = FALSE)
# ------------------------

# write a loop that runs a Welch's t-test on every binary variable
for (v in unique(tt$variable)) {
  
  # format the variable name 
  var_name = as.character(v)
  
  # some variables do not have enough data and generate errors - exclude these 
  excl_vars = c('hivtest', 'hivresult', 'complete_cryptoindcuti2weeks', 'everart',
                'restarted_art', 'lamresult', 'crag_result', 'csf_result')
  if (var_name %in% excl_vars) next
  
  # print the variable name so that the output is easy to recognize in a Word doc
  print(paste0('Variable: ', var_name))
  
  # run t-tests for samples with unequal variance
  print( t.test(tt[period=='b' & variable==var_name]$value, tt[period=='e' & variable==var_name]$value,
          var.eqal = FALSE)) 
  }

# ------------------------

# ----------------------------------------------
# T-TESTS ON NUMERICAL OUTCOMES: MALAWI
# ----------------------------------------------

# ------------------------
# cd4 testing
# t-test on cd4 count - 433 total results - all results are in the endline
# we cannot run a t-test on cd4 due to lack of baseline data 
dt[!is.na(cd4_after_ahdelig_result), unique(cd4_after_ahdelig_result), by = period]
# ------------------------

# ------------------------
# viral load testing - result for suppression ran above
t.test(dt[period=='b']$ahd_vl_result, dt[period=='e']$ahd_vl_result,
       var.eqal = FALSE)

# ----------------------------------------------



