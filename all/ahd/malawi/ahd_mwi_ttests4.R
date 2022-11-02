# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/1/22
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
# CLEAN THE DATA: T-TESTS ON BINARY OUTCOMES
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



table(tt[variable=='lamtest']$value)

test = t.test(tt[period=='b' & variable=='lamtest']$value, 
       tt[period=='e' & variable=='lamtest']$value, var.eqal = FALSE)





# ----------------------------------------------
# T-TEST LOOP: BINARY OUTCOMES
# ----------------------------------------------
# write a loop that runs a Welch's t-test on every binary variable

i = 1 # keep track of iteration of the loop
for (v in unique(tt$variable)) {
  
  # format the variable name 
  var_name = as.character(v)
  
  # some variables do not have enough data and generate errors
  # exclude those variables from the analysis
  excl_vars = c('hivtest', 'hivresult', 'complete_cryptoindcuti2weeks', 'everart',
                'sstest', 'lumbar_referred',
                'restarted_art', 'lamresult', 'crag_result', 'csf_result')
  if (var_name %in% excl_vars) next
  
  # run t-tests for samples with unequal variance
  test = t.test(tt[period=='b' & variable==var_name]$value, 
          tt[period=='e' & variable==var_name]$value,
          var.eqal = FALSE)
  
  # print the variable name so that the output is easy to recognize in a Word doc
  print(paste0('Variable: ', var_name))
  print(test)
  
  # ------------------------
  # save all of the outcomes in a data table
  
  # extract the elements of the test
  estimate = test$estimate
  mean_x = estimate[[1]]
  mean_y = estimate[[2]]
  difference = round((mean_y - mean_x), 2)
  base_mean = round(mean_x, 2)
  end_mean = round(mean_y, 2)
  
  p_full = test$p.value # p-value
  p = as.numeric(round(test$p.value, 4))
  if (p==0) p = "0.000***"
  if (p==0.001) p = "0.001**"
  
  ci_lower = test$conf.int[[1]] # 95% confidence interval
  ci_upper = test$conf.int[[2]]
  ci = paste0(round(ci_lower, 2), ' - ', round(ci_upper, 2))
  
  t_full = test$statistic # t-statistic
  t = round(t_full, 3)
  
  d_full = test$parameter # degrees of freedom
  df = round(test$parameter, 2)
  
  # t-test results for export
  x = cbind(var_name, base_mean, end_mean, difference, p, ci, t, df)
  
  # create a table of all of the results
  if (i == 1) full_data = data.table(x)
  if (1 < i) full_data = data.table(rbind(full_data, x))
  # ------------------------  
  # move into the next iteration of the loop and track the iterations 
  i = i+1
  
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



