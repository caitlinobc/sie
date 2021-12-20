# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/20/2021
# Tanzania Baseline Cohort Data
# Import and clean the AHD study data
# Processes both the baseline and endline data sets and appends
# Run data quality checks to identify any issues with the extraction
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
library(eeptools)
# ------------------------

# ------------------------
# files and directories

# set the working directory to the ahd data sets
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/tanzania/'
setwd(dir)

# set the output directory for prepped data 
prepDir = paste0(dir, 'prepped/')

# set the output directory for visuals
outDir = paste0(dir, 'outputs/')

# ------------------------
# select baseline or endline for processing raw data 
# determine if you want to append the data sets together

period = 'baseline'

# append the data sets? T/F
append = FALSE

# ------------------------

# -------------------------------------
# load the study data 

# load the patient study eligibility and service provision data - tab 1
dt = data.table(read_xlsx(paste0(dir,
    'raw/Tanzania AHD_', period, '_28.10.xlsx'), 
     sheet = 'flat(leys_siteid,pid)', skip = 3))

# load the sex data to be combined with the original file
dt_sex = data.table(read_xlsx(paste0(dir,
  'raw/tanzania_ahd_', period, '_sex.xlsx')))

# subset the sex data to solely the variables needed to merge
dt_sex = dt_sex[ ,.(sex, siteid, pid, ahd_dt)]

# -------------------------------------
# deal with duplicate patient ids

# create a data set of duplicate id patients and examine
dt[ , count:=.N, by = pid]
dup_ids = dt[count==2, unique(pid)]

# 6 duplicate ids, 12 patients
dup = dt[pid %in% dup_ids, .(pid, dob, ahd_dt)][order(pid, ahd_dt)]
dup[ , count:=seq(1, nrow(dup))]

# export the data for the duplicates
write.csv(dt[pid %in% dup_ids],paste0(prepDir, period, '_duplicate_patient_ids.csv'))

# numbers the entries by date within each pid group
dup$ord = ave(dup$count, dup$pid, FUN = seq_along)

# drop the entry for the same pid, dob, ahd_dt (keep later service delivery)
if (period=='baseline') dt = dt[!(pid=='01-04-0100-009528' & siteid==14216)]
if (period=='baseline') dup = dup[pid!='01-04-0100-009528']

# create a unique identifier based on pid and ahd_dt
dup[ ,c('dob', 'count'):=NULL]
dt = merge(dt, dup, by = c('pid', 'ahd_dt'), all.x = T)

# add a b to the same pids with distinct dobs; get rid of order
dt[(pid=='03-04-0100-009460' | pid=='03-04-0300-000140') & ord==2, pid:=paste0(pid, 'b')]
dt[(pid=='03-04-0100-009460' | pid=='03-04-0300-000140'), ord:=NA]

# drop the earlier entry of the duplicates
dt = dt[is.na(ord) | ord==2]

#------------------------
# re-check the count and drop unecessary variables
dt[duplicated(pid)] # should be empty
dt[ , c('count', 'ord'):=NULL]

# ------------------------

# -------------------------------------
# merge in the sex data 

#add the bs to the ids from baseline
if (period=='baseline') {
dt_sex[pid=='03-04-0100-009460' & siteid==2569, pid:=paste0(pid, 'b')]
dt_sex[pid=='03-04-0300-000140' & siteid==2569, pid:=paste0(pid, 'b')]}

# create a unique identifier for each entry
dt[ ,id:=paste0(pid, ahd_dt, siteid)]
dt_sex[ ,id:=paste0(pid, ahd_dt, siteid)]

# merge based on the unique identifier and drop
dt_sex = dt_sex[ ,.(sex, id)]
dt = merge(dt, dt_sex, by = 'id', all.x = T)
dt[ , id:=NULL]

# -------------------------------------

# -------------------------------------
# LINKED DATA SETS 

# # load the tb data 
# tb = data.table(read_xlsx(paste0(dir,
#       'Tanzania Baseline Data Abstraction - October 6th, 2020.xlsx'), 
#        sheet = 'Tbvis_long (siteid,pid,dov)', skip = 2))
# 
# # load the baseline data - CD4 testing
# cd4 = data.table(read_xlsx(paste0(dir,
#     'Tanzania Baseline Data Abstraction - October 6th, 2020.xlsx'), 
#     sheet = 'allCD4_flat(siteid,pid)', skip = 1))
# -------------------------------------

# -------------------------------------
# DATA PREP AND BASIC QUALITY CHECKS
# -------------------------------------

# ------------------------
# ELIGIBILITY AND INITIAL SERVICE PROVISION DATA 

# ------------------------
# drop all the variables beginning with "x" (x1, x2, etc.)
# these variables do not contain data 

# check that every variable beginning with x is missing
x_names = names(dt)[grepl('^x', names(dt))]
test_dt = melt(dt, id.vars=c('ahd_dt', 'pid', 'siteid')) # warning ok 
test_dt = test_dt[variable %in% x_names]
if (nrow(test_dt[all(is.na(value)),.(unique(variable))])==length(x_names)) print ("All x variables missing!")

# drop out the variables with no associated values
drop_names = data.table(names(dt))
drop_names = drop_names[grepl('^x', drop_names$V1), V1]
dt[ , c('dov', drop_names):=NULL] #dov also has no data 

# ------------------------
# calculate age from dob
# this expresses the age at initial enrollment
dt[ , dob:=as.Date(dob)]
dt[ , ahd_dt:=as.Date(ahd_dt)]
dt[!is.na(ahd_dt), 
   age:=(age_calc(dob, enddate = ahd_dt, units = "years", precise = TRUE))]
dt[ , age:=floor(age)]

# add a binary for under 5
dt[age < 5, under5:=TRUE]
dt[5 <= age, under5:=FALSE]

# check age and dob for missingness
dt[is.na(age)]
dt[is.na(dob)]

# ------------------------
# format additional date variables

# format the date of variables imported as POSIXct
dt[ , dtpos:=as.Date(dtpos)]

dt[ , cd4_after_ahdelig_dt:=as.Date(cd4_after_ahdelig_dt)]
dt[ , whostage1st_dt:=as.Date(whostage1st_dt)]
dt[ , tptstart_dt:=as.Date(tptstart_dt)]
dt[ , tptalready_dt:=as.Date(tptalready_dt)]

dt[ , tptcplt_dt:=as.Date(tptcplt_dt)]
dt[ , sstest_dt:=as.Date(sstest_dt)]
dt[ , firstart_dt:=as.Date(firstart_dt)]
dt[ , hvl6m_dt:=as.Date(hvl6m_dt)]
dt[ , tbtxstart_dt:=as.Date(tbtxstart_dt)]

dt[ , tbtxalready_dt:=as.Date(tbtxalready_dt)]
dt[ , tptcplt_dt:=as.Date(tptcplt_dt)]
dt[ , tbtxcplt_dt:=as.Date(tbtxcplt_dt)]
dt[ , firstvis:=as.Date(firstvis)]
dt[ , cd4_fvis_dt:=as.Date(cd4_fvis_dt)]

dt[ , ahd_u5_dt:=as.Date(ahd_u5_dt)]
dt[ , ahd_new_cd4_dt:=as.Date(ahd_new_cd4_dt)]
dt[ , ahd_oclin_who_dt:=as.Date(ahd_oclin_who_dt)]
dt[ , ahd_incwho_dt:=as.Date(ahd_incwho_dt )]
dt[ , ahd_whostage_incwho:=as.Date(ahd_whostage_incwho)]

dt[ , ahd_hvl_dt:=as.Date(ahd_hvl_dt)]
dt[ , ahd_cd4u200_dt:=as.Date(ahd_cd4u200_dt)]
# ------------------------

# ------------------------
# recode 1s and 0s as logicals
dt[ , knwstat:=as.logical(knwstat)]
dt[ , hivtest:=as.logical(hivtest)]
dt[ , hivresult:=as.logical(hivresult)]
dt[ , cd4done_after_ahdelig:=as.logical(cd4done_after_ahdelig)]
dt[ , whostage1_done:=as.logical(whostage1_done)]

dt[ , tptalready:=as.logical(tptalready)]
dt[ , tptcplt:=as.logical(tptcplt)]
dt[ , ahd_cd4u200:=as.logical(ahd_cd4u200)]

dt[ , tbsympscrn:=as.logical(tbsympscrn)]
dt[ , tptstart:=as.logical(tptalready)]
dt[ , sstest:=as.logical(sstest)]
dt[ , gxtest:=as.logical(gxtest)]
dt[ , tbtxstart:=as.logical(tbtxstart)]

# -------------------------------------
# DATA QUALITY CHECKS
# -------------------------------------

# ------------------------
# check the sites and site ids against the study protocols
# correct sites in baseline
ids = dt[ ,.(siteid = unique(siteid)), by = .(dhisid, dhisname)]
write.csv(ids, paste0(outDir, period, '_facilities_in_data.csv'))

# check for duplicate patient ids and duplicate entries
dt[duplicated(dt)] # no duplicate entries

# check that study eligibility was after the patient was born
dt[ahd_dt < dob] # no entries

# when age is equal to 0, how old is the participant?
dt[age==0, .(dob, ahd_dt)]

# -------------------------------------
# add a five year age category

dt[age < 5, age_cat:='<5']
dt[4 < age & age <=9, age_cat:='5-9']
dt[9 < age & age<=14, age_cat:='10-14']
dt[14 < age & age<=19, age_cat:='15-19']
dt[19 < age & age<=24, age_cat:='20-24']

dt[24 < age & age<=29, age_cat:='25-29']
dt[29 < age & age<=34, age_cat:='30-34']
dt[34 < age & age<=39, age_cat:='35-39']
dt[39 < age & age<=44, age_cat:='40-44']
dt[44 < age & age<=49, age_cat:='45-49']

dt[49 < age, age_cat:='50+']

# factor the age category to sort correctly in figures and tables
dt$age_cat = factor(dt$age_cat, c('<5', '5-9',
        '10-14', '15-19', '20-24', '25-29', '30-34',
        '35-39', '40-44', '45-49', '50+'),
        c('<5', '5-9','10-14', '15-19', '20-24', '25-29', '30-34',
          '35-39', '40-44', '45-49', '50+'))

# -------------------------------------
# MISSINGNESS CHECKS
# -------------------------------------

# key variables for study eligibility determination
# should always be 0 rows 
dt[is.na(pid)]
dt[is.na(ahd_dt)]
dt[is.na(siteid)]
dt[is.na(ahd_elig)]
dt[is.na(dob)]

# -------------------------------------
# OUTPUT THE DATA SETS AND APPEND
# -------------------------------------

# -----------------
# add a variable distinguishing baseline from endline
if (period=='baseline') dt[ , period:='b']
if (period=='endline') dt[ , period:='e']

# output baseline data 
if (period=='baseline') {
write.csv(dt, paste0(prepDir, 'baseline_main_to_append.csv'))
  saveRDS(dt, paste0(prepDir, 'baseline_main_to_append.rds'))
} else {
  write.csv(dt, paste0(prepDir, 'endline_main_to_append.csv'))
  saveRDS(dt, paste0(prepDir, 'endline_main_to_append.rds'))
}

# -------------------------------------
# append the data sets to create a master data set

if (append==T) {
  
  # read in the data sets
  base = readRDS(paste0(prepDir, 'baseline_main_to_append.rds'))
  end = readRDS(paste0(prepDir, 'endline_main_to_append.rds'))
  
  # rbind them together
  full_data = rbind(base, end)
  
  # run some quick checks
  
  # save the final data set
  saveRDS(full_data, paste0(prepDir, 'full_data.RDS'))
  write.csv(full_data, paste0(prepDir, 'full_data.csv')) }

# -------------------------------------

# -------------------------------------
# LE FIN
# -------------------------------------





