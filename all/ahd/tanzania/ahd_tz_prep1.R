# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/11/2021
# Tanzania Baseline Cohort Data
# Import and clean the AHD study data
# Processes both the baseline and endline and appends
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

if (period=='baseline') {
# load the patient study eligibility and service provision data 
dt = data.table(read_xlsx(paste0(dir,
    'raw/Tanzania AHD_Baseline_28.10.xlsx'), 
     sheet = 'flat(leys_siteid,pid)', skip = 3))}

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
# additional date variables

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

dt[ ,unique( ahd_cd4u200)]

# -------------------------------------
# DATA QUALITY CHECKS
# -------------------------------------

# ------------------------
# check the sites and site ids against the study protocols
ids = dt[ ,.(siteid = unique(siteid)), by = .(dhisid, dhisname)]
write.csv(ids, paste0(outDir, 'facilities_in_data.csv'))

#-----------------
# check for duplicate patient ids and duplicate entries
dt[duplicated(dt)] # no duplicate entries
dt[ , pid_test:=.N, by = pid]
dt[1 < pid_test, .(pid, pid_test, dhisname, dob), by = pid]

# output a list of duplicate patient identifiers
write.csv(ids, paste0(outDir, 'duplicate_participant_ids.csv'))
dt[ , pid_test:=NULL]

#-----------------
# # check links with other data sets
# cd4[!(pid %in% dt$pid)] # all pids there, but one entry is a dup pid
# tb[!(pid %in% dt$pid), length(unique(pid))]

#-----------------
# check that patients eligible as under 5 are under 5
dt[age < 5, unique(ahd_elig)] # includes 5 and 1
dt[age < 5 & ahd_elig==1]

# check that study eligibility was after the patient was born
dt[ahd_dt < dob]

# when age is equal to 0, how old is the participant?
dt[age==0, .(dob, ahd_dt)]

# -------------------------------------
# MISSINGNESS



# -------------------------------------
# output the data set for processing 

# add a variable distinguishing baseline from endline
if (period=='baseline') dt[ , period:='Baseline']
if (period=='endline') dt[ , period:='Endline']

# output baseline data 
if (period=='baseline') {
write.csv(dt, paste0(prepDir, 'baseline_main.csv'))
  saveRDS(dt, paste0(prepDir, 'baseline_main.rds'))
} else {
  write.csv(dt, paste0(prepDir, 'endline_main.csv'))
  saveRDS(dt, paste0(prepDir, 'endline_main.rds'))
}

# -------------------------------------
# append the data sets to create a master data set

if (append==T) {
  
  
}

# -------------------------------------







