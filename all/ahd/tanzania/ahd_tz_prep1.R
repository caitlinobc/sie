# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/25/2021
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

period = 'endline'

# append the data sets? T/F
append = TRUE

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
write.csv(dt[pid %in% dup_ids],paste0(prepDir, 'duplicate_ids/', period, '_duplicate_patient_ids.csv'))

# numbers the entries by date within each pid group
dup$ord = ave(dup$count, dup$pid, FUN = seq_along)

# drop the entry for the same pid, dob, ahd_dt (keep later service delivery)
if (period=='baseline') dt = dt[!(pid=='01-04-0100-009528' & siteid==14216)]
if (period=='baseline') dup = dup[pid!='01-04-0100-009528']

# create a unique identifier based on pid and ahd_dt
dup[ ,c('dob', 'count'):=NULL]
dt = merge(dt, dup, by = c('pid', 'ahd_dt'), all.x = T)

# add a b to the same pids with distinct dobs; get rid of order
if (period=='baseline') {dt[(pid=='03-04-0100-009460' | pid=='03-04-0300-000140') & ord==2, pid:=paste0(pid, 'b')]
} else {dt[pid=='02-03-0100-006660' & ord==2, pid:=paste0(pid, 'b')]} # endline duplicate

dt[(pid=='03-04-0100-009460' | pid=='03-04-0300-000140' | pid=='02-03-0100-006660'), ord:=NA]

# drop the earlier entry of the duplicates
dt = dt[is.na(ord) | ord==2]

#------------------------
# re-check the count and drop unnecessary variables
dt[duplicated(pid)] # should be empty
dt[ , c('count', 'ord'):=NULL]

# ------------------------

# -------------------------------------
# merge in the sex data 

#add the bs to the ids from baseline
if (period=='baseline') {
dt_sex[pid=='03-04-0100-009460' & siteid==2569, pid:=paste0(pid, 'b')]
dt_sex[pid=='03-04-0300-000140' & siteid==2569, pid:=paste0(pid, 'b')]
} else {dt_sex[pid=='02-03-0100-006660' & siteid==2657, pid:=paste0(pid, 'b')]}

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

# the following variable appears as POSIX CT but is an import error
setnames(dt, 'ahd_whostage_incwho', 'ahd_whostage_incwho1' )
dt[grepl(3, ahd_whostage_incwho1), ahd_whostage_incwho:=3]
dt[grepl(4, ahd_whostage_incwho1), ahd_whostage_incwho:=4]
dt[ , ahd_whostage_incwho1:=NULL] #after this, should be no more POSIXCT in data

# all date names include dt except dob and firstvis
dates = c(names(dt)[ grepl("dt", names(dt))], 'dob', 'firstvis')

# convert the date variables to date type variables
date_byVars = names(dt)[!(names(dt) %in% dates)] # list of not dates
dt = dt[ , lapply(.SD, as.Date), by = date_byVars]

# ------------------------

# ------------------------
# convert logicals to logicals from 0/1
logicals = c('knwstat', 'hivtest', 'hivresult', 'cd4done_after_ahdelig',
      'whostage1_done', 'tbsympscrn', 'tptstart', 'tptalready', 'tptcplt',
      'ahd_cd4u200', 'sstest', 'sspos', 'gxtest', 'tbtxstart', 
      'everart', 'art6m', 'hvl6m', 'ahd_newcd4', 'ahd_newwho')

TF_byVars = names(dt)[!(names(dt) %in% logicals)] # list of not logicals
dt = dt[ , lapply(.SD, as.logical), by = TF_byVars]

# ------------------------
# check against cd4 for new patients 
# recode as yes if a new patient received cd4 (stored as a distinct variable)
dt[is.na(cd4done_after_ahdelig) & ahd_newcd4==T, cd4done_after_ahdelig:=T]

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

# --------------------------
# create a variable called site that re-formats the names

dt$site = factor(dt$dhisname, c("Arusha Lutheran Medical Hospital",
            "Dodoma  Regional Referral Hospital", "KCMC  Referral Hospital at Zonal Level",
            "Kibong`oto TB Hospital", "Manyara  Regional Referral Hospital", 
            "Mawenzi  Regional Referral Hospital", "Mt. Meru  Regional Referral Hospital",   
            "Singida  Regional Referral Hospital"),
            c('ALMH', 'Dodoma RRH', 'KCMC RRH', 'Kibongoto TB Hospital', 'Manyara RRH',
              'Mawenzi RRH', 'Mt. Meru RRH', 'Singida RRH'))

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
# FIX CD4 
# -------------------------------------

# --------------------------
# check that cd4 results were obtained on or after ahd diagnosis
# 
# cd4 = dt[!is.na(cd4_after_ahdelig_result) | !is.na(ahd_cd4result) | !is.na(cd4_fvis) | !is.na(ahd_new_cd4result),
#          .(cd4_after_ahdelig_result, ahd_cd4result, cd4_fvis, ahd_new_cd4result,
#           ahd_dt, cd4_after_ahdelig_dt, ahd_cd4u200_dt, cd4_fvis_dt, ahd_new_cd4_dt)]
# 
# # check that cd4 after ahd eligibility is truly after
# cd4[ , after_ahd:=(cd4_after_ahdelig_dt - ahd_dt)]
# cd4[after_ahd <= 0] #all cd4 testing occurred on or after an ahd diagnosis
# 
# # check that first visit is truly after
# cd4[ , fvis_after_ahd:=(cd4_fvis_dt - ahd_dt)]
# # no first visit cd4 tests before ahd diagnosis in baseline, one in endline
# cd4[fvis_after_ahd < 0] 
# 
# # check that new patients who received cd4 received it after ahd diagnosis
# cd4[ , newcd4_afterahd:=(ahd_new_cd4_dt - ahd_dt)]
# cd4[newcd4_afterahd < 0] # 0 entries
# 
# # check that the eligibility stream 6 patients received cd4 after ahd diagnosis
# cd4[ ,  u200:=(ahd_cd4u200_dt - ahd_dt)]
# # 33 patients received u200 cd4 testing before diagnosis in baseline, 7 endline
# cd4[u200 < 0] 
# cd4[u200 < 0, ahd_cd4result:=NA ] # drop the earlier results
# --------------------------
# 
# # --------------------------
# if one is missing, are the others missing?
# dt[is.na(cd4_after_ahdelig_result) & !is.na(ahd_cd4result)]
# dt[is.na(cd4_after_ahdelig_result) & !is.na(ahd_cd4result) & !is.na(cd4_fvis)] # never has both
# dt[is.na(cd4_after_ahdelig_result) & !is.na(ahd_cd4result) & !is.na(ahd_new_cd4result)] # never has both
# 
# dt[is.na(cd4_after_ahdelig_result) & !is.na(cd4_fvis)] # adds 129 entries in baseline
# dt[is.na(cd4_after_ahdelig_result) & !is.na(cd4_fvis) & !is.na(ahd_new_cd4result)] # sometimes has both
# dt[is.na(cd4_after_ahdelig_result) & cd4_fvis!=ahd_new_cd4result] # but the values are always equal
# 
# dt[is.na(cd4_after_ahdelig_result) & !is.na(ahd_new_cd4result)] # adds 129 entries in baseline

# --------------------------
# alter the actual data to maximize cd4 results

# if missing ahd cd4 and cd4 under 200 was performed before ahd diagnosis, drop under 200
# if missing ahd cd4 and first visit was before ahd diagnosis, drop first visit c4 (one entry)
# all other results are on or after ahd diagnosis 
dt[  , u200:=(ahd_cd4u200_dt - ahd_dt)] # drops 32 baseline results
dt[u200 < 0, ahd_cd4result:=NA ]

dt[ , fvis_after_ahd:=(cd4_fvis_dt - ahd_dt)]
dt[fvis_after_ahd < 0, cd4_fvis:=NA] 

# when cd4 after ahd eligibility is missing, replace with cd4 under 200
dt[is.na(cd4_after_ahdelig_result), cd4_after_ahdelig_result:=ahd_cd4result]
dt[is.na(cd4_after_ahdelig_result), cd4_after_ahdelig_dt:=ahd_cd4u200_dt]

# when cd4 after ahd eligibility is missing, replace first visit cd4
dt[is.na(cd4_after_ahdelig_result) & !is.na(cd4_fvis), cd4_after_ahdelig_result:=cd4_fvis]
dt[is.na(cd4_after_ahdelig_result) & !is.na(cd4_fvis), cd4_after_ahdelig_dt:=cd4_fvis_dt]

# one cd4 result has a decimal place ...
dt[cd4_after_ahdelig_result==49.5, cd4_after_ahdelig_result:=NA]

# change cd4 done binary to reflect the added results
dt[!is.na(cd4_after_ahdelig_result) , cd4done_after_ahdelig:=TRUE]
dt[is.na(cd4_after_ahdelig_result) , cd4done_after_ahdelig:=FALSE]

# --------------------------

# --------------------------
# drop excess variables
dt[ ,c('u200', 'fvis_after_ahd'):=NULL]

# --------------------------

# -------------------------------------
# OUTPUT THE DATA SETS AND APPEND
# -------------------------------------

# -----------------
# add a variable distinguishing baseline from endline
if (period=='baseline') dt[ , period:='b']
if (period=='endline') dt[ , period:='e']

# output baseline data 
if (period=='baseline') { saveRDS(dt, paste0(prepDir, 'baseline_main_to_append.rds'))
} else {saveRDS(dt, paste0(prepDir, 'endline_main_to_append.rds'))}

# -------------------------------------
# append the data sets to create a master data set

if (append==T) {
  
  # read in the data sets
  base = readRDS(paste0(prepDir, 'baseline_main_to_append.rds'))
  end = readRDS(paste0(prepDir, 'endline_main_to_append.rds'))
  
  # rbind them together
  full_data = rbind(base, end)
  
  # drop the bs from the patient ids to check duplication across periods
  full_data[ , pid2:=pid]
  full_data[ , pid2:=gsub('b', '', pid2)]
  
  # run some quick checks
  full_data[duplicated(pid2)] # 154 duplicate ids, baseline to endline
  
  # --------------------------
  # DUPLICATE IDS ENDLINE TO BASELINE
  
  # count the total number of duplicate id rows
  full_data[ , id_count:=.N, by = pid2]
  full_data[ ,unique(id_count)]
  
  # export the duplicate ids across intervention periods
  write.csv(full_data[1 < id_count], paste0(prepDir, 'duplicate_ids/full_data_duplicate_patient_ids.csv')) 
  
  # create a data set of duplicates and examine
  # count the number of entries with same pid, dob
  full_data[ , id_dob:=paste0(pid, dob)]
  full_data[ , id_dob_count:=.N, by = id_dob]
  
  # check that there is one entry per period
  full_data[id_dob_count==2, pers:=length(unique(period)), by = id_dob]
  full_data[id_dob_count==2, unique(pers)] # all are two
  
  # for entries with the same pid and dob keep baseline only
  full_data = full_data[!(id_dob_count==2 & period=='e')]
  
  # there is one entry with the same id, but different dob, in each period
  full_data[pid=='02-03-0200-005511' & siteid=='14216', pid:='02-03-0200-005511b']
  full_data = full_data[!(pid=='03-04-0100-009460' & period=='e')]
  
  # drop excess variables
  full_data[ , c('pid2', 'id_count', 'id_dob', 'id_dob_count', 'pers'):=NULL]
  
  # test for duplicates
  full_data[duplicated(pid)] # should be 0!
  
  # --------------------------
  
  # --------------------------
  # fix the cd4 testing variable
  
  
  # --------------------------
  
  # rearrange the data set in the order that you prefer
  # data set starts with demographic variables, meta data
  
   full_data = full_data[ , c("pid", "period", "dob", "age", "age_cat", "under5",  "sex",     
   "siteid", "dhisid", "dhisname", "site",  "district", "region",
    "ahd_dt", "ahd_elig", "knwstat", "hivtest","dtpos", "hivresult",
    "cd4done_after_ahdelig", "cd4_after_ahdelig_dt","cd4_after_ahdelig_result", 
    "whostage1_done", "whostage1st_dt", "whostage1st",             
    "tbsympscrn","tptstart", "tptalready", "tptstart_dt", "tptalready_dt",           
    "tptcplt","tptcplt_dt", "tptcplt_impute", "sstest", "sstest_dt",               
    "sspos","gxtest", "gxtest_dt","gxpos", "everart",                 
    "firstart_dt","art6m", "hvl6m", "hvl6m_dt",                
    "hvl6mcat", "hvl6mresult",  "tbtxstart", "tbtxalready", 
    "tbtxstart_dt", "tbtxalready_dt", 
    "tbtxcplt","tbtxcplt_dt", "firstvis", "whostage_fvis",  
    "transferinid", "cd4_fvis_dt","cd4_fvis", "ahd_u5", "ahd_u5_dt",  
    "ahd_newcd4","ahd_new_cd4_dt", "ahd_new_cd4result",  "ahd_newwho",
    "ahd_new_who_dt","ahd_new_whostage",        
    "ahd_oclin","ahd_oclin_who_dt", "ahd_oclin_whostage", 
    "ahd_tb", "ahd_tb_dt",  "ahd_incwho", "ahd_incwho_dt", "ahd_whostage_incwho", 
    "ahd_hvl", "ahd_hvl_dt", "ahd_hvlresult","ahd_hvlcat", "ahd_cd4u200",
    "ahd_cd4u200_dt","ahd_cd4result")]       
                        
  # --------------------------
  #  SAVE THE FINAL, PREPPED DATA SET
  saveRDS(full_data, paste0(prepDir, 'full_data.RDS'))
  write.csv(full_data, paste0(prepDir, 'full_data.csv')) }

# -------------------------------------

# -------------------------------------
# format the names of the variables for Tableau

# create a copy of the data
tab_full = copy(full_data)

# format the variables you will visualize
tab_full[period=='b', period:='Baseline']
tab_full[period=='e', period:='Endline']

# convert WHO stage
tab_full[whostage1st==1, who_stage:='Stage 1']
tab_full[whostage1st==2, who_stage:='Stage 2']
tab_full[whostage1st==3, who_stage:='Stage 3']
tab_full[whostage1st==4, who_stage:='Stage 4']

# convert key variables to numerics
tab_full[ ,knwstat:=as.numeric(knwstat)]
tab_full[ ,cd4done_after_ahdelig:=as.numeric(cd4done_after_ahdelig)]
tab_full[ ,whostage1_done:=as.numeric(whostage1_done)]
tab_full[ ,tbsympscrn:=as.numeric(tbsympscrn)]
tab_full[ ,tptstart:=as.numeric(tptstart)]

tab_full[ ,tptalready:=as.numeric(tptalready)]
tab_full[ ,sstest:=as.numeric(sstest)]
tab_full[ ,sspos:=as.numeric(sspos)]
tab_full[ ,gxtest:=as.numeric(gxtest)]
tab_full[ ,gxpos:=as.numeric(gxpos)]

# rename a few of the variables for Tableau  
setnames(tab_full, c("Patients", "Period", "DOB", "Age", "Age Category", "Under 5",  "Sex",     
 "Site ID", "DHIS2 ID", "DHIS2 Site", "Site",  "District", "Region",
     "ahd_dt", "ahd_elig", "knwstat", "hivtest","dtpos", "hivresult",
         "cd4done_after_ahdelig", "cd4_after_ahdelig_dt","cd4_after_ahdelig_result", 
               "whostage1_done", "whostage1st_dt", "whostage1st",             
                   "tbsympscrn","tptstart", "tptalready", "tptstart_dt", "tptalready_dt",           
                      "tptcplt","tptcplt_dt", "tptcplt_impute", "sstest", "sstest_dt",               
                         "sspos","gxtest", "gxtest_dt","gxpos", "everart",                 
                             "firstart_dt","art6m", "hvl6m", "hvl6m_dt",                
                                "hvl6mcat", "hvl6mresult",  "tbtxstart", "tbtxalready", 
                                   "tbtxstart_dt", "tbtxalready_dt", 
  "tbtxcplt","tbtxcplt_dt", "firstvis", "whostage_fvis",  
        "transferinid", "cd4_fvis_dt","cd4_fvis", "ahd_u5", "ahd_u5_dt",  
           "ahd_newcd4","ahd_new_cd4_dt", "ahd_new_cd4result",  "ahd_newwho",
               "ahd_new_who_dt","ahd_new_whostage",        
                   "ahd_oclin","ahd_oclin_who_dt", "ahd_oclin_whostage", 
                        "ahd_tb", "ahd_tb_dt",  "ahd_incwho", "ahd_incwho_dt", "ahd_whostage_incwho", 
                             "ahd_hvl", "ahd_hvl_dt", "ahd_hvlresult","ahd_hvlcat", "ahd_cd4u200",
                                "ahd_cd4u200_dt","ahd_cd4result", "WHO Stage"))


write.csv(tab_full, paste0(prepDir, 'tz_tableau_data.csv'))
# -------------------------------------


# -------------------------------------
# LE FIN
# -------------------------------------





