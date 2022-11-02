#---------------------
# Malawi AHD Initial Exploratory Analysis
# Prep the data for analysis and check data quality
# Preps the data for both cohorts simultaneously
# Caitlin O'Brien-Carelli
# 11/1/222

#---------------------
# load the packages

rm(list=ls())
library(data.table)
library(stringr)
library(dplyr)
library(openxlsx)
library(ggplot2)
#---------------------

#-----------------------------------
# read in the files

# set the working directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/malawi/raw/'

# set the directory for the prepped data
prepDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/malawi/prepped/'

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/malawi/outputs/'

# read in the master data file
# note missings are coded as "n/a"
dt = data.table(read.csv(paste0(dir, 'AHD_M&E_Valueslabels.csv'), na.strings = "n/a"))

#-----------------------------------

#-----------------------------------
# CLEAN THE DATA
#-----------------------------------

#---------------------
# drop unnecessary variables

# drop variables associated with identifying the data collector
drop_names = names(dt)[grepl('^X', names(dt))] # starts with X - not needed
               
dt[ , c('starttime', 'deviceid', 'capturedate', 'section1.SexFemale',
        'meta.instanceID', "studyid_calcu", "AHD_Diagnosisdate",
        "hiv_treatment.artStarddate",
        "hiv_treatment.artStarddate2", # binary for if start date is missing
        drop_names):=NULL]
#---------------------

#---------------------
# de-duplication process

# check for repeat patient ids
# there are 8 repeat ids - none have the same DOB
# these are unlikely to be duplicate entries
dt[ , pt_count:=.N, by = participant_number]

# export a table of the duplicate patients 
dups = dt[pt_count==2, participant_number, 
   by = section1.dob][order(participant_number)]
setnames(dups, c('dob', 'pid'))
write.csv(dups, paste0(outDir, 'tables/duplicate_patient_ids.csv'))

# create alternate IDs with no duplicates by adding a 'b' to the 2nd entry
dt[pt_count==2, rank:=seq(1:2), by = participant_number]
dt[ , participant_number:=as.character(participant_number)]
dt[pt_count==2 & rank==2, participant_number:=paste0(participant_number, 'b')]
dt[ ,c('pt_count', 'rank'):=NULL]
#---------------------
#-----------------------------------
# RENAME THE VARIABLES 
#-----------------------------------

#---------------------
# change the names to match the names in the tanzania data 
setnames(dt, c( "participant_number", "prepostrecord", "section1.dob", "section1.Sex", "StudySite",           
     "AHD_Diagnosisdate1" , "peligibility", "section1.Patient_HIVknowledge", "section1.HIV_Tested",
      "section1.Date_tested", "section1.hivtestresult",   
     
     "section2.wascd4_done", "section2.wascd4_donedate", 
     "section2.wascd4_resultsreturneddate", "section2.CD4_counttestresult", 
     "section2.WHO_staged",  "section2.patientdatestaged","section2.patientWHOStage",
     
     "TB_screening.screenedforTB", "TB_screening.datescreenedforTB", "TB_screening.patiernttb_Symptomsscreenresult",  
     "TB_screening.waspatient_startedonTB", "TB_screening.datepatient_startedonTB",        
     "TB_screening.didpatientcompleteTBtherapy",  "TB_screening.date_tbtherapycompleted",
     "TB_screening.testedfo_TBvaiSputumSmear", "TB_screening.date_sputumCollected",     
     "TB_screening.date_sputumtestresultreturned", "TB_screening.sputSmeartestresult", 
     "TB_screening.wasGenXparttested",  "TB_screening.GenXparttesteddatecollected",         
     "TB_screening.Gxtest_resultsreturndate", "TB_screening.GXtest_testresult",
     "TB_screening.TestedTBViaLAM", "TB_screening.TBLAMcollectdate", 
     "TB_screening.LAMtestresults_ReturnDate", "TB_screening.LAMtestresults",  
     
     "tb_treatment.Start_TBTreatment", "tb_treatment.DateStart_TBTreatment",               
     "tb_treatment.complete_TBTreatment", "tb_treatment.dateTBtreat_Completed",         
     
     "Crypto_Screening.screenedfor_Crypto", "Crypto_Screening.CrAg_Sampletest_date",            
     "Crypto_Screening.CrAg_Sampleresult_date", "Crypto_Screening.CrAg_Sampletest_result", 
     "Crypto_Screening.referred_LumberPuncture","Crypto_Screening.date_Lumberreferred",
     "Crypto_Screening.LumberPucturedone",  "Crypto_Screening.DateLumberPucturedone",           
     "Crypto_Screening.CSF_CrAgperformed", "Crypto_Screening.Date_CSF_CrAgperformed", "Crypto_Screening.Date_CSFCrAgresultsreturned",     
     "Crypto_Screening.CSF_testresults", "crypto_treatment.crypo_regimen",  "crypto_treatment.Date_startedon_crypo_regimen",    
     "crypto_treatment.complete_cryptoindcuti2weeks", "crypto_treatment.datecomplete_cryptoindcuti2weeks",
     
     "hiv_treatment.startedonART", "hiv_treatment.art_startdate",
     "hiv_treatment.restartedonART", "hiv_treatment.art_startdate2", "hiv_treatment.on6monthsART",                       
     "hiv_treatment.was_VLtestPerformedlast6", "hiv_treatment.Vltestdate", "hiv_treatment.patientVLresult"), 
                       
    # new names for the variables  
    c("pid", "period", "dob", "sex", "siteid",  
      "ahd_dt", "ahd_elig", "knwstat", "hivtest", "dtpos", "hivresult",
       "cd4done_after_ahdelig", "cd4_after_ahdelig_dt",
       "cd4_afterahdelig_res_ret_dt", "cd4_after_ahdelig_result", 
       "whostage1_done", "whostage1st_dt", "whostage1st",
       "tbsympscrn", "tbsympscrn_dt", "tbsympscrn_result", 
       "tptstart",  "tptstart_dt", "tptcplt","tptcplt_dt",
       "sstest", "sstest_dt", "ssreturn_dt", "ssresult",
       "gxtest", "gxtest_dt","gxreturn_dt", "gxresult", 
       "lamtest", "lamtest_dt", "lamreturn_dt", "lamresult",
       "tbtx_start", "tbtx_start_dt", "tb_tx_cplt", "tb_tx_cplt_dt",
       
      "screenedfor_crypto", "crag_dt", "crag_result_dt", "crag_result", 
       "lumbar_referred", "lumbarreferred_dt", "lumbar_done", "lumbar_done_dt",           
        "csf_cragperformed", "csf_cragperformed_dt", "csfcragresultsreturned_dt",     
        "csf_result", "crypto_regimen", "crypto_regimen_start_dt",    
        "complete_cryptoindcuti2weeks", "complete_cryptoindcuti2weeks_dt",
        
      "everart", "firstart_dt", "restarted_art", "art_restart_dt", "art6m",
        "ahd_vl", "ahd_vl_dt", "ahd_vl_result"
      
      ))  
                
#---------------------
                                                                                                          
#-----------------------------------
# FORMAT THE DATA FOR ANALYSIS
#-----------------------------------        

#---------------------
# format the period variable and label
dt[ , period:=as.character(period)]
dt[period=='1', period:='b']
dt[period=='2', period:='e']
#---------------------

#---------------------
# convert dates to date type variables

# all date names include dt except dob and firstvis
dates = c(names(dt)[ grepl("dt", names(dt))], 'dob')

# convert the date variables to date type variables
date_byVars = names(dt)[!(names(dt) %in% dates)] # list of not dates
dt = dt[ , lapply(.SD, as.Date, '%m/%d/%Y'), by = date_byVars]

#---------------------

#---------------------
# calculate age
dt[ , age:=as.numeric(floor((ahd_dt - dob)/365))]
dt[age < 0, age:=0] # one mistaken dob (before ahd_elig)

# when ahd eligibility date is missing, sub in who staging date
# there are only three missing eligibility dates
dt[is.na(age), age:=as.numeric(floor((whostage1st_dt - dob)/365))]

# add a binary for under 5
dt[age < 5, under5:=TRUE]
dt[5 <= age, under5:=FALSE]
#---------------------

#---------------------
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
#---------------------

#---------------------
# format the sex variable
dt[ , sex:=as.character(sex)]
dt[sex=="1", sex:="Male"]
dt[sex=="2", sex:="Female"]
#---------------------

#---------------------
# label the site names based on the data dictionary
dt[siteid==1, site:='Bangwe Health Centre']
dt[siteid==2, site:='Chileka Health Centre']
dt[siteid==3, site:='Domasi Rural Hospital']
dt[siteid==4, site:='Malamulo Rural Hospital']

dt[siteid==5, site:='Mpemba Health Centre']
dt[siteid==6, site:='Mwanza District Hospital']
dt[siteid==7, site:='St. Lukes Mission Hospital']
dt[siteid==8, site:='Thyolo District Hospital']
dt[siteid==9, site:='Police College Clinic']
#---------------------

#-----------------------------------
# CHECK AND FORMAT DATA TYPES 
#-----------------------------------  

#---------------------
# format YN variables as logicals 
# format the variables in which 3 is coded as missing
#---------------------
# variables that are mysteriously coded correctly
# one data entry person used 3 for missing, one left the field blank/NA
# ssreslt, gxresult, lamresult, crag_result
# lumbar_done is always missing 
# crypto_regimen is categorical
#---------------------
#---------------------
# reformat the 3 value as missing
dt[knwstat==3, knwstat:=NA]
dt[hivtest==3, hivtest:=NA]
dt[hivresult==3, hivresult:=NA]
dt[cd4done_after_ahdelig==3, cd4done_after_ahdelig:=NA]
dt[whostage1_done==3, whostage1_done:=NA]

dt[tbsympscrn==3, tbsympscrn:=NA]
dt[tbsympscrn_result==3, tbsympscrn_result:=NA] # includes 1s, 0s, and 3s
dt[tptstart==3, tptstart:=NA]
dt[tptcplt==3, tptcplt:=NA]
dt[sstest==3, sstest:=NA]

dt[gxtest==3, gxtest:=NA]
dt[lamtest==3, lamtest:=NA]
dt[tbtx_start==3, tbtx_start:=NA]
dt[tb_tx_cplt==3, tb_tx_cplt:=NA]
dt[screenedfor_crypto==3, screenedfor_crypto:=NA]

dt[lumbar_referred ==3, lumbar_referred :=NA]
dt[csf_cragperformed ==3, csf_cragperformed:=NA]
dt[csf_result==3, csf_result:=NA]
dt[complete_cryptoindcuti2weeks==3, complete_cryptoindcuti2weeks:=NA]
dt[everart==3, everart:=NA]

dt[restarted_art==3, restarted_art:=NA]
dt[art6m==3, art6m:=NA]
dt[ahd_vl==3, ahd_vl:=NA]
      
#---------------------

#---------------------
# reformat the 2 value as 0s
dt[knwstat==2, knwstat:=0]
dt[hivtest==2, hivtest:=0]
dt[hivresult==2, hivresult:=0]
dt[cd4done_after_ahdelig==2, cd4done_after_ahdelig:=0]
dt[whostage1_done==2, whostage1_done:=0]

dt[tbsympscrn==2, tbsympscrn:=0]
dt[tbsympscrn_result==2, tbsympscrn_result:=0] # includes 1s, 0s, and 2s
dt[tptstart==2, tptstart:=0]
dt[tptcplt==2, tptcplt:=0]
dt[sstest==2, sstest:=0]

dt[gxtest==2, gxtest:=0]
dt[lamtest==2, lamtest:=0]
dt[tbtx_start==2, tbtx_start:=0]
dt[tb_tx_cplt==2, tb_tx_cplt:=0]
dt[screenedfor_crypto==2, screenedfor_crypto:=0]

dt[lumbar_referred==2, lumbar_referred :=0]
dt[csf_cragperformed==2, csf_cragperformed:=0]
dt[csf_result==2, csf_result:=0]
dt[complete_cryptoindcuti2weeks==2, complete_cryptoindcuti2weeks:=0]
dt[everart==2, everart:=0]

dt[restarted_art==2, restarted_art:=0]
dt[art6m==2, art6m:=0]
dt[ahd_vl==2, ahd_vl:=0]

#---------------------

#---------------------
# 4 coded as inconclusive, treat as missing here
dt[hivresult==4, hivresult:=NA] 

# there is one patient who tested negative for tb on sept. 14
# they were coded as false for starting tpt with a start date of sept. 14
# e.g. they started tpt - recode as true
# and a completion date of the same day - remove completion date since it is incorrect
dt[tptstart==F & tptcplt==T, tptstart:=TRUE]
dt[tptstart==F & tptcplt==T, tptcplt_dt:=NA]
#---------------------

#---------------------
# reformat the 0,1 variables as logicals 
logi_vars = c('knwstat', 'hivtest', 'hivresult', 'cd4done_after_ahdelig', 'whostage1_done',
              'tbsympscrn', 'tbsympscrn_result', 'tptstart', 'tptcplt', 'sstest',
              'gxtest', 'lamtest', 'tbtx_start', 'tb_tx_cplt', 'screenedfor_crypto',
              'lumbar_referred', 'csf_cragperformed', 'complete_cryptoindcuti2weeks', 'everart',
              'restarted_art', 'art6m', 'ahd_vl',
              'ssresult', 'gxresult', 'lamresult', 'crag_result', 'csf_result',
              'lumbar_done')

# # check that the variables were properly coded
# test = dt[, .(pid = unique(pid)), by = logi_vars]
# test = melt(test, id.vars = 'pid')
# for (v in logi_vars) {
# print(v)
# print(table(test[variable==v]$value))}

logi_byVars = names(dt)[!(names(dt) %in% logi_vars)] # list of not dates
dt = dt[ , lapply(.SD, as.logical), by = logi_byVars]
#---------------------

#---------------------
# the viral load variable was hand entered with some nonsensical results

# one result is an actual sentence
dt[ahd_vl_result=="=5022 another sample collected on 2 July 2021 results not yet in", 
   ahd_vl_result:="5022"]

# recode typos and write-ins noting missing data as missing
dt[ahd_vl_result=="#NAME?", ahd_vl_result:=NA]
dt[ahd_vl_result=="results not yet in", ahd_vl_result:=NA]
dt[grepl("iss", ahd_vl_result), ahd_vl_result:=NA] # data collector wrote in missing
dt[grepl("avail", ahd_vl_result), ahd_vl_result:=NA] 

# create three categories: undetectable, treatment success, and detectable
# undetectable: <50 copies/ml or LDL 
# LDL - "lower than detectable limit" or <50 copies/ml (undetectable)

# recode LDL as undetectable
dt[grepl('d', tolower(ahd_vl_result)), ahd_vl_result:='Undetectable']

# if data collector wrote in <1000, treatment success (WHO)
dt[grepl('<1000', tolower(ahd_vl_result)), ahd_vl_result:='Success']

# use numeric values to determine treatment success
# select numeric values only (no values that include text)
dt[!(grepl('<', ahd_vl_result) | grepl('>', ahd_vl_result)) & ahd_vl_result!='Undetectable' & ahd_vl_result!='Success', 
   test:=as.numeric(ahd_vl_result)]
dt[grepl('<', ahd_vl_result), 
    test:=as.numeric(gsub('<', '', ahd_vl_result))]
dt[test < 50, ahd_vl_result:='Undetectable']
dt[test < 1000, ahd_vl_result:='Success']
dt[1000 <= test, ahd_vl_result:='Detectable']

# drop one entry in which the value is >906; unknown if suppressed
dt[ahd_vl_result=='>906', ahd_vl_result:=NA]

# every value with a greater than sign is above 1,000 - detectable
dt[grepl('>', ahd_vl_result), ahd_vl_result:='Detectable']

# add a binary for suppressed/unsuppressed
dt[ahd_vl_result=='Undetectable' | ahd_vl_result=='Success', suppressed:=TRUE]
dt[ahd_vl_result=='Detectable', suppressed:=FALSE]

# factor the variable for display
dt$ahd_vl_result = factor(dt$ahd_vl_result, 
            c('Undetectable', 'Success', 'Detectable'),
            c('Undetectable', 'Success', 'Detectable'))

# drop test (numeric created to code categorical variable)
dt[ ,test:=NULL]

#---------------------

#-----------------------------------
# ARRANGE VARIABLES IN PREFERRED ORDER
#-----------------------------------

#---------------------
dt = dt[ ,.(ahd_dt, ahd_elig, knwstat, hivtest, dtpos, hivresult,
cd4done_after_ahdelig, cd4_after_ahdelig_dt,
cd4_afterahdelig_res_ret_dt, cd4_after_ahdelig_result, 
whostage1_done, whostage1st_dt, whostage1st,
tbsympscrn, tbsympscrn_dt, tbsympscrn_result, 
tptstart,  tptstart_dt, tptcplt,tptcplt_dt,
sstest, sstest_dt, ssreturn_dt, ssresult,
gxtest, gxtest_dt,gxreturn_dt, gxresult, 
lamtest, lamtest_dt, lamreturn_dt, lamresult,
tbtx_start, tbtx_start_dt, tb_tx_cplt, tb_tx_cplt_dt,
screenedfor_crypto, crag_dt, crag_result_dt, crag_result, 
lumbar_referred, lumbarreferred_dt, lumbar_done, lumbar_done_dt,           
csf_cragperformed, csf_cragperformed_dt, csfcragresultsreturned_dt,     
csf_result, crypto_regimen, crypto_regimen_start_dt,    
complete_cryptoindcuti2weeks, complete_cryptoindcuti2weeks_dt,
everart, firstart_dt, restarted_art, art_restart_dt, art6m,
ahd_vl, ahd_vl_dt, ahd_vl_result, suppressed),
        by = .(siteid, site, pid, period, sex, dob, age, age_cat, under5)]
#---------------------

#----------------------------------- 
#  SAVE THE FINAL, PREPPED DATA SET
saveRDS(dt, paste0(prepDir, 'full_data.RDS'))
write.csv(dt, paste0(prepDir, 'full_data.csv'))
#----------------------------------- 

#----------------------------------- 
# END
#----------------------------------- 

