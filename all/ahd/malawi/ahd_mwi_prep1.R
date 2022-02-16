#---------------------
# Malawi AHD Initial Exploratory Analysis
# Caitlin O'Brien-Carelli
# 2/15/21
# Prep the Malawi data for both cohorts

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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/malawi/'

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Global/AHD/outputs/malawi/'

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
drop_names = names(dt)[grepl('^X', names(dt))]
               
dt[ , c('starttime', 'deviceid', 'capturedate', 'section1.SexFemale',
        'meta.instanceID', "studyid_calcu", "AHD_Diagnosisdate",
        "hiv_treatment.artStarddate","hiv_treatment.artStarddate2", # binary for if start date is missing
        drop_names):=NULL]

#---------------------

#---------------------
# check for repeat patient ids

# check for repeat patient ids
# there are 8 repeat ids - none have the same DOB
# these are unlikely to be duplicate entries
dt[ , pt_count:=.N, by = participant_number]
dt[pt_count==2][order(participant_number)]


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
     "AHD_Diagnosisdate1" , "peligibility", "section1.Patient_HIVknowledge",     "section1.HIV_Tested",
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
     "Crypto_Screening.CrAg_Sampleresult_date", "Crypto_Screening.CrAg_Sampletest_result", "Crypto_Screening.referred_LumberPuncture",         
     "Crypto_Screening.date_Lumberreferred","Crypto_Screening.LumberPucturedone",  "Crypto_Screening.DateLumberPucturedone",           
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
       "referred_lumbarpuncture", "lumbarreferred_dt", "lumbardone", "lumbar_dt",           
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
dt[siteid==7, site:='St. Lukes   Mission Hospital']
dt[siteid==8, site:='Thyolo District Hospital']
dt[siteid==9, site:='Police College Clinic']
#---------------------

#-----------------------------------
# CHECK AND FORMAT DATA TYPES 
#-----------------------------------  

#---------------------
# format YN variables as logicals 
# format the variables in which 3 is coded as missing
dt[knwstat==3, knwstat:=NA]
dt[knwstat==2, knwstat:=0]
dt[ , knwstat:=as.logical(knwstat)]

dt[hivtest==3, hivtest:=NA]
dt[hivtest==2, hivtest:=0]
dt[ , hivtest:=as.logical(hivtest)]

dt[hivresult==3, hivresult:=NA]
dt[hivresult==4, hivresult:=NA] # 4 coded as inconclusive, treat as missing here
dt[hivresult==2, hivresult:=0]
dt[ , hivresult:=as.logical(hivresult)]




dt[, unique(hivresult)]




