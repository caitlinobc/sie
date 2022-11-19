#---------------------
# Tableau prep code for both data sets
# Exports country-specific and pooled data 
# Caitlin O'Brien-Carelli
# 11/1/222

#---------------------
# load the packages

rm(list=ls())
library(data.table)
library(stringr)
library(dplyr)
library(openxlsx)
#---------------------

#-----------------------------------
# read in the files

# set the working directories
mwiDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/malawi/prepped/'
mwiOutDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/malawi/prepped/'


# read in malawi and tanzania prepped data 
mdt = readRDS(paste0(mwiDir, 'full_data.RDS')) #malawi

#---------------------------------------------------
# FORMAT A DATA SET SPECIFIC TO TABLEAU
#---------------------------------------------------

#--------------------------------------
# MALAWI 
#--------------------------------------

#----------------------------------- 
# Format the values for visualization 
#----------------------------------- 

#------------------------
# sociodemographic variables
#------------------------
# change patient id to a numeric, replace bs
mdt[ ,pid:=as.numeric(gsub('b', '2', pid))]

# format period
mdt[period=='b', period:='Baseline']
mdt[period=='e', period:='Endline']

# format under 5 as a numeric (not logical)
mdt[ , under5:=as.numeric(under5)]

# change the names of the variables to display 
setnames(mdt, c('siteid', 'site', 'pid', 'period', 
           'sex', 'dob', 'age', 'age_cat', 'under5'), 
    c('Site ID', 'Site', 'Patient ID', 'Cohort', 
      'Sex', 'DOB', 'Age', 'Age Category', '<5'))
    
# convert WHO stage to a label for graphs
mdt[ , whostage1st:=as.character(whostage1st)]
mdt[whostage1st==1, whostage1st:='Stage 1']
mdt[whostage1st==2, whostage1st:='Stage 2']
mdt[whostage1st==3, whostage1st:='Stage 3']
mdt[whostage1st==4, whostage1st:='Stage 4']

#------------------------
# logicals (Y/N)
#------------------------
#convert logical variables to numerics (works better in tableau)
mdt[ , knwstat:=as.numeric(knwstat)]
mdt[ , hivtest:=as.numeric(hivtest)]
mdt[ , hivresult:=as.numeric(hivresult)]

mdt[ , cd4done_after_ahdelig:=as.numeric(cd4done_after_ahdelig)]
mdt[ , whostage1_done:=as.numeric(whostage1_done)]

mdt[ , tbsympscrn:=as.numeric(tbsympscrn)]
mdt[ , tbsympscrn_result:=as.numeric(tbsympscrn_result)]
mdt[ , tptstart:=as.numeric(tptstart)]
mdt[ , tptcplt:=as.numeric(tptcplt)]

mdt[ , sstest:=as.numeric(sstest)]
mdt[ , ssresult:=as.numeric(ssresult)]
mdt[ , gxtest:=as.numeric(gxtest)]
mdt[ , gxresult:=as.numeric(gxresult)]

mdt[ , lamtest:=as.numeric(lamtest)]
mdt[ , lamresult:=as.numeric(lamresult)]
mdt[ , tbtx_start:=as.numeric(tbtx_start)]
mdt[ , tb_tx_cplt:=as.numeric(tb_tx_cplt)]

mdt[ , screenedfor_crypto:=as.numeric(screenedfor_crypto)]
mdt[ , crag_result:=as.numeric(crag_result)]
mdt[ , lumbar_referred:=as.numeric(lumbar_referred)]
mdt[ , lumbar_done:=as.numeric(lumbar_done)]
mdt[ , csf_cragperformed:=as.numeric(csf_cragperformed)]
mdt[ , csf_result:=as.numeric(csf_result)]
mdt[ , complete_cryptoindcuti2weeks:=as.numeric(complete_cryptoindcuti2weeks)]

mdt[ , everart:=as.numeric(everart)]
mdt[ , restarted_art:=as.numeric(restarted_art)]
mdt[ , art6m:=as.numeric(art6m)]
mdt[ , ahd_vl:=as.numeric(ahd_vl)]
mdt[ , suppressed:=as.numeric(suppressed)]


#------------------------
# rename the variables for display
#------------------------

setnames(mdt, c('ahd_dt', 'ahd_elig', 'knwstat', 'hivtest', 'dtpos', 'hivresult',
     'cd4done_after_ahdelig', 'cd4_after_ahdelig_dt',
     'cd4_afterahdelig_res_ret_dt', 'cd4_after_ahdelig_result', 
     'whostage1_done', 'whostage1st_dt', 'whostage1st',
     
     'tbsympscrn', 'tbsympscrn_dt', 'tbsympscrn_result', 
     'tptstart',  'tptstart_dt', 'tptcplt', 'tptcplt_dt',
     
     'sstest', 'sstest_dt', 'ssreturn_dt', 'ssresult',
     'gxtest', 'gxtest_dt', 'gxreturn_dt', 'gxresult',  
     'lamtest', 'lamtest_dt', 'lamreturn_dt', 'lamresult',
     'tbtx_start', 'tbtx_start_dt', 'tb_tx_cplt', 'tb_tx_cplt_dt',
     
     'screenedfor_crypto', 'crag_dt', 'crag_result_dt', 'crag_result',
     'lumbar_referred', 'lumbarreferred_dt', 'lumbar_done', 'lumbar_done_dt',
     'csf_cragperformed', 'csf_cragperformed_dt', 'csfcragresultsreturned_dt',
     'csf_result', 'crypto_regimen', 'crypto_regimen_start_dt',
     'complete_cryptoindcuti2weeks', 'complete_cryptoindcuti2weeks_dt',

     'everart', 'firstart_dt', 'restarted_art', 'art_restart_dt', 'art6m',
     'ahd_vl', 'ahd_vl_dt', 'ahd_vl_result', 'suppressed'
     ),

     c('AHD Diagnosis Date', 'Eligibility Stream', 'Knowledge of HIV Status',
       'HIV Test Completed', 'HIV Test Date', 'HIV Test Result',
       'CD4 Test Completed', 'CD4 Test Date',
       'CD4 Result Date', 'CD4 Test Result', 
       'Received WHO Staging', 'WHO Staging Date', 'WHO Stage',
       
       'Screened TB+', 'TB Screening Date', 'TB Screening Result', 
       'Started TPT',  'TPT Start Date', 'Completed TPT' , 'TPT Completion Date',
       
       'Sputum Smear Test', 'SS Test Date', 'SS Result Date', 'Sputum Smear Result',
       'GeneXpert Test', 'GeneXpert Test Date', 'GeneXpert Result Date', 'GeneXpert Result', 
        'LAM Test', 'LAM Test Date', 'LAM Result Date', 'LAM Result',
       'TB Tx Started', 'TB Tx Start Date', 'Completed TB Tx', 'TB Tx Completion Date',
       
       'CrAg Screened', 'CrAg Date', 'CrAg Result Date', 'CrAg Result',
       'Lumbar Referred', 'Lumber Referred Date', 'Lumbar Puncture Done',
       'Lumbar Puncture Done Date',
       'CSF Performed', 'CSF Date', 'CSF Result Date',
       'CSF Result', 'Crypto Regimen', 'Crypto Regimen Start Date',
       'Completed Crypto in <2 Weeks', 'Crypto Completion Date',
       
       'Started on ART', 'ART Start Date', 'Restarted ART', 'ART Restart Date',
       'On ART for 6+ Months','VL Test Performed', 'VL Test Date', 'VL Result', 
       'Virally Suppressed' ))


#----------------------------------- 
# export the data for Malawi 
# ensure missing values coded as blank spaces
write.csv(mdt, paste0(mwiOutDir, 'Malawi Tableau Data.csv'))

#----------------------------------- 

#--------------------------------------------------------

#--------------------------------------
# TANZANIA
#--------------------------------------



#------------------------
# POOL THE COUNTRIES
#------------------------

