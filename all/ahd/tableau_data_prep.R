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
    
# convert WHO stage
mdt[whostage1st==1, who_stage:='Stage 1']
mdt[whostage1st==2, who_stage:='Stage 2']
mdt[whostage1st==3, who_stage:='Stage 3']
mdt[whostage1st==4, who_stage:='Stage 4']
mdt[ , whostage1st:=NULL]
setnames(mdt, 'who_stage', 'WHO Stage')

#------------------------
# logicals (Y/N)
#------------------------
# convert logical variables to numerics (works better in tableau)
mdt[ , knwstat:=as.numeric(knwstat)]
mdt[ , hivtest:=as.numeric(hivtest)]
mdt[ , hivresult:=as.numeric(hivresult)]

mdt[ , cd4done_after_ahdelig:=as.numeric(cd4done_after_ahdelig)]
mdt[ , whostage1_done:=as.numeric(whostage1_done)]

mdt[ , tbsympscrn:=as.numeric(tbsympscrn)]
mdt[ , tbsympscrn_result:=as.numeric(tbsympscrn_result)]
mdt[ , tptstart:=as.numeric(tptstart)]
mdt[ , tptcplt:=as.numeric(tptcplt)]

mdt[ ,sstest:=as.numeric(sstest)]
mdt[ ,ssresult:=as.numeric(ssresult)]
mdt[ ,gxtest:=as.numeric(gxtest)]
mdt[ ,gxresult:=as.numeric(gxresult)]

mdt[ ,lamtest:=as.numeric(lamtest)]
mdt[ ,lamresult:=as.numeric(lamresult)]
mdt[ ,tbtx_start:=as.numeric(tbtx_start)]










#----------------------------------- 
# export the data for Malawi 
write.csv(mdt, paste0(mwiOutDir, 'Malawi Tableau Data.csv'))
#----------------------------------- 

#--------------------------------------
# TANZANIA
#--------------------------------------



#------------------------
# POOL THE COUNTRIES
#------------------------

