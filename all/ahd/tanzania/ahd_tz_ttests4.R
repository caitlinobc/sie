# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/14/2021
# Tanzania Baseline Cohort Data
# Run a series of t-tests ok key outcome variables
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

# ----------------------------------------------
# T-TESTS ON BINARY OUTCOMES: TANZANIA
# ----------------------------------------------

# ------------------------
# was who staging completed?
who = dt[, whostage1_done, by = period]
who[is.na(whostage1_done), whostage1_done:=FALSE]
t.test(who[period=='b']$whostage1_done, who[period=='e']$whostage1_done, var.eqal = FALSE)
# ------------------------

# ------------------------
# was cd4 testing completed on or after the ahd diagnosis?
cd4 = dt[, .(cd4done_after_ahdelig), by = period]
cd4[is.na(cd4done_after_ahdelig), cd4done_after_ahdelig:=FALSE]
t.test(cd4[period=='b']$cd4done_after_ahdelig, cd4[period=='e']$cd4done_after_ahdelig)
# ------------------------

# ------------------------
# was the patient screened for signs and symptoms of tb?
tbs = dt[, .(tbsympscrn), by = period]
t.test(tbs[period=='b']$tbsympscrn, tbs[period=='e']$tbsympscrn)
# ------------------------

# ------------------------
# was the patient started on tb preventive therapy?
tpt = dt[, .(tptstart, tptalready), by = period]
tpt[tptstart==T | tptalready==T, tpt_ever:=TRUE]
tpt[is.na(tpt_ever), tpt_ever:=FALSE]
t.test(tpt[period=='b']$tpt_ever, tpt[period=='e']$tpt_ever)
# ------------------------

# ------------------------
# did the patient complete a regimen of tb preventive therapy?
tptc = dt[, .(tptcplt), by = period]
t.test(tptc[period=='b']$tptcplt, tptc[period=='e']$tptcplt)
# ------------------------



logicals = c('knwstat', 'hivtest', 'hivresult', 'cd4done_after_ahdelig',
             'whostage1_done', 'tbsympscrn', 'tptstart', 'tptalready', 'tptcplt',
             'ahd_cd4u200', 'sstest', 'sspos', 'gxtest', 'tbtxstart', 
             'everart', 'art6m', 'hvl6m', 'ahd_newcd4', 'ahd_newwho')


t.test(dt[period=='baseline']$whostage1_done, dt[period=='endline']$whostage1_done)
t.test(dt[period=='baseline']$cd4done_after_ahdelig, dt[period=='endline']$cd4done_after_ahdelig)
t.test(dt[period=='baseline']$tbsympscrn, dt[period=='endline']$tbsympscrn)


#--------------------------
# MALAWI

dt = dt[ ,.(id, pre_post, who_staged, cd4_done)]
tbscrn = data.table(tbscrn = df$TB_screening.screenedforTB)

dt = cbind(dt, tbscrn = tbscrn$tbscrn)

dt[who_staged==1, who_staged:=TRUE]
dt[who_staged==2, who_staged:=FALSE]
dt[who_staged==3, who_staged:=NA]
dt[who_staged=='n/a', who_staged:=NA]
dt[ ,who_staged:=as.logical(who_staged)]

dt[cd4_done==1, cd4_done:=TRUE]
dt[cd4_done==2, cd4_done:=FALSE]
dt[cd4_done==3, cd4_done:=NA]
dt[ ,cd4_done:=as.logical(cd4_done)]


dt[tbscrn==1, tbscrn:=TRUE]
dt[tbscrn==2, tbscrn:=FALSE]
dt[tbscrn==3, tbscrn:=NA]
dt[ ,tbscrn:=as.logical(tbscrn)]

t.test(dt[pre_post=='Baseline']$who_staged, dt[pre_post=='Endline']$who_staged)
t.test(dt[pre_post=='Baseline']$cd4_done, dt[pre_post=='Endline']$cd4_done)
t.test(dt[pre_post=='Baseline']$tbscrn, dt[pre_post=='Endline']$tbscrn)






