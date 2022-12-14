# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/3/22
# Malawi data set
# Run some fun regressions
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
library(stargazer)
# ------------------------

# ------------------------
# files and directories

# set the working directory to the ahd data sets
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/malawi/'
setwd(dir)

# set the output directory for prepped data 
prepDir = paste0(dir, 'prepped/')

# set the output directory for tables
outDir = paste0(dir, 'outputs/')

# ------------------------
# import the data
dt = readRDS(paste0(prepDir, 'full_data.RDS'))
# ------------------------

# ----------------------------------------------
# FORMAT THE DATA FOR REGRESSION ANALYSIS
# ----------------------------------------------

# factor sex, with male as the reference group
dt$sex = factor(dt$sex, c('Male', 'Female'), c('Male', 'Female'))

# factor period, with baseline as the reference group
dt$period = factor(dt$period, c('b', 'e'), c('Baseline', 'Endline'))

# factor site, with the highest volume site as the reference
dt$site = factor(dt$site, c('Thyolo District Hospital',
           'Mwanza District Hospital', 'Bangwe Health Centre',
           'Chileka Health Centre', 'Malamulo Rural Hospital',
           'St. Lukes Mission Hospital', 'Domasi Rural Hospital',
           'Police College Clinic', 'Mpemba Health Centre'),
c('Thyolo District Hospital',
       'Mwanza District Hospital', 'Bangwe Health Centre',
         'Chileka Health Centre', 'Malamulo Rural Hospital',
             'St. Lukes Mission Hospital', 'Domasi Rural Hospital',
             'Police College Clinic', 'Mpemba Health Centre'))

# ----------------------------------------------
# REGRESSIONS: TUBERCULOSIS
# ----------------------------------------------

# ---------------------------------
# regression: screening for signs and symptoms of TB

# --------------------
# subset to a small data set on screening
tb_dt = dt[ ,.(tbsympscrn) , by =.(sex, age_cat, period, site)]

# --------------------
# run a series of binomial models

# tb screening
tb_model = glm(formula = tbsympscrn~period+sex+age_cat+site, family = "binomial", data = tb_dt)
summary(tb_model) # selected model

tb_model2 = glm(formula = tbsympscrn~period+sex, family = "binomial", data = tb_dt)
summary(tb_model2)

tb_model3 = glm(formula = tbsympscrn~period+age_cat, family = "binomial", data = tb_dt)
summary(tb_model3)

tb_model4 = glm(formula = tbsympscrn~period+sex+site, family = "binomial", data = tb_dt)
summary(tb_model4)

# ---------------------------------
# regression: started and completed tb preventive therapy

# --------------------
# subset to a small data set on screening
tpt_dt = dt[ ,.(tptstart, tptcplt) , by =.(sex, age_cat, period, site)]

# --------------------
# run a series of binomial models

# starting tb preventive therapy
tpts_model = glm(formula = tptstart~period+sex+age_cat+site, family = "binomial", data = tpt_dt)
summary(tpts_model) # selected model

# completing tb preventive therapy
tptc_model = glm(formula = tptcplt~period+sex+age_cat+site, family = "binomial", data = tpt_dt)
summary(tptc_model)

tptc_model2 = glm(formula = tptcplt~period+sex+age_cat, family = "binomial", data = tpt_dt)
summary(tptc_model2) # selected model - small sample size

# ---------------------------------
# print the output - TB regressions

stargazer(tb_model, tpts_model, tptc_model2,
          title = 'TB Screening & TB Preventive Therapy', 
          align=T, type = 'text', no.space = TRUE, omit.stat = c("LL","ser","f"),
          dep.var.labels = c("Screened for TB", 'Started TPT', 'Completed TPT'),
          covariate.labels = c("Endline Cohort",
                               "Female", '5-9',
                               '10-14', '15-19', '20-24', '25-29', '30-34',
                               '35-39', '40-44', '45-49', '50+',
                               'Mwanza District Hospital', 'Bangwe Health Centre',
                               'Chileka Health Centre', 'Malamulo Rural Hospital',
                               'St. Lukes Mission Hospital', 'Domasi Rural Hospital',
                               'Police College Clinic', 'Mpemba Health Centre'),
          out = paste0(outDir, 'tb_regressions_mwi.txt'),
          single.row = TRUE)

# ---------------------------------

# ----------------------------------------------
# REGRESSIONS: VIRAL SUPPRESSION
# ----------------------------------------------

# --------------------
# subset to the viral suppression data 
v_dt = dt[ ,.(ahd_vl, suppressed) , by =.(sex, age_cat, period, site)]

# --------------------
# run a series of binomial models - received a viral load test
vl_model = glm(formula = ahd_vl~period+sex+age_cat+site, family = "binomial", data = v_dt)
summary(vl_model)

vl_model2 = glm(formula = ahd_vl~period+sex, family = "binomial", data = v_dt)
summary(vl_model2)

vl_model3 = glm(formula = ahd_vl~period+sex+site, family = "binomial", data = v_dt)
summary(vl_model3) # model of interest, age makes categories too small

# --------------------
# run a series of binomial models - viral suppression
vs_model = glm(formula = suppressed~period+sex+age_cat+site, family = "binomial", data = v_dt)
summary(vs_model)

vs_model2 = glm(formula = suppressed~period+sex+age_cat, family = "binomial", data = v_dt)
summary(vs_model2) # preferred model, no site was significant in first model

# ---------------------------------

# ---------------------------------
# print the output
stargazer(vl_model, vs_model2, 
          title = 'Viral Load Testing & Viral Suppression', 
          align=T, type = 'text', no.space = TRUE, omit.stat = c("LL","ser","f"),
          dep.var.labels = c("Received a VL Test", 'Virally Suppressed'),
          covariate.labels = c("Endline Cohort",
           "Female", '5-9',
             '10-14', '15-19', '20-24', '25-29', '30-34',
               '35-39', '40-44', '45-49', '50+',
           'Mwanza District Hospital', 'Bangwe Health Centre',
           'Chileka Health Centre', 'Malamulo Rural Hospital',
           'St. Lukes Mission Hospital', 'Domasi Rural Hospital',
           'Police College Clinic', 'Mpemba Health Centre'),
          out = paste0(outDir, 'viral_load_regressions_mwi.txt'),
          single.row = TRUE)

# ---------------------------------


