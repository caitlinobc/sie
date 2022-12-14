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
          out = paste0(outDir, 'tb_regressions.txt'),
          single.row = TRUE)

# ---------------------------------

# ----------------------------------------------
# REGRESSIONS: VIRAL SUPPRESSION
# ----------------------------------------------

# --------------------
# subset to a small data set on viral suppression 
s_dt = dt[!is.na(suppressed) ,.(suppressed) , by =.(sex, age_cat, period, site)]

# --------------------
# run a series of binomial models
s_model = glm(formula = suppressed~sex+age_cat+period, family = "binomial", data = s_dt)
summary(s_model)

s_model2 = glm(formula = suppressed~sex+period, family = "binomial", data = s_dt)
summary(s_model2)

s_model3 = glm(formula = suppressed~age_cat+period, family = "binomial", data = s_dt)
summary(s_model3)

s_model4 = glm(formula = suppressed~period+sex+age_cat+site, family = "binomial", data = s_dt)
summary(s_model4) # final model of interest

# --------------------
# ---------------------------------

# ---------------------------------
# print the output
stargazer(tb_model4, s_model4, 
          title = 'Screening for Signs and Symptoms of TB & Viral Suppression', 
          align=T, type = 'text', no.space = TRUE, omit.stat = c("LL","ser","f"),
          dep.var.labels = c("Screened for TB", 'Virally Suppressed'),
          covariate.labels = c("Post-Intervention Cohort",
           "Female", '5-9',
             '10-14', '15-19', '20-24', '25-29', '30-34',
               '35-39', '40-44', '45-49', '50+',
           'Mwanza District Hospital', 'Bangwe Health Centre',
           'Chileka Health Centre', 'Malamulo Rural Hospital',
           'St. Lukes Mission Hospital', 'Domasi Rural Hospital',
           'Police College Clinic', 'Mpemba Health Centre'),
          out = paste0(outDir, 'sample_regressions.txt'),
          single.row = TRUE)

# ---------------------------------

# ---------------------------------
# regression: viral load testing

# --------------------
# subset to a small data set on viral suppression 
t_dt = dt[!is.na(ahd_vl) ,.(ahd_vl) , by =.(sex, age_cat, period, site)]

# --------------------
# run a series of binomial models
t_model = glm(formula = ahd_vl~period, family = "binomial", data = t_dt)
summary(t_model)

t_model1 = glm(formula = ahd_vl~period+age_cat, family = "binomial", data = t_dt)
summary(t_model1)

t_model2 = glm(formula = ahd_vl~sex+period, family = "binomial", data = t_dt)
summary(t_model2)


# print the output
stargazer(t_model1, 
          title = 'Received a Viral Load Test', 
          align=T, type = 'text', no.space = TRUE, omit.stat = c("LL","ser","f"),
          dep.var.labels = c("Received a VL Test"),
          covariate.labels = c("Post-Intervention Cohort",
                               '5-9',
                               '10-14', '15-19', '20-24', '25-29', '30-34',
                               '35-39', '40-44', '45-49', '50+'),
          out = paste0(outDir, 'sample_regressions2.txt'),
          single.row = TRUE)



