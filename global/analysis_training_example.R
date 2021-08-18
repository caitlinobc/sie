# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/12/21
# Example of a simple linear regression
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggpubr)
library(stargazer)
# --------------------
# Files and directories

# set the home directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Global/Analysis Training/'
setwd(dir)

# read in the data 
dt = data.table(read.csv(paste0(dir, 'datim_regression_example.csv')))
# --------------------

# ----------------------------------------------
# run some regressions

# --------------------
# format the data set
setnames(dt, 'ï..country', 'country')

# factor fiscal quarter so you can use as a categorical control
dt$fiscal_quarter = factor(dt$fiscal_quarter,
      c('FY21 Q1', 'FY21 Q2'), c('FY21 Q1', 'FY21 Q2'))

# factor country and sex
dt$country = factor(dt$country, c('Cameroon', 'Eswatini'), c('Cameroon', 'Eswatini'))
dt$sex = factor(dt$sex, c('Female', 'Male'), c('Female', 'Male'))

# create a summed up data set for all data 
dt_sum = dt[ ,.(tx_curr = sum(tx_curr), tx_pvls_d = sum(tx_pvls_d)),
                by = .(site, country, region, district)]

# --------------------
# simple linear regression - pooled relationship

model1 = lm(tx_pvls_d ~ tx_curr, data=dt_sum)
summary(model1)

# output the pooled model (only two variables)
stargazer(model1, type = 'text', title = 'Relationship between TX CURR and TX PVLS (D)',
          out = paste0(dir, 'model1_pooled.doc'))

# --------------------
# multiple linear regression - pooled relationship

model2 = lm(tx_pvls_d ~ tx_curr + country + sex + fiscal_quarter, data=dt)
summary(model2)

# output the pooled model (only two variables)
stargazer(model2, type = 'text', title = 'Relationship between TX CURR and TX PVLS (D)',
          out = paste0(dir, 'model2.doc'))








