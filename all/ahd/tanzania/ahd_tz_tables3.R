# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/21/2021
# Tanzania Baseline Cohort Data
# Create and export tables for analysis
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
# DEMOGRAPHIC & ELIGIBILITY TABLES 
# ----------------------------------------------
# create some demographic tables to show study eligibility

# ------------------------
# participants by sex, age category, period
dt_sa = dt[ ,.(value = length(unique(pid))), by =.(sex, age_cat, period)]
dt_sa = dcast(dt_sa, age_cat~period+sex)
dt_sa[ , b_Total:=(b_Female+b_Male)]
dt_sa[ , e_Total:=(e_Female+e_Male)]
dt_sa = dt_sa[ ,.(age_cat,b_Female, b_Male, b_Total, e_Female, e_Male, e_Total)]

# export the table
write.csv(dt_sa, paste0(outDir, 'participants_age_sex.csv'))
# ------------------------

# ------------------------
# participants by site, sex, period
dt_ss = dt[ ,.(value = length(unique(pid))), by =.(sex, dhisname, period)]
dt_ss = dcast(dt_ss, dhisname~period+sex)
dt_ss[ , b_Total:=(b_Female+b_Male)]
dt_ss[ , e_Total:=(e_Female+e_Male)]
dt_ss = dt_ss[ ,.(Site = dhisname,b_Female, b_Male, b_Total, e_Female, e_Male, e_Total)]

# export the table
write.csv(dt_ss, paste0(outDir, 'participants_site_sex.csv'))
# ------------------------

# ------------------------
# age distribution by site, period
dt_as = dt[ ,.(value = length(unique(pid))), by =.(age_cat, site, period)]
dt_as = dcast(dt_as, age_cat~period+site)

# export the table
write.csv(dt_as, paste0(outDir, 'participants_site_age.csv'))
# ------------------------

# ------------------------
# study eligibility by sex, period
dt_elig = dt[, .(value = length(unique(pid))) , by = .(ahd_elig, sex, period)]
dt_elig2 = dt[, .(value = length(unique(pid))) , by = .(ahd_elig, period)]
dt_elig2[ , sex:='Total']
dt_elig = rbind(dt_elig, dt_elig2)
dt_elig = dcast(dt_elig, ahd_elig~period+sex)

# export the table
write.csv(dt_elig, paste0(outDir, 'eligibilty_sex.csv'))
# ------------------------

# ----------------------------------------------
# OUTCOME TABLES: HIV TESTING/STATUS
# ----------------------------------------------

# ----------------------------------------------
# OUTCOME TABLES: TB SCREENING
# ----------------------------------------------
# no missing data in tb symptom screening

# ------------------------
# screened and not screened for tb by period
tb_scrn = dt[, .(scrn = sum(tbsympscrn), type = 'Screened'), by = period]
tb_scrn2 = dt[tbsympscrn==0, .(scrn = length(unique(pid)), type = 'Not screened'), by = period]
tb_scrn = rbind(tb_scrn, tb_scrn2)
tb_scrn = dcast(tb_scrn, type~period, value.var = 'scrn')
setnames(tb_scrn, c('Screened for TB', 'Pre-intervention', 'Post-intervention'))

# export the table
write.csv(tb_scrn, paste0(outDir, 'screened_for_tb.csv'))
# ------------------------

# ------------------------
# screened for tb by sex, age, period
tb_scrn_as = dt[, .(scrn = sum(tbsympscrn)), by = .(sex, age_cat, period)]
tb_scrn_as = dcast(tb_scrn_as, age_cat~period+sex, value.var = 'scrn')

# export the table
write.csv(tb_scrn_as, paste0(outDir, 'screened_for_tb_sex_age.csv'))

# ------------------------

# ------------------------
# screened and not screened for tb by sex, age, period
setnames(tb_scrn_as,c('age_cat', 'b_Female_s', 'b_Male_s', 
           'e_Female_s', 'e_Male_s'))

# table of not screened for tb
tb_ns_as = dt[tbsympscrn==0, .(scrn = length(unique(pid))), by = .(sex, age_cat, period)]
tb_ns_as = dcast(tb_ns_as, age_cat~period+sex, value.var = 'scrn')
tb_all = merge(tb_scrn_as, tb_ns_as, by = 'age_cat', all = T)

# rearrange variables in order of table - by sex, then screening status
tb_all = tb_all[ ,.(age_cat, b_Female_s, b_Female, b_Male_s, b_Male,
            e_Female_s, e_Female, e_Male_s, e_Male)]

# export the table
write.csv(tb_all, paste0(outDir, 'screened_ns_for_tb_sex_age.csv'))

# ------------------------





