# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/17/21
# Eswatini DATIM analysis - GYSI
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(openxlsx)
library(ggplot2)
library(RColorBrewer)
# --------------------

# ----------------------------------------------
# Files and directories

# set the home directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/eswatini/gysi/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Eswatini/GYSI/'

# read in the data 
dt = data.table(read.csv(paste0(dir, 'gysi_indicators.csv')))

# ----------------------------------------------

# ----------------------------------------------
# format the data set for analysis

# --------------------
# rename the variables
setnames(dt, c('age', 'sex', 'country', 'region', 'district', 'site', 'drop',
               'fq', 'prep_curr', 'prep_new', 'hts_tst', 'hts_tst_pos', 'tx_curr',
               'tx_new', 'tx_pvls_n', 'tx_pvls_d'))

# drop out level 7 - no data
dt[!is.na(drop)]
dt[ , drop:=NULL]

# check that no facility is not reporting at all
dt[is.na(hts_tst) & is.na(hts_tst_pos) & is.na(tx_curr) & is.na(tx_new) & is.na(tx_pvls_n) & is.na(tx_pvls_d)]

# reshape the data long and replace NAs with 0s
dt = melt(dt, 
    id.vars = c('age', 'sex', 'country', 'region', 'district', 'site', 'fq'))
dt[is.na(value) & !grepl('prep', variable), value:=0] # only prep indicators missing

# drop the missing values for prep (e.g. no pediatric enrollments)
dt = dt[!is.na(value)] # prep should now be semi-annual in FY20

# --------------------

# --------------------
# factor variables to create hierarchies in visuals, tables

# factor quarter
dt$fq = factor(dt$fq, c("FY20 Q1", "FY20 Q2", "FY20 Q3", "FY20 Q4", 
          "FY21 Q1", "FY21 Q2", "FY21 Q3"),
          c("FY20 Q1", "FY20 Q2", "FY20 Q3", "FY20 Q4", 
            "FY21 Q1", "FY21 Q2", "FY21 Q3"))

# factor age
dt$age = factor(dt$age,c("<1", "1-4", "5-9",  "10-14", "15-19",
  "20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50+"), c("<1", "1-4", "5-9",  "10-14", "15-19",
                     "20-24", "25-29", "30-34", "35-39", "40-44",
                     "45-49", "50+")) 

# --------------------
# create summary age categories

# Child, youth, adult category
dt[age %in% c("<1", "1-4", "5-9", "10-14"), cya:='Child']
dt[age %in% c("15-19","20-24"), cya:='Youth']
dt[age %in% c("25-29", "30-34", "35-39", "40-44",
              "45-49", "50+"), cya:='Adult']

# child, adolescent, young adult, adult
dt[age %in% c("<1", "1-4", "5-9"), caya:='Child']
dt[age %in% c("10-14", "15-19"), caya:='Adolescent']
dt[age %in% c("20-24"), caya:='Youth']
dt[age %in% c("25-29", "30-34", "35-39", "40-44",
              "45-49", "50+"), caya:='Adult']

# factor the results
dt$cya = factor(dt$cya, c('Child', 'Youth', 'Adult'),
                c('Child', 'Youth', 'Adult'))

dt$caya = factor(dt$caya, c('Child', 'Adolescent', 'Youth', 'Adult'),
                c('Child', 'Adolescent', 'Youth', 'Adult'))

# --------------------
# sum to the national level and export the data 

# national - all variables (include fiscal quarter)
dt_all = dt[ , .(value = sum(value, na.rm = T)),
            by = .(fq, sex, age, cya, caya, variable)]


# national - all variables - subset to just fy20
dt_all_fy20 = dt[grepl('20', fq)]

# drop cumulative indicators (include only Q4 for annual total)
dt_all_fy20 = dt_all_fy20[!(variable=='prep_curr' & grepl('Q2', fq))] 
dt_all_fy20 = dt_all_fy20[!(variable=='tx_curr' & fq %in% c('FY20 Q1', 'FY20 Q2',
                                                           'FY20 Q3'))] 
dt_all_fy20 = dt_all_fy20[!(variable=='tx_pvls_n' & fq %in% c('FY20 Q1', 'FY20 Q2',
                                                            'FY20 Q3'))] 
dt_all_fy20 = dt_all_fy20[!(variable=='tx_pvls_d' & fq %in% c('FY20 Q1', 'FY20 Q2',
                                                            'FY20 Q3'))] 

# sum it all up to get the correct totals
dt_all_fy20 = dt_all_fy20[ , .(value = sum(value, na.rm = T)),
                      by = .(sex, age, cya, caya, variable)]

# export the data 
write.csv(dt_all, paste0(dir, 'dt_all.csv'))
write.csv(dt_all_fy20, paste0(dir, 'dt_all_fy20_no_fq.csv'))


# --------------------
# rename the FY20 data frame for use in stats and figures
df = copy(dt_all_fy20)

# add an agyw binary
df[caya %in% c('Adolescent', 'Youth') & sex=='Female', agyw:=TRUE]
df[is.na(agyw), agyw:=FALSE]

# --------------------

# ----------------------------------------------
# source tables and visualizations

# 

# ----------------------------------------------



# ----------------------------------------------

# --------------------
# Descriptive statistics

# --------------------
# PrEP  Enrollment, FY20

# currently on prep - disparities
df[variable=='prep_curr', sum(value)]
df[variable=='prep_curr' & sex=='Female', sum(value)]
df[variable=='prep_curr' & sex=='Male', sum(value)]
df[variable=='prep_curr' & agyw==T, sum(value)]

df[variable=='prep_new', sum(value)]
df[variable=='prep_new' & sex=='Female', sum(value)]
df[variable=='prep_new' & sex=='Male', sum(value)]
df[variable=='prep_new' & agyw==T, sum(value)]

# --------------------
# HIV testing disparities, FY20 

# tested for HIV - hts_tst
test_tot = df[variable=='hts_tst', sum(value)]
df[variable=='hts_tst' & sex=='Male', sum(value)/test_tot]
df[variable=='hts_tst' & sex=='Female', sum(value)/test_tot]

df[variable=='hts_tst' & sex=='Female' & age %in% c("10-14", "15-19", "20-24"),
       sum(value)]
df[variable=='hts_tst' & sex=='Female' & age %in% c("10-14", "15-19"),
       sum(value)]
df[variable=='hts_tst' & sex=='Female' & age %in% c("20-24"),
       sum(value)]
df[variable=='hts_tst' & sex=='Male' & age %in% c("10-14", "15-19", "20-24"),
       sum(value)]
df[variable=='hts_tst' & sex=='Female' & age %in% c("10-14", "15-19", "20-24"),
   sum(value)]


# test positivity
pos = df[(variable=='hts_tst' | variable=='hts_tst_pos') & age %in% c("10-14", "15-19", "20-24"),
      .(value = sum(value)), by = .(sex, variable)]
pos = dcast(pos, sex~variable)
pos[ , rate:=hts_tst_pos/hts_tst]

# --------------------
# ART enrollment, FY20

df[variable=='hts_tst_pos' & age %in% c("10-14", "15-19", "20-24"),
       .(sum(value)), by = sex]

# ----------------------------------------------
