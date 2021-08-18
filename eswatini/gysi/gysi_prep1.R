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
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(openxlsx)
library(RColorBrewer)
# --------------------
# Files and directories

# set the home directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/eswatini/gysi/'
setwd(dir)

# read in the data 
dt = data.table(read.csv(paste0(dir, 'data.csv')))
# --------------------

# --------------------------------------------------
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
dt[is.na(value) & !grepl('prep', variable), value:=0]

# --------------------
# format the prep data

# in FY20, prep data are reported semi-annually
# both values should be 0
dt[grepl('prep', variable) & fq == 'FY20 Q1' & !is.na(value)]
dt[grepl('prep', variable) & fq == 'FY20 Q3' & !is.na(value)]

# replace the missing values in quarters reported with 0
dt[grepl('prep', variable) & is.na(value) & !(fq == 'FY20 Q2' | fq == 'FY20 Q4'), value:=0]
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
# sum to the national level

# national - all variables - subset to just fy20
dt_all = dt[grepl('20', fq), .(value = sum(value, na.rm = T)),
                      by = .(sex, age, cya, caya, variable)]



# ----------------------------------------------
# source tables and visualizations



# ----------------------------------------------




# --------------------
# Descriptive statistics

# --------------------
# HIV testing disparities
test_tot = dt_all[variable=='hts_tst', sum(value)]
dt_all[variable=='hts_tst' & sex=='Male', sum(value)/test_tot]
dt_all[variable=='hts_tst' & sex=='Female', sum(value)/test_tot]

dt_all[variable=='hts_tst' & sex=='Female' & age %in% c("10-14", "15-19", "20-24"),
       sum(value)]
dt_all[variable=='hts_tst' & sex=='Female' & age %in% c("10-14", "15-19"),
       sum(value)]
dt_all[variable=='hts_tst' & sex=='Female' & age %in% c("20-24"),
       sum(value)]

dt_all[variable=='hts_tst' & sex=='Male' & age %in% c("10-14", "15-19", "20-24"),
       sum(value)]

# --------------------
# test positivity

pos = dt_all[(variable=='hts_tst' | variable=='hts_tst_pos') & age %in% c("10-14", "15-19", "20-24"),
      .(value = sum(value)), by = .(sex, variable)]
pos = dcast(pos, sex~variable)
pos[ , rate:=hts_tst_pos/hts_tst]

# --------------------
# newly enrolled on ART

dt_all[variable=='hts_tst_pos' & age %in% c("10-14", "15-19", "20-24"),
       .(sum(value)), by = sex]

# ----------------------------------------------

# --------------------
# Figures specifically for the write-up

pdf(paste0(dir, 'writeup_figures.pdf'), height = 9, width = 16 )

# testing by sex, age over time - hts_tst
ggplot(dt_all[variable=='hts_tst'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#b2182b', '#fdae61')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Tested for HIV',
       fill ='Sex') +
  theme(text = element_text(size=24)) 

# tested HIV+ by sex, age over time - hts_tst_pos
ggplot(dt_all[variable=='hts_tst'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#d6604d', '#92c5de')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Tested HIV+',
       fill ='Sex') +
  theme(text = element_text(size=24)) 

# enrolled on ART by sex, age, over time
ggplot(dt_all[variable=='tx_curr'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#4575b4', '#a6dba0')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'On ART',
       fill ='Sex') +
  theme(text = element_text(size=24)) 

# newly enrolled on ART by sex, age over time
ggplot(dt_all[variable=='tx_new'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#35978f', '#c2a5cf')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Newly Enrolled on ART',
       fill ='Sex') +
  theme(text = element_text(size=24)) 


dev.off()
# --------------------
