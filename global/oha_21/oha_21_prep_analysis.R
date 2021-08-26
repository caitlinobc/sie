# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/26/21
# 2021 OHA Meeting Analysis 
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
library(raster)
# --------------------

# ----------------------------------------------
# Files and directories

# set the home directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/oha_21/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Global/Presentations/OHA Meeting 2021/'

# mapping director
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps/r_shape_files/'

# read in the data 
dt = data.table(read.csv(paste0(dir, 'oha_21_prep.csv')))

# ----------------------------------------------

# --------------------
# prep the data for analysis

# rename the variables for analysis 
setnames(dt, c('award','country', 'prep_curr', 'prep_new',
               'fq', 'date', 'sex', 'age'))
               
# shape the data long for visualization
idVars = c('award','country', 'sex', 'age', 'fq', 'date')
dt = melt(dt, id.vars = idVars)

# --------------------
# factor the variables

dt$fq = factor(dt$fq, c("FY20 Q2", "FY20 Q4",
        "FY21 Q1", "FY21 Q2", "FY21 Q3"),
        c("Q2 FY20", "Q4 FY20",
          "Q1 FY21", "Q2 FY21", "Q3 FY21"))

dt$date = factor(dt$date, c('Jan-Mar 2020', 'Oct-Dec 2020',
      'Jul-Sep 2020', 'Jan-Mar 2021', 'Apr-Jun 2021'), 
      c('Jan-Mar 2020', 'Oct-Dec 2020',
        'Jul-Sep 2020', 'Jan-Mar 2021', 'Apr-Jun 2021'))

dt$age = factor(dt$age, c("Unknown Age", "15-19", "20-24", "25-29",
                          "30-34", "35-39", "40-44", "45-49", "50+"),
                c("Unknown", "15-19", "20-24", "25-29",
                  "30-34", "35-39", "40-44", "45-49", "50+")) 

# format the grant names to match style guide
dt[award=='USAID RHITES SW', award:='USAID RHITES-SW']
dt[award=='USAID Eswatini', award:='USAID EHPCS']

# create an award name variable without the donor name
dt[ , award_alt:=trimws(gsub('USAID', '', award))]

# rearrange the order
dt = dt[ ,.(value), by = .(country, award, award_alt, fq, date,
        sex, age, variable)]

# --------------------

# ----------------------------------------------
# export an Excel file for Excel graphs 
dt_excel = copy(dt)

# change the names of variables to appear correctly on graphs
dt_excel[variable=='prep_curr', variable:='PREP_CURR']
dt_excel[variable=='prep_new', variable:='PREP_NEW']
setnames(dt_excel, c('Country', 'Award Full', 'Award', 'Quarter',
                     'Fiscal Quarter', 'Sex', 'Age', 'Variable',
                     'Value'))

# export the data, over writing the previous version 
write.xlsx(dt_excel, paste0(outDir, 'OHA_21_PREP_figures.xlsx'), overwrite = T)

# delete the data set
dt_excel = NULL

# ----------------------------------------------

# --------------------
# do some mapping




