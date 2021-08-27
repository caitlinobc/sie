# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/27/21
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
library(rgdal)
library(ggrepel)
# --------------------

# ----------------------------------------------
# Files and directories

# set the home directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/oha_21/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Global/Presentations/OHA Meeting 2021/'

# mapping directory
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps/r_shape_files/'

# read in the data 
dt = data.table(read.csv(paste0(dir, 'viral_load_data_all.csv')))

# ----------------------------------------------

# --------------------
# prep the data for analysis

# rename the variables for analysis 
setnames(dt, c('award','fq', 'country', 'vlc', 'vls',
                'vln', 'vld'))

# ----------------------------------------------
# convert decimals to percentages
dt[ , vlc:=round(vlc*100, 1)]
dt[ , vls:=round(vls*100, 1)]

# ----------------------------------------------
# shape the data long for visualization
idVars = c('award','country', 'fq')
dt = melt(dt, id.vars = idVars)

# --------------------
# factor the variables

dt$fq = factor(dt$fq, c("FY20 Q1","FY20 Q2","FY20 Q3", "FY20 Q4",
                        "FY21 Q1", "FY21 Q2", "FY21 Q3"),
               c("Q1 FY20", "Q2 FY20", "Q3 FY20", "Q4 FY20",
                 "Q1 FY21", "Q2 FY21", "Q3 FY21"))


# factor quarter descriptions
dt[fq=="Q1 FY20", date:='Oct-Dec 2019']
dt[fq=="Q2 FY20", date:='Jan-Mar 2020']
dt[fq=="Q3 FY20", date:='Apr-Jun 2020']
dt[fq=="Q4 FY20", date:='Jul-Sep 2020']
dt[fq=="Q1 FY21", date:='Oct-Dec 2020']
dt[fq=="Q2 FY21", date:='Jan-Mar 2021']
dt[fq=="Q3 FY21", date:='Apr-Jun 2021']

dt$date = factor(dt$date, c('Oct-Dec 2019','Jan-Mar 2020','Apr-Jun 2020',
          'Jul-Sep 2020', 'Oct-Dec 2020', 'Jan-Mar 2021', 'Apr-Jun 2021'),
          c('Oct-Dec 2019','Jan-Mar 2020','Apr-Jun 2020',
            'Jul-Sep 2020', 'Oct-Dec 2020', 'Jan-Mar 2021', 'Apr-Jun 2021'))


# format the grant names to match style guide
dt[award=='USAID RHITES SW', award:='USAID RHITES-SW']
dt[award=='USAID Eswatini', award:='USAID EHPCS']

# create an award name variable without the donor name
dt[ , award_alt:=trimws(gsub('USAID', '', award))]

# rearrange the order
dt = dt[ ,.(value), by = .(country, award, award_alt, fq, date, variable)]

# --------------------

# ----------------------------------------------
# export an Excel file for Excel graphs 
dt_excel = copy(dt)

# change the names of variables to appear correctly on graphs
dt_excel[variable=='vlc', variable:='Viral Load Coverage (%)']
dt_excel[variable=='vls', variable:='Viral Suppression (%)']
dt_excel[variable=='vln', variable:='Virally Suppressed']
dt_excel[variable=='vld', variable:='Received a VL Test']


setnames(dt_excel, c('Country', 'Award Full', 'Award', 'Quarter',
                     'Fiscal Quarter', 'Variable', 'Value'))

# export the data, over writing the previous version 
write.xlsx(dt_excel, paste0(outDir, 'OHA_21_VL_figures.xlsx'), overwrite = T)

# delete the data set
rm(dt_excel)

# ----------------------------------------------