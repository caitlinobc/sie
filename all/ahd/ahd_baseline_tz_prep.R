# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/11/2021
# Import and clean the AHD study data
# For now: only Tanzania Baseline Cohort Data
# Run descriptive statistics, including account of missingness
# ----------------------------------------------

# ------------------------
# load packages

rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)

# ------------------------

# --------------------
# Files and directories

# set the working directory to the cameroon data
#dir = 'C:/Users/ccarelli/Documents/data/raw_att95_weekly/'
#OutDir = 'C:/Users/ccarelli/Documents/data/prepped/'
dir = 'C:/Users/Caitlin/OneDrive/Documents/'
setwd(dir)

# load the baseline data - CD4 
cd4 = data.table(read_xlsx(paste0(dir,
    'Tanzania Baseline Data Abstraction - October 6th, 2020.xlsx'), 
    sheet = 'allCD4_flat(siteid,pid)', skip = 1))

# --------------------

# ------------------------
# munge the CD4 data







