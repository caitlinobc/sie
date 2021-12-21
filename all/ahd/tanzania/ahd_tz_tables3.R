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

# set the output directory for visuals
outDir = paste0(dir, 'outputs/')

# ------------------------

# ----------------------------------------------
# import the data
dt = readRDS(paste0(prepDir, 'full_data.RDS'))

# ----------------------------------------------



# number of participants by sex and age category


dt_sa = dt[ ,.(value = length(unique(pid))), by =.(sex, age_cat)]
dt_sa = dcast(dt_sa, age_cat~sex)
write.csv(dt_sa, paste0(outDir, 'age_sex_endline.csv'))

dt_elig = dt[, .(value = length(unique(pid))) , by = .(ahd_elig)]
write.csv(dt_elig, paste0(outDir, 'elig_baseline.csv'))