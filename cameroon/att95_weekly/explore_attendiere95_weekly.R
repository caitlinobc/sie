# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/19/2021
# Explore and visualise CDC Weekly Data
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
library(Hmisc)

# --------------------
# Files and directories

# set the working directory to the Weekly CDC Reporting Data 
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Cameroon/data/'
setwd(dir)

# --------------------
# import the data 
dt = readRDS("att95_prepped/cameroon_weekly_27_fy21_no_sex.rds")

# set the current week for checks
current_week = 27

# -------------------------------------------
# explore the data 

# --------------------
# create a variable for pre and post week 3
dt[ , var_set:=max(week), by=variable]
dt[var_set==current_week, current_var:=TRUE]
dt[var_set!=current_week, current_var:=FALSE]
dt[ ,var_set:=NULL]

# --------------------

write.csv(dt[current_var==TRUE, unique(variable)], paste0(dir, 'variables.csv'))






