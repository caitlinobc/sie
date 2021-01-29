# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/28/21
# Prep data from the AHD study for visualization
# Check fordata erros 
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/AHD/'

#---------------------------------------
# TANZANIA BASELINE DATA PREP

# --------------------
# First tab - enrollment indicators

# upload the data 
dt = data.table(read_xlsx(paste0(dir, 
        '/data/Tanzania Baseline Data Abstraction - October 6th, 2020.xlsx'), 
         sheet = 2, skip = 3))

# drop empty column names - most begin with x (e.g. x1)
drop_names = names(dt)[ grepl("^x", names(dt))]
dt[ ,c(drop_names, 'dov', 'gxtest_dt', 'gxpos'):=NULL]

# --------------------
# check and convert data types for ease of analysis

# --------------------
# convert dates to date types
dt[ , ahd_dt:=as.Date(ahd_dt)]
dt[ , dob:=as.Date(dob)]
dt[ , dtpos:=as.Date(dtpos)]
dt[ , cd4_after_ahdelig_dt:=as.Date(cd4_after_ahdelig_dt)]
dt[ , whostage1st_dt:=as.Date(whostage1st_dt)]

dt[ , tptstart_dt:=as.Date(tptstart_dt)]
dt[ , tptalready_dt:=as.Date(tptalready_dt)]
dt[ , sstest_dt:=as.Date(sstest_dt)]
dt[ , hvl6m_dt:=as.Date(hvl6m_dt)]
dt[ , tbtxcplt_dt:=as.Date(tbtxcplt_dt)]


# --------------------
# convert1/0s to logicals


# --------------------
# use factor labelling on a few variables



# --------------------
# add categories to subset while doing analysis



# --------------------
# examine the data quality challenges

# export a list of sites to compare to study criterion
sites = dt[ ,.(siteid = unique(siteid)), by = .(dhisid, dhisname, region, district)]
sites = sites[!is.na(dhisid)]
sites = dt[ ,.(siteid, dhisid, dhisname, region, district)] # put siteid first
write.csv(sites, paste0(dir, '/data/prepped/sites_included.csv'))

# patient ids: these counts should be the same (length of sheet and length of data)
dt[ ,length(unique(pid))]
nrow(dt)

# export a list of repeat patient identifiers
dt[ , pid_count:=.N, by = pid]
pids = dt[1 < pid_count]
pids = pids[order(pid)]
write.csv(pids, paste0(dir, '/data/prepped/repeat_pt_ids.csv'))

# export the rows with substantial missing data 
missing_rows = dt[is.na(ahd_dt)]
write.csv(missing_rows, paste0(dir, '/data/prepped/rows_with_missing_data.csv'))

# --------------------


#WHY ISNT THIS CHANGING