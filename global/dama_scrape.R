# ----------------------------------------------
# Caitlin O'Brien-Carelli, Mike Bitok
#
# 8/17/21
# Scrape the DAMA backups from Sharepoint
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
# --------------------

# --------------------
# set the working directory and load files

# set the working directory
dir = 'C:/Users/ccarelli/E Glaser Ped AIDS Fdtn/SI&E Cameroon - Atteindre 95 FY20'
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/cameroon/'

# list the files in the folder
all_files = list.files(dir, recursive = T, full.names = T)

# among the files, find the .BAK backups
files = data.table(file_path = all_files[grepl('.BAK', all_files)])

# --------------------
# extract the region, zone, and facility
files[ , region:=sapply(strsplit(file_path,"/"), "[", 6)]
files[ , zone:=sapply(strsplit(file_path,"/"), "[", 7)]
files[ , district:=sapply(strsplit(file_path,"/"), "[", 8)]
files[ , facility:=sapply(strsplit(file_path,"/"), "[", 9)]

# write a list to excel
write.xlsx(files, paste0(outDir, 'dama_backups.xlsx' ))

# --------------------
# # loop to load the files
# i = 1
# for (f in files) {
#   read.table(f)
#   if (i==1) full_data = f
#   if (i!=1) full_data = rbind(full_data, f)
#   
# }
# --------------------
