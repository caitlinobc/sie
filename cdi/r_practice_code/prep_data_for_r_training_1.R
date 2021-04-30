# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/29/21
# Prep the sample data for the CDI team
# --------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clears the workspace

# --------------------
# load the packages you need to run this code file

library(readxl) # loads excel files
library(data.table) # manipulates data
# --------------------

# --------------------------------------------
# set directories and import the data

# CHANGE THIS TO THE LOCATION OF THE DATA ON YOUR COMPUTER
# set the main directory where the data are stored
# use forward slashes // for file names
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/CDI/data/r_training_data/'

# --------------------
# load the data 
dt = read.csv(paste0(dir, 'datim_sample_data_og.csv'))

# convert the data to a 'data table'
# data tables are a special format that makes data cleaning easier
dt = data.table(dt)

# --------------------
# rename columns and shape long

setnames(dt, c('region', 'quarter', 'sex', 'HTS_TST', 'HTS_POS'))

dt = melt(dt, id.vars = c('region', 'quarter', 'sex'))

# --------------------
# output the fancy, regional level data 

write.csv(dt, paste0(dir, 'datim_sample_data.csv'))

# --------------------




