# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/19/20
# Cleaning and prep file for Attendiere 95 weekly data 
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls())
library(readxl)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)

# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/Documents/data/'
setwd(dir)

# list the files to be prepped
files = list.files('./', recursive=TRUE)
length(files)

# --------------------
# Create a prep function to be run on facility-level weekly data

# ------------------------------------
# FORMAT AND PREP FACILITY LEVEL DATA 

# read in the weekly facility level data 
dt = data.table(read_excel(paste0(dir, files[1]), sheet=3))

# --------------------
# rename the correctly named columns
original_names = c('Region', 'District', 'Facility Name', 'Tiers')
setnames(dt, original_names, c('region', 'district', 'facility', 'tier'))

# drop the totals row and save for quality check
tot_rows = dt[ ,.N]
tot_check = dt[tot_rows]
dt = dt[-tot_rows] 

# --------------------
# replace values with preceding values in nested columns 

# create a data table that will only contain unique column names
alt = dt[1:3]
alt[ , region:= 'region']
alt[ , district:= 'district']
alt[ , facility:= 'facility']
alt[ , tier:= 'tier']

for (s in seq(1:3)) {
# select a row and expand to fill all fields
vec = alt[s]
vec = melt(vec, id.vars = c(1:4))
vec[ ,variable:=NULL]
vec[ , value:= na.locf(value, na.rm = F)]
vec[ , placehold:=seq(from = 1, to = nrow(vec))]

# shape wind to attach to the data set and subset to filled rows
vec_wide = dcast(vec,region+district+facility+tier~placehold, value.var = 'value')
alt = rbind(alt, vec_wide, use.names = F) }
alt = alt[-(1:3)]
alt[ ,row:=seq(1:nrow(alt))] # drop the rows with NA included

# --------------------
# create collapsed identifiers with indicator, age, sex
alt_long = melt(alt, id.vars = c('region',
          'district', 'facility', 'tier', 'row')) # shape data long to paste
alt_long[ , var_new:='p']

for (v in unique(alt_long$variable)) {
 x = as.character(paste(alt_long[variable==v, unique(value)],
                        collapse = " "))
 alt_long[variable==v]$var_new = x }

# --------------------
# create the list of new variable names for the data set
IDvars = c('region', 'district', 'facility','tier')
new_names = c(IDvars, (unique(alt_long$var_new)))

# rename the variables
setnames(dt, new_names)
# --------------------

# ------------------------------------
# FORMAT THE DATA FOR UNIQUE ROWS

# shape data long 
dt = dt[-(1:3)]
dt_long = melt(dt, id.vars = IDvars)
dt_long[,variable:=trimws(variable, which = "both")] #ensure no excess white space

# create age, sex, indicator variables
dt_long[grepl('M$', dt_long$variable)==T, sex:='Male']
dt_long[grepl('F$', dt_long$variable)==T, sex:='Female']

dt_long[grepl('Adult', dt_long$variable)==T, age:='Adults (15+)']
dt_long[grepl('Child', dt_long$variable)==T, age:='Children (<15)']

dt_long[ , variable:=gsub('Children', 'Adults', dt_long$variable)] #split on 'adults'
dt_long$variable = unlist(lapply(strsplit(dt_long$variable,
                                          "Adults"), "[", 1))

# format the tier variable
dt_long[ , tier:=gsub('Tiers', '', tier)]
dt_long[ ,tier:=as.numeric(as.character(tier))]

# --------------------
# shorten the indicator variable and alter to description

# strip the date from the file name

# save as rds

# run function on sud








