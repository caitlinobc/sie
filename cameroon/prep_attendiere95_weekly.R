# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/19/20
# Cleaning and prep file for Attendiere 95 weekly data 
# Uses the weekly reporting tab at the site (facility) level
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

for (f in seq(length(files))) {
  
# ------------------------------------
# FORMAT AND PREP FACILITY LEVEL DATA 

# read in the weekly facility level data 
file_name = files[f]

#list the sheets to extract the tab by name
sheets = excel_sheets(file_name)
if (any(grepl("Littoral_Report", sheets))==T) sheet_name = "Littoral_Report"
if (any(grepl("SouthSite_Report", sheets))==T) sheet_name = "SouthSite_Report"
if (any(grepl("SITE Weekly", sheets))==T) sheet_name = "SITE Weekly"

# import the correct facility level sheet by name
dt = data.table(read_excel(paste0(dir, file_name), sheet = sheet_name))

# --------------------
# strip the date from the file name and save as a vector
file_name = gsub("weekly report ", "", tolower(files[f]))
file_name = gsub(".xlsx", "", file_name)

# the first week of this data set begins in fiscal year 20
if (grepl("53fy", file_name)==TRUE) { fiscal_yr = 20
  start_week = trimws(sapply(strsplit(file_name,"week"), "[", 2))
  start_week = gsub("fy19","",start_week)
  start_week = as.numeric(as.character(gsub("(?<![0-9])0+",
                                "", start_week, perl = TRUE)))
  print(paste0("FY overlap week: FY", fiscal_yr, " Week ", start_week))
   } else {fiscal_yr = 21 # refers to the fiscal year of the first day of the week
  start_week = sapply(strsplit(file_name,"week"), "[", 2)
  start_week = as.numeric(as.character(gsub("(?<![0-9])0+",
                        "", start_week, perl = TRUE)))
  } 

# --------------------
# rename the correctly named columns
original_names = c('Region', 'District', 'Facility Name', 'Tiers')
setnames(dt, original_names, c('region', 'district', 'facility', 'tier'))

# drop the totals row and save for quality check (last row)
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

# shape the data wide to attach to the data set and subset to filled rows
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

# create sex, age, indicator variables
dt_long[grepl('M$', dt_long$variable)==T, sex:='Male']
dt_long[grepl('F$', dt_long$variable)==T, sex:='Female']

dt_long[grepl('Adult', dt_long$variable)==T, age:='Adults (15+)']
dt_long[grepl('Child', dt_long$variable)==T, age:='Children (<15)']

dt_long[ , variable:=gsub('Children', 'Adults', dt_long$variable)] #split on 'adults'
dt_long$variable = unlist(lapply(strsplit(dt_long$variable,
                                          "Adults"), "[", 1))

# format the tier variable
dt_long[ , tier:=gsub('Tiers', '', tier)]
dt_long[ , tier:=as.numeric(as.character(tier))]

# --------------------
# create a date using information stripped from the file name
dt_long[ , week:=start_week]
dt_long[ , fiscal_yr:=fiscal_yr]

# calculate dates as seven days from the first monday in fiscal year 21
# in other words, monday, sept. 28 as october 1 was on thursday
dt_long[fiscal_yr==20, date:= as.Date("2020-09-28" , "%Y-%m-%d")]
dt_long[fiscal_yr==21, 
        date:= (as.Date("2020-09-28" , "%Y-%m-%d"))+(7*week)]

# add file name column for loop testing
dt_long[ , file_name:=file_name]

# --------------------
# rbind the files together to create a complete data set
if (f==1) { full_data = dt_long } else {
  full_data = rbind(full_data, dt_long)
}

} # end of for loop

# --------------------
# shorten the indicator variable and alter to description

# --------------------
# arrange columns in an intuitive order


# --------------------

# save as rds




View(full_data)




