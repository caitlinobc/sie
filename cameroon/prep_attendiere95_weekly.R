# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/8/21
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

# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Cameroon/data/att95_weekly_raw/'
OutDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Cameroon/data/'
setwd(dir)

# set current week to check files against
current_week = 18

# run the data checks to ensure no data entry errors?
run_check = TRUE

# ----------------------------------------------
# PREP DATA 

# --------------------
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
# the first two weeks have tabs named for the region - after that SITE WEEKLY
if (any(grepl("Littoral_Report", sheets))==T) sheet_name = "Littoral_Report"
if (any(grepl("SouthSite_Report", sheets))==T) sheet_name = "SouthSite_Report"
if (any(grepl("SITE Weekly", sheets))==T) sheet_name = "SITE Weekly"

# import the correct facility level sheet by name
dt = data.table(read_excel(paste0(dir, file_name), sheet = sheet_name))

# --------------------
# strip the date from the file name and save as a vector
file_name = gsub("weekly report ", "", tolower(files[f]))
file_name = gsub(".xlsx", "", file_name)
file_name = sapply(strsplit(file_name,"/"), "[", 2)

# the first weeks of the data are in FY20 and FY21
# file names changed to solely reflect FY21 for just this week (both regions)
start_week = sapply(strsplit(file_name,"week"), "[", 2)
start_week = as.numeric(as.character(gsub("(?<![0-9])0+",
                        "", start_week, perl = TRUE)))

# --------------------
# rename the correctly named columns
setnames(dt, 1:4,  c('region', 'district', 'facility', 'tier'))

# drop the totals row (last row in the sheet)
# may include a "region = ALL" row which is dropped later
tot_rows = dt[ ,.N]
dt = dt[-tot_rows]

# --------------------
# replace values with preceding values in nested columns 
# this takes the values of the top three rows: indicator, age, sex
# aggregates them to a single variable name (e.g. HIV+ adolescent male)
# then reshapes to create an age and sex column
# and finally, shapes data long

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
# for data that are not sex stratified, drop additional rows
if (sheet_name=='SITE Weekly') alt = alt[-(2:3)]

# --------------------
# create collapsed identifiers with indicator, age, sex 
alt_long = melt(alt, id.vars = c('region',
          'district', 'facility', 'tier', 'row')) # shape data long to paste
alt_long[ , variable:=as.character(variable)]

# for data that are not sex stratified, create collapsed identifiers
if (sheet_name=='SITE Weekly') {
alt_long[ , var_temp:=shift(variable)]
alt_long[value=="Adults ( 15+)" , var_replace:=paste0(variable, value)]
alt_long[value=="Children ( <15)" , var_replace:=paste0(var_temp, value)] }

# create collapsed identifiers for sex stratified data 
if (sheet_name!='SITE Weekly') {
alt_long[ , var_replace:='p']
for (v in unique(alt_long$variable)) {
 x = as.character(paste(alt_long[variable==v, unique(value)],
                        collapse = " "))
 alt_long[variable==v]$var_replace = x }}

# --------------------
# create the list of new variable names for the data set
IDvars = c('region', 'district', 'facility','tier')
new_names = c(IDvars, (unique(alt_long$var_replace)))

# rename the variables
setnames(dt, new_names)
# --------------------

# ------------------------------------
# FORMAT THE DATA FOR UNIQUE ROWS

# shape data long 
if (sheet_name=='SITE Weekly') dt = dt[-(1)] # drop only first row
if (sheet_name!='SITE Weekly') dt = dt[-(1:3)]
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
dt_long[ , fiscal_yr:=21] # change if ever using multi-year data

# calculate dates as seven days from the first monday in fiscal year 21
# in other words, monday, sept. 28 as october 1 was on thursday
dt_long[week==1, date:= as.Date("2020-09-28" , "%Y-%m-%d")]
dt_long[week!=1, date:= (as.Date("2020-09-28" , "%Y-%m-%d"))+(7*(week-1))]

# add file name column for loop testing
dt_long[ , file_name:=file_name]

# trim white space from variable names in early weeks
dt_long[ , variable:=trimws(variable, "both")]

# --------------------
# check the totals against tot_check

# --------------------
# rbind the files together to create a complete data set
if (f==1) { full_data = dt_long } else {
  full_data = rbind(full_data, dt_long)
}
# --------------------
# print a statement to show for loop progress
print(paste0("Processed week: ", start_week, "; region: ", dt_long$region[[1]]))

} # END OF FOR LOOP

# --------------------
# shorten the indicator variable and alter to description
full_data[ , value:=as.numeric(as.character(value))]
full_data[ ,tier:=factor(tier)]

# --------------------
# in the first two weeks, South is translated
full_data[region=='South', region:='Sud']

# --------------------
# fix minor data inconsistencies

# some rows have double totals - ensure 'all' is not included
# this has to be at the end - creates errors otherwise 
full_data = full_data[region!='ALL']

# --------------------
# SIMPLE QUALITY CHECKS 

# check that every week is counted
for (r in unique(full_data$region)) {
if (all(full_data[, unique(week)] %in% c(1:current_week))!=TRUE) print("Week skipped!") }

# --------------------
# corrections based on initial quality check

# correct facility names to match DATIM names
# these also differ across weeks (some typos/lack of standardization)



# --------------------
# run the data checking file, including checks against total rows

# reset working directory and run the file
setwd()
source()


# --------------------
# save as rds file
saveRDS(full_data, paste0(OutDir, 'att95_prepped/cameroon_weekly_fy21_full.rds'))

# save file aggregated across sex (data are not sex stratified after week 2)
sum_vars = names(full_data)[names(full_data)!="sex" & names(full_data)!="value"]
full_data_no_sex = full_data[,.(value = sum(value)), by = sum_vars]
saveRDS(full_data_no_sex, paste0(OutDir, 'att95_prepped/cameroon_weekly_fy21_no_sex.rds'))

# --------------------
# export a csv for use in power bi dashboards
setnames(full_data_no_sex, c('Region', 'District', 'Health Facility', 'Tier', 'Indicator',
                  'Age Category', 'Week', 'Fiscal Year', 'Date', 'File Name', 'Value'))
write.csv(full_data_no_sex, paste0(OutDir, 'att95_prepped/cameroon_weekly_fy21_no_sex.csv'))

# ------------------------------------
# THE END
# ------------------------------------

