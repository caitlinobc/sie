# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 6/22/21
# Cleaning and prep file for Attendiere 95 weekly data 
# Includes if statements for monthly data 
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
library(Hmisc)

# --------------------
# Files and directories

# set frequency - 'Weekly' or 'Monthly'
freq = 'Monthly'

# set the working directory to the Weekly CDC Reporting Data 
mainDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/cameroon/cdc_reporting/'
if (freq == 'Weekly') dir = paste0(mainDir, 'weekly_raw/')
if (freq == 'Monthly') dir = paste0(mainDir,'monthly_raw/')
setwd(dir)

# set the output directory
outDir = paste0(mainDir, 'outputs/')

# set the output directory for prepped data
prepDir = paste0(mainDir, 'prepped/')

# run the data checks to ensure no data entry errors?
run_check = TRUE

# set the mapping directory
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps/pepfar_org_units/prepped/'

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
if (any(grepl("SITE Monthly", sheets))==T) sheet_name = "SITE Monthly"

# import the correct facility level sheet by name
dt = data.table(read_excel(paste0(dir, file_name), sheet = sheet_name))

# --------------------
# use the information in the file name 

# drop unnecessary pieces of the file name
file_name = gsub(".xlsx", "", file_name)
file_name = sapply(strsplit(file_name,"/"), "[", 2)

# strip the date from the file name and save as a vector
if (freq == "Weekly") file_name = gsub("weekly report ", "", tolower(file_name))
if (freq == "Monthly") file_name = gsub("monthly report ", "", tolower(file_name))

# the first weeks of the data are in FY20 and FY21
# file names changed to solely reflect FY21 for just this week (both regions)
if (freq == 'Weekly') { start_week = sapply(strsplit(file_name,"week"), "[", 2)
start_week = as.numeric(as.character(gsub("(?<![0-9])0+",
                        "", start_week, perl = TRUE))) } else {
              month_name = trimws(sapply(strsplit(file_name,"_"), "[", 2))
              year = trimws(sapply(strsplit(file_name,"_"), "[", 3))}

# --------------------
# rename the correctly named columns
setnames(dt, 1:4,  c('region', 'district', 'facility', 'tier'))

# drop the totals row (last row in the sheet)
# may include a "region = ALL" row which is dropped later
tot_rows = dt[ ,.N]
dt = dt[-tot_rows]

# check that the last row does not include total
dt = dt[!grepl("Total", region)]
dt = dt[!grepl("ALL", region)]

# --------------------
# replace values with preceding values in nested columns 
# this takes the values of the top three rows: indicator, age, sex
# aggregates them to a single variable name (e.g. HIV+ adolescent male)
# then reshapes to create an age and sex column
# and finally, shapes data long

# create a data table that will only contain unique column names
alt = dt[1:3]
if (freq == 'Monthly') alt = dt[1:2]
alt[ , region:= 'region']
alt[ , district:= 'district']
alt[ , facility:= 'facility']
alt[ , tier:=as.character(tier)]
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

if (freq == 'Weekly') {
alt = alt[-(1:3)]
alt[ ,row:=seq(1:nrow(alt))] # drop the rows with NA included
} else { alt = alt[-(2:5)] }

# --------------------
# for data that are not sex stratified, drop additional rows
if (sheet_name=='SITE Weekly') alt = alt[-(2:3)]

# --------------------
# create collapsed identifiers with indicator, age, sex
# shape data long to paste
if (freq=='Weekly') {alt_long = melt(alt, id.vars = c('region',
          'district', 'facility', 'tier', 'row')) } else {
            alt_long = melt(alt, id.vars = c('region',
            'district', 'facility', 'tier'))}
alt_long[ , variable:=as.character(variable)]

# for data that are not sex stratified, create collapsed identifiers
if (sheet_name=='SITE Weekly' | sheet_name =='SITE Monthly') {
alt_long[ , var_temp:=shift(variable)]
alt_long[value=="Adults ( 15+)" , var_replace:=paste0(variable, value)]
alt_long[value=="Children ( <15)" , var_replace:=paste0(var_temp, value)] }

# create collapsed identifiers for sex stratified data 
if (freq=='Weekly' & sheet_name!='SITE Weekly') {
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
if (sheet_name=='SITE Weekly' | sheet_name=='SITE Monthly') dt = dt[-(1)] # drop only first row
if (freq=='Weekly' | sheet_name!='SITE Weekly') dt = dt[-(1:3)]
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
if(freq=='Weekly') dt_long[ , week:=start_week]
if(freq=='Monthly') dt_long[ , month_name:=month_name]

# --------------------
# set up monthly dates 
if(freq=='Monthly') {
  
dt_long[month_name=='jan', month:='01']
dt_long[month_name=='feb', month:='02']
dt_long[month_name=='march', month:='03']
dt_long[month_name=='april', month:='04']
dt_long[month_name=='may', month:='05']
dt_long[month_name=='june', month:='06']

dt_long[month_name=='july', month:='07']
dt_long[month_name=='aug', month:='08']
dt_long[month_name=='sept', month:='09']
dt_long[month_name=='oct', month:='10']
dt_long[month_name=='nov', month:='11']
dt_long[month_name=='dec', month:='12']}

# --------------------

# calculate dates as seven days from the first monday in fiscal year 21
# in other words, monday, sept. 28 as october 1 was on thursday
if(freq=='Weekly') { dt_long[week==1, date:= as.Date("2020-09-28" , "%Y-%m-%d")]
dt_long[week!=1, date:= (as.Date("2020-09-28" , "%Y-%m-%d"))+(7*(week-1))] }

if(freq=='Monthly') dt_long[, date:=as.Date(paste0(year, '-', month, "-01" , "%Y-%m-%d"))]
if(freq=='Monthly') dt_long[ , week:=date]
dt_long[ ,month_name:=NULL]
  
# add in the year
dt_long[ , year:=year(date)] 

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
if (freq=='Monthly') start_week = dt_long[, unique(date)]
print(paste0("Processed week: ", start_week, "; region: ", dt_long$region[[1]]))

# create vector of start weeks and file names
if(freq=='Weekly') x = c(week = as.numeric(start_week), region = dt_long$region[[1]])
if(freq=='Monthly') x = c(month = month_name, region = dt_long$region[[1]])
if (f==1) files_processed = x
if (f!=1) files_processed = rbind(files_processed, x)
} # END OF FOR LOOP

# print the files processed to check
files_processed

# --------------------------------------

# --------------------------------------
# FORMAT THE AGGREGATE DATA 

# --------------------------------------
# some rows have double totals - ensure 'all' is not included
# this has to be at the end - creates errors otherwise 
full_data = full_data[region!='ALL']

# in the first two weeks, South is translated
full_data[region=='South', region:='Sud']

# --------------------
# shorten the indicator variable and alter to description
full_data[ , value:=as.numeric(as.character(value))]
full_data[ ,tier:=factor(tier)]

# --------------------
# replace indicators with abbreviations
full_data[ , variable:=gsub("index case testing \\(ICT\\)", "ICT", full_data$variable)]
full_data[ , variable:=gsub("enhanced adherence counseling \\(EAC\\)", "EAC", full_data$variable)]
full_data[ , variable:=gsub("same day ART initiation \\(SDI\\)", "SDI", full_data$variable)]
full_data$variable = gsub("community ART dispensations \\(CAD\\)", "CAD", full_data$variable)

# --------------------

#-------------------------------------------------
# SIMPLE QUALITY CHECKS 

# for the weekly data, check all weeks are included 
if(freq=='Weekly') {
  
most_recent_week = max(full_data$week)
# check that every week is counted - counts each week or month up to most recent
for (r in unique(full_data$region)) {
if (all(full_data[, unique(week)] %in% c(1:most_recent_week))!=TRUE) print("Week skipped!") }

# double check that every file was processed
list_weeks = seq(1:most_recent_week)
files_processed = data.table(files_processed)

sud_weeks = files_processed[region=='Sud' | region=='South', list_weeks %in% week]
lit_weeks = files_processed[region=='Littoral', list_weeks %in% week]
if (!all(sud_weeks==TRUE)) print("Missing a week in Sud!")
if (!all(lit_weeks==TRUE)) print("Missing a week in Littoral!")}

# ------------------------------------------------
# FACILITY NAME CORRECTIONS

# correct facility names to match DATIM names
# these also differ across weeks (some typos/lack of standardization)

# subset to facility names to perform check 
fac = full_data[,.(facility = unique(facility))][order(facility)]

# import the list of datim health facilities
hf = data.table(readRDS(paste0(mapDir, 'datim_health_facilities.rds')))

# subset to egpaf facilities in cameroon
hf = hf[country=='CMR' & egpaf==TRUE]

# drop unecessary variables for PBI
hf[ ,c('level', 'type', 'sub_dist', 'country', 'egpaf'):=NULL]

# are the facilities in DATIM?
fac[facility %in% hf$name] # 70 match initially 

# change one name in the DATIM list with failed special characters
full_data[grepl('CSIU', facility), facility:='CSIU Number 1']

# change names in the data to standardized DATIM names
full_data[grepl('Marguerite', facility), facility:='CSI Ste Marguerite de Nkolomang']
full_data[grepl('CEBEC Bonaberie', facility), facility:='Hopital CEBEC  Bonaberie']
full_data[grepl('AD LUCEM Bonabéri', facility), facility:='CS AD LUCEM Bonaberi']
full_data[facility=='CM SAFACAM MBONGO', facility:='CM SAFACAM MBONGO (DIZANGUE)']
full_data[grepl("Ndogpassi", facility), facility:='CMA  Ndogpassi III Centre']

# ensure no facility dropped out
full_data[!(facility %in% hf$name), unique(facility)] # 1 does not match

# --------------------
# merge in org unit ids
ids = hf[ , .(id, facility = name)]
full_data = merge(full_data, ids, by = 'facility', all.x=T)

# one facility does not merge
full_data[facility=='CSIU Number 1', id:='nXrhnc7vTDj']

# change the name of this facility in DATIM so it appears correctly in PBI
hf[id=='nXrhnc7vTDj', name:='CSIU Number 1']

# --------------------
# reset the names of the hf file for pbi
setnames(hf, c('orgUnit ID', 'Health Facility', 'Parent ID', 'District',
               'Region', 'Latitude', 'Longitude'))

# --------------------
# export the list of facilities 
write.csv(hf, paste0(prepDir, 'List of DATIM sites.csv'))

# --------------------
# save as rds file - shaped long for R analysis

# label the last week uploaded - full data; all disaggregation
# major difference: this sheet includes sex for weeks 1 - 3
saveRDS(full_data, paste0(OutDir, 'att95_prepped/cameroon_weekly_',
              most_recent_week, '_fy21_full.rds'))
full_data[ ,length(unique(week)), by = region]

# save file aggregated across sex (data are not sex stratified after week 2)
sum_vars = names(full_data)[names(full_data)!="sex" & names(full_data)!="value"]

full_data_no_sex = full_data[,.(value = sum(value)), by = sum_vars]
saveRDS(full_data_no_sex, paste0(OutDir, 'att95_prepped/cameroon_weekly_',
            most_recent_week, '_fy21_no_sex.rds'))
full_data_no_sex[ ,length(unique(week)), by = region]

# -----------------------------------------------
# PBI DATA 

# --------------------
# add quarter year and month year for PBI

# create a temporary month variable used for labeling
full_data_no_sex[ , month:=as.Date(paste0('01-', month(date), '-', year), '%d-%m-%Y')]

# create pepfar quarter-year for visualization
full_data_no_sex[month(month) %in% c(10, 11, 12), qtr:=paste0('Q1 FY21')]
full_data_no_sex[month(month) %in% c(1, 2, 3), qtr:=paste0('Q2 FY21')]
full_data_no_sex[month(month) %in% c(4, 5 , 6), qtr:=paste0('Q3 FY21')]
full_data_no_sex[month(month) %in% c(7, 8, 9), qtr:=paste0('Q4 FY21')]
# one October week in FY21 starts in September 2020
full_data_no_sex[month(month)==9 & year==2020, qtr:=paste0('Q1 FY21')]

#--------------------
# convert year to fiscal year
full_data_no_sex[ , year:=2021]

# --------------------
# drop out the first two weeks of data until the mapping
full_data_no_sex = full_data_no_sex[week!=1 & week!=2]

# --------------------
# remove "# of" from variable names to shorten the names 
full_data_no_sex[ , variable:=trimws(gsub("# of", "", variable))]
full_data_no_sex[ , variable:=capitalize(variable)]

# --------------------
# reshape wide for power bi
full_data_no_sex = dcast(full_data_no_sex, file_name+id+facility+tier+district+region+age+date+week+month+qtr+year~variable, value.var='value')

# -----------------
# export a csv for use in power bi dashboards
setnames(full_data_no_sex, c('file_name', 'id', 'facility', 'tier', 
              'district', 'region', 'age', 'date', 
              'week', 'month', 'qtr', 'year'),
              c('File Name', 'orgUnit ID', 'Health Facility', 'Tier',
               'District', 'Region', 'Age Category', 
                'Date', 'Week', 'Month', 'Quarter', 'Fiscal Year'))

write.csv(full_data_no_sex, paste0(OutDir, 'att95_prepped/cameroon_weekly_fy21_master_pbi.csv'))

# ------------------------------------
# THE END
# ------------------------------------







