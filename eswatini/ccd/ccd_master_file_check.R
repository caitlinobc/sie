# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# Run descriptive statistics and data quality checks on CCD data

# --------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace

# install the packages
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(openxlsx)
# --------------------

# --------------------------------------------
# import the data 

# set the main directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/ccd/'

# get the sheets from the data 
excel_sheets(paste0(dir, 'Reviewed DDD Reporting Template 29 Sept (20).xlsx'))


dt = data.table(read.xlsx(paste0(dir, 'Reviewed DDD Reporting Template 29 Sept (20).xlsx'),
               sheet = 5))

# --------------------------------------------

# --------------------------------------------
# EXAMINE THE TABS FOR DATA ERRORS
# --------------------
# examine the TB Screening tab - tab 5

# this sheet has multiple extraneous columns
length(dt)
names(dt)

# check that these columns do not contain data 
i = 1
for (i in i:length(dt)) {
  all_missing = all(is.na(dt[ ,..i]))
  if (all_missing==FALSE) print(paste0(i, " contains data!")) 
  i = i+1 }

# only 17 of the 16,285 columns actually contain data 
# subset to the first 17 columns
dt = dt[ , 1:17]

# --------------------
# rename the columns for usability

# save the original names for the output file
original_names = names(dt)

setnames(dt, original_names, c('region', 'facility', 'location',
    'week', 'sex', 'age', 'ppl_seen', 'on_tb_tx', 'eligible_for_screening',
    'screened', 'presumptive_tb', 'presum_tb_referred', 'samples_collected',
    'samples_sent_to_lab', 'results_received', 'bact_confirmed', 
    'bact_confirmed_started_tx'
))

# --------------------
# change column types for ease of manipulation

# region is sometimes misspelled
dt[region=='hhohho', region:='Hhohho']

# facility and location OK
# one week has incorrect formatting - drop from the data set
# one week is formatted correctly but has no associated dates
# these have associated values so ideally fix 
dt = dt[!is.na(week)]

# one location listed as "No DDD Conducted" - drop? (values all 0)
dt = dt[location!='No DDD Conducted']

# factor sex
dt$sex = factor(dt$sex, c('M', 'F'), c('Male', 'Female'))

# --------------------
#establish dates

# start and end dates
dt[ , start_date:=sapply(strsplit(week," "), "[", 2)]
dt[ , end_date:=sapply(strsplit(week," "), "[", 4)]
dt[ , start_date:=gsub("\\(", "", start_date)]
dt[ , start_date:=gsub("\\,", "", start_date)]
dt[ , end_date:=gsub("\\,", "", end_date)]

# establish years
dt[ , start_year:=as.numeric(sapply(strsplit(week," "), "[", 3))]
dt[ , end_year:=sapply(strsplit(week," "), "[", 5)]
dt[start_year==2021, end_year:=2021]
dt[ , end_year:=gsub("\\)", "", end_year)]
dt[ , end_year:=as.numeric(end_year)]

# paste the dates together and drop extra variables
dt[ , start_wk:=as.Date(paste(start_date, start_year), "%B %d %Y")]
dt[ , end_wk:=as.Date(paste(end_date, end_year), "%B %d %Y")]

# convert week number to numeric
dt[ , week:=sapply(strsplit(week," "), "[", 1)]
dt[ , week:=gsub("\\(", "", week)]
dt[ , week:=sapply(strsplit(week,"Week"), "[", 2)]
dt[,  week:=gsub("\\s", "", week)]
dt[ , week:=as.numeric(week)]


# fix week 3 of 2020
dt[week==3, start_year:=2021]

# drop extraneous variables
dt[ ,c('start_date', 'end_date', 'start_year', 'end_year'):=NULL]



# add quarter



