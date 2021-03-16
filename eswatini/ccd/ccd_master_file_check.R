# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# Clean CCD data and run data quality checks
# uses the CCD Master File
# 3/15/2021
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
# set directories and import the data

# set the main directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/ccd/'

# get the sheets from the data 
excel_sheets(paste0(dir, 'Reviewed DDD Reporting Template 29 Sept (20).xlsx'))

# output directory for pdfs
pdf_out = paste0(dir, 'outputs/tb_services_diagnostic_plots.pdf')

# --------------------
# import the data by sheet 

# week 5, tb services
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

# region is sometimes misspelled; change other incongruous spellings
dt[region=='hhohho', region:='Hhohho']
dt[location=='church', location:='Church']
dt[location=='Foot ball Pitch', location:='Football Pitch']
dt[location=='Shops', location:='Shop']

# facility and location OK
# one week has incorrect formatting - drop from the data set
# one week is formatted correctly but has no associated dates
# these entries have associated values so ideally fix 
dt = dt[!is.na(week)]

# one location listed as "No DDD Conducted" - drop? (values all 0)
dt = dt[location!='No DDD Conducted']

# factor sex
dt$sex = factor(dt$sex, c('M', 'F'), c('Male', 'Female'))

# people seen mysteriously loads as a character - reformat
dt[ , ppl_seen:=as.numeric(ppl_seen)]

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

# fix week 3 of 2020 - should be 2021 - program did not exist Jan 2020
dt[start_wk=='2020-01-13', start_wk:=as.Date('2021-01-18')]
dt[end_wk=='2020-01-19', end_wk:=as.Date('2021-01-24')]

# drop extraneous variables
dt[ ,c('start_date', 'end_date', 'start_year', 'end_year'):=NULL]

# add monthly date 
dt[ , month:=paste0(year(start_wk), '-', month(start_wk), '-01')]
dt[ , month:=as.Date(month, '%Y-%m-%d')]

# add quarter
dt[month(start_wk) %in% c(1:3), qtr:=1]
dt[month(start_wk) %in% c(4:6), qtr:=2]
dt[month(start_wk) %in% c(7:9), qtr:=3]
dt[month(start_wk) %in% c(10:12), qtr:=4]

# --------------------
# logic checks

dt[ppl_seen < on_tb_tx] # 4
dt[ppl_seen < eligible_for_screening] # 4
dt[ppl_seen < screened] # 3
dt[ppl_seen < presumptive_tb] # 9

# --------------------
# organize the variables in a preferred order 

# --------------------
# shape the data long for visualization
idVars = c('facility', 'location', 'region',
           'sex', 'age', 'week', 'start_wk', 'end_wk',
           'month', 'qtr')
dt_long = melt(dt, id.vars = idVars)

# factor the variable for display
vars = dt_long[,unique(variable)]
dt_long$variable = factor(dt_long$variable, vars,
          c('People seen at the CCD', 'On TB treatment', "Eligible for screening",
            'Screened', 'Presumptive TB', 'Presumptive TB - referred', 
            'Samples collected', 'Samples sent to the lab', 'Lab results received',
            'Bacteriologically confirmed', 'Confirmed and started treatment'))
            
            
# --------------------

