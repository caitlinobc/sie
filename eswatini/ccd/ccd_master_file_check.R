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
library(tools)
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
excel_sheets(paste0(dir, 'master_file/Reviewed DDD Reporting Template 29 Sept (20).xlsx'))

# output directory for pdfs
pdf_out = paste0(dir, 'outputs/tb_services_diagnostic_plots.pdf')

# output directory for prepped data 
outDir = paste0(dir, 'prepped/')

# --------------------
# set the argument for the sheet you want to import 

sheet = 2

# --------------------------------------------
# import the data by sheet 

dt = data.table(read.xlsx(paste0(dir,
    'master_file/Reviewed DDD Reporting Template 29 Sept (20).xlsx'),
     sheet = sheet))
# --------------------------------------------

# --------------------------------------------
# EXAMINE THE TABS FOR DATA ERRORS
# --------------------
# examine the tab for excess columns

length(dt)
names(dt)

# --------------------
# Tab 2 - ART Indicators has some unnecessary columns 

if (sheet==2) {
print(dt[ ,unique(Check)])
print(dt[ ,unique(X31)]) # this column appears to contain notes
print(dt[ ,unique(X32)]) # this column appears to be a pivot table for internal use
}

# drop calculation columns to solely include variables
if (sheet==2) dt = dt[ , 1:29]

# --------------------
# Tab 5 -TB Services has multiple extraneous columns (drop)

# check that these columns do not contain data 
i = 1
for (i in i:length(dt)) {
  all_missing = all(is.na(dt[ ,..i]))
  if (all_missing==FALSE) print(paste0(i, " contains data!")) 
  i = i+1 }

# only 17 of the 16,285 columns actually contain data 
# subset to the first 17 columns
if (sheet==5) dt = dt[ , 1:17]
# --------------------

# --------------------------------------------
# rename the columns for usability

# save the original names for the output file
original_names = names(dt)

# art indicators - sheet 2
setnames(dt, original_names, c('region', 'facility', 'location',
      'week', 'sex', 'age', 'tx_curr_art_clients_refill_due',
      'cum_clients_enrolled', 'offered_chcd', 'accepted_enrolled',
      'exited_chcd', 'opted_out', 'died', 'transferred', 'stopped_tx',
      'ltfu', 'chcd_clt_due_for_refill', 'received_arvs_chcd', 
      'received_3mos_arvs', 'received_6mos_arvs', 'missed_chcd_appt',
      'missed_followed_up', 'missed_fu_reached', 'missed_fu)_reapp',
      'missed_fu_reapp_received_arvs_chcd', 'missed_fu_reapp_received_arvs_facility',
      'vl_eligible', 'vl_chcd', 'vl_chcd_received_results'
))


# tb screening - sheet 5
if (sheet==5) setnames(dt, original_names, c('region', 'facility', 'location',
    'week', 'sex', 'age', 'ppl_seen', 'on_tb_tx', 'eligible_for_screening',
    'screened', 'presumptive_tb', 'presum_tb_referred', 'samples_collected',
    'samples_sent_to_lab', 'results_received', 'bact_confirmed', 
    'bact_confirmed_started_tx'
))

# --------------------
# change column types for ease of manipulation

# check for odd values in the ID variables
dt[ ,unique(region)]
dt[ ,.(facility = unique(facility))][order(facility)]
dt[ ,.(location = unique(location))][order(location)]
dt[ ,unique(sex)]
dt[ ,unique(age)]

# one location listed as "No DDD Conducted" - drop?
dt = dt[location!='No DDD Conducted'] # in art indicators - some values

# standardize the regions
dt[ ,region_check:=tolower(region)]
dt[grepl('sh', region_check), region:='Shiselweni']
dt[grepl('hh', region_check), region:='Hhohho']
dt[ , region_check:=NULL]

# standardize locations
dt[ , location:=trimws(toTitleCase(tolower(location)))]

# region is sometimes misspelled; change other incongruous spellings
dt[location=='church', location:='Church']
dt[location=='Foot ball Pitch', location:='Football Pitch']
dt[location=='Shops', location:='Shop']
dt[location=='under a Tree', location:='Under a Tree']

# facility and location OK
# one week has incorrect formatting - drop from the data set
# one week is formatted correctly but has no associated dates
# these entries have associated values so ideally fix 
dt = dt[!is.na(week)]

# format and factor sex
dt[ , sex:=trimws(sex)]
dt$sex = factor(dt$sex, c('M', 'F'), c('Male', 'Female'))

# a few variables mysteriously load as a character - reformat
# these typically have " " rather than NA for missing data 
if(sheet==2)dt[ , missed_fu_reapp_received_arvs_chcd:=as.numeric(missed_fu_reapp_received_arvs_chcd)]
if(sheet==2) dt[ , missed_fu_reapp_received_arvs_facility:=as.numeric(missed_fu_reapp_received_arvs_facility)]
if(sheet==5) dt[ , ppl_seen:=as.numeric(ppl_seen)] 

# --------------------
# fix typos in age categories and factor age

dt[grepl('Child', age) | grepl('14', age), age:='Child (1 - 14)']
dt[grepl('Adol', age) | grepl('15', age), age:='Adolescent (15 - 21)']
dt[grepl('Adult', age) | grepl('22', age), age:='Adult (22+)']

# --------------------
#establish dates

# drop out old template values
dt = dt[!grepl('Template', week)]

# there is a mistake in one weekly value for wek 33 - fix so code applies
if (sheet==2) dt[grepl("33", week), week:=gsub("33", "33 ", week)]

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
if (sheet==5) {
dt[start_wk=='2020-01-13', start_wk:=as.Date('2021-01-18')]
dt[end_wk=='2020-01-19', end_wk:=as.Date('2021-01-24')] }

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

if (sheet==5) {
dt[ppl_seen < on_tb_tx] # 4
dt[ppl_seen < eligible_for_screening] # 4
dt[ppl_seen < screened] # 3
dt[ppl_seen < presumptive_tb] # 9 
}

# --------------------
# organize the variables in a preferred order 

# --------------------
# shape the data long for visualization
idVars = c('facility', 'location', 'region',
           'sex', 'age', 'week', 'start_wk', 'end_wk',
           'month', 'qtr')
dt_long = melt(dt, id.vars = idVars)

# factor the variable for display
vars = dt_long[, unique(variable)]

if (sheet==5) dt_long$variable = factor(dt_long$variable, vars,
            c('People seen at the CCD', 'On TB treatment', 'Eligible for screening',
            'Screened', 'Presumptive TB', 'Presumptive TB - referred', 
            'Samples collected', 'Samples sent to the lab', 'Lab results received',
            'Bacteriologically confirmed', 'Confirmed and started treatment')) 

# --------------------
    
# --------------------------------------------
# FACILITY NAME STANDARDIZATION

# import the list of datim health facilities
hf = data.table(readRDS('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/prepped/datim_health_facilities.rds'))

# subset to egpaf facilities in eswatini
hf = hf[country=='ESW' & egpaf==TRUE]

# drop unecessary variables for PBI
hf[ ,c('level', 'type', 'sub_dist', 'country', 'egpaf'):=NULL]

dt[ , facility:=gsub("Center", "Centre", facility)]
dt[grepl("ane Nazarene", facility), facility:='Bhalekane Nazarene  Clinic']
dt[facility=='EMKHUZWENI H C', facility:='Emkhuzweni Health Centre']
dt[grepl("Ezulwini", facility), facility:='Ezulwini Satellite Clinic']
dt[grepl("herefo", tolower(facility)), facility:='Herefords  Clinic']
dt[grepl("Hhukw", facility), facility:='Hhukwini Clinic']
dt[grepl("Horo", facility), facility:='Horo Clinic']
dt[grepl("Hlathikhulu", facility) & grepl("Hospital", facility), facility:='Hlatikhulu Hospital']

dt[grepl("mar", tolower(facility)) & grepl("st", tolower(facility)), facility:='St Mary\'S Clinic']

# list of facilitiesin the data set
ft = dt[ ,.(facility = unique(facility))]
ft = ft[order(facility)]

dt[facility %in% hf$name, .(facility = unique(facility))][order(facility)]
dt[!(facility %in% hf$name), .(facility = unique(facility))][order(facility)]

View(ft[,.(facility = unique(facility))])
View(hf)

write.csv(hf, paste0(outDir, 'datim_lat_long.csv'))




# --------------------------------------------
# reformat the export file for use in PBI

# --------------------
# put the variables in the desired order - TB SERVICES TAB
# if (sheet==2) { dt_export = dt[ ,.(facility, location, region, sex, age, week, 
#                                    month, qtr, year = year(start_wk),
#                                    )]
# 
# # reset the variable names for PBI
# setnames(dt_export, names(dt_export), c('Facility', 'Location',
#        'Region', 'Sex', 'Age', 'Week', 'Month', 'Quarter', 'Year',
#        'Start Week', 'End Week', )
# 
# }

# --------------------
# put the variables in the desired order - TB SERVICES TAB
if (sheet==5) { dt_export = dt[ ,.(facility, location, region, sex, age, week, 
                                   month, qtr, year = year(start_wk),
                                   start_wk, end_wk, ppl_seen, on_tb_tx,
                                   eligible_for_screening, screened, 
                                   presumptive_tb, presum_tb_referred,       
                                   samples_collected, samples_sent_to_lab,
                                   results_received, bact_confirmed, 
                                   bact_confirmed_started_tx)]

# reset the variable names for PBI
setnames(dt_export, names(dt_export), c('Facility', 'Location',
                                        'Region', 'Sex', 'Age', 'Week', 'Month', 'Quarter', 'Year',
                                        'Start Week', 'End Week', 'People seen at the CCD', 
                                        'On TB treatment', 'Eligible for screening',
                                        'Screened', 'Presumptive TB', 'Presumptive TB - referred', 
                                        'Samples collected', 'Samples sent to the lab', 'Lab results received',
                                        'Bacteriologically confirmed', 'Confirmed and started treatment'))

# export a csv of the data 
write.csv(dt_export, paste0(outDir, 'tb_services_pbi.csv')) }

# --------------------

