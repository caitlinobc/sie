# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# Clean CCD data and run data quality checks
# uses the CCD Master File
# 3/15/2021
# --------------------------------------------

# --------------------
# Set up R

rm(list=ls()) 

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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/eswatini/ccd/'

# get the sheets from the data 
excel_sheets(paste0(dir, 'master_file/Reviewed DDD Reporting Template 29 Sept.xlsx'))

# output directory for pdfs
pdf_out = paste0(dir, 'outputs/tb_services_diagnostic_plots.pdf')

# directory for longitude and latitude and shape files
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps/'

# output directory for prepped data 
outDir = paste0(dir, 'prepped/')

# --------------------
# set the argument for the sheet you want to import 

# 1. "Indicators Data Sources" - data dictionary; not data
# 2. "ART Indicators"     
# 3. "PrEP Indicators " #no current data   
# 4. "TB Indicators "         
# 5. "TB Screening"         
# 6. "Commodities"           
# 7. "Sheet2" - does not contain data; only clinic list from old template              

# import sheets by number or by name
sheet = 5

# do you want to aggregate all of the csvs into a single file?
create_master = TRUE

# --------------------------------------------
# import the data by sheet 

dt = data.table(read.xlsx(paste0(dir,
    'master_file/Reviewed DDD Reporting Template 29 Sept.xlsx'),
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
print(dt[ ,unique(X31)]) # this column appears to contain notes
print(dt[ ,unique(X32)]) # this column appears to be a pivot table for internal use
}

# drop calculation columns to solely include variables
if (sheet==2) dt = dt[ , 1:30]

# --------------------
# Tab 5 -TB Services has multiple extraneous columns (drop)

# check that these columns do not contain data 
i = 1
for (i in i:length(dt)) {
  all_missing = all(is.na(dt[ ,..i]))
  if (all_missing==FALSE) print(paste0(i, " contains data!")) 
  if (all_missing==TRUE) print(paste0(i, ": Houston, we have a problem!")) 
  i = i+1 }

# only 17 of the 16,285 columns actually contain data on sheet 5
# subset to the first 17 columns
if (sheet==5) dt = dt[ , 1:17] # corrected in most recent file

# there is an extra column with no data on sheet 6
if (sheet==6) dt = dt[ , 1:11]

# --------------------

# --------------------------------------------
# rename the columns for usability

# save the original names for the output file
original_names = data.table(og = names(dt))

# art indicators - sheet 2
if (sheet==2) setnames(dt, c('region', 'facility', 'location',
      'week', 'sex', 'age', 'tx_curr_art_clients_refill_due',
      'cum_clients_enrolled', 'offered_chcd', 'accepted_enrolled',
      'exited_chcd', 'opted_out', 'died', 'transferred', 'stopped_tx',
      'ltfu', 'other', 'chcd_clt_due_for_refill', 'received_arvs_chcd', 
      'received_3mos_arvs', 'received_6mos_arvs', 'missed_chcd_appt',
      'missed_followed_up', 'missed_fu_reached', 'missed_fu_reapp',
      'missed_fu_reapp_received_arvs_chcd', 'missed_fu_reapp_received_arvs_facility',
      'vl_eligible', 'vl_chcd', 'vl_chcd_received_results'
))

# prep indicators - sheet 3
if (sheet==3) setnames(dt, c('region', 'facility', 'location',
                             'week', 'sex', 'age', 
          'prep_due_for_refill','received_prep_in_chcd', 'cumulative_clients_enrolled',
          'offerd_chcd', 'accepted_chcd', 'exited_chcd', 'no_longer_at_risk',
          'no_longer_want_prep', 'pers_side_effects_adr', 'seroconversion', 'transferred_out',
          'ltfu', 'due_for_refill_chcd', 'received_prep_via_chcd', 'tested_for_hiv',
          'tested_hiv_pos', 'tested_hiv_neg', 'missed_chcd_appts', 'missed_followed_up',
          'missed_followed_up_reached', 'missed_followed_up_re_appt', 
          'missed_fu_re_appt_received_medicines'
))

# tb indicators - sheet 4
if (sheet==4) setnames(dt, c('region', 'facility', 'location',
      'week', 'sex', 'age', 'tb_clients_enrolled_prev_mo', 'additional_offered_ccd',
      'accepted_enrolled_mo', 'total_tb_pts_booked', 'tb_pts_refilled_tb_art_cdp',
      'tb_pts_refilled_tb_only_cdp', 'tb_pts_missed_appts', 'opted_out', 'ltfu', 'death', 'back_to_facility_compl',
      'back_to_facility_completed_tx', 'transferred', 'back_to_facility_monitoring',
      'other'))

# tb screening - sheet 5
if (sheet==5) setnames(dt, c('region', 'facility', 'location',
    'week', 'sex', 'age', 'ppl_seen', 'on_tb_tx', 'eligible_for_screening',
    'screened', 'presumptive_tb', 'presum_tb_referred', 'samples_collected',
    'samples_sent_to_lab', 'results_received', 'bact_confirmed', 
    'bact_confirmed_started_tx'
))

# commodities - sheet 6
if (sheet==6) setnames(dt, c('region', 'facility', 'location',
           'week', 'sex', 'age', 'ipt_initiations', 'ipt_refills',
           'hivst', 'fp', 'condoms'))

# check that the names match
check_names = cbind(original_names$og, dt_names = names(dt))
# View(check_names) # to ensure the new names match the old

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
dt[location=='Foot Ball Pitch', location:='Football Pitch']
dt[location=='Shops', location:='Shop']
dt[location=='under a Tree', location:='Under a Tree']

# facility and location OK
# one week has incorrect formatting - drop from the data set
# one week is formatted correctly but has no associated dates
# these entries have associated values so ideally fix 
dt = dt[!is.na(week)]

# fix missing location in art indicators
if (sheet==2) dt = dt[!is.na(facility)] # facility missing, no associated values

# format and factor sex
dt[ , sex:=trimws(sex)]
dt$sex = factor(dt$sex, c('M', 'F'), c('Male', 'Female'))

# a few variables mysteriously load as a character - reformat
# these typically have " " rather than NA for missing data 
if(sheet==2)dt[ , missed_fu_reapp_received_arvs_chcd:=as.numeric(missed_fu_reapp_received_arvs_chcd)]
if(sheet==2) dt[ , missed_fu_reapp_received_arvs_facility:=as.numeric(missed_fu_reapp_received_arvs_facility)]
if(sheet==2) dt[ ,offered_chcd:=as.numeric(offered_chcd)]
if(sheet==2) dt[ ,vl_chcd:=as.numeric(vl_chcd)]
if(sheet==2) dt[ ,vl_chcd_received_results:=as.numeric(vl_chcd_received_results)]

if(sheet==5) dt[ , ppl_seen:=as.numeric(ppl_seen)] 

# --------------------
# fix typos in age categories and factor age

dt[grepl('Child', age) | grepl('14', age), age:='Child (1-14)']
dt[grepl('Adol', age) | grepl('15', age), age:='Adolescent (15-21)']
dt[grepl('Adult', age) | grepl('22', age), age:='Adult (22+)']

# --------------------
#establish dates

# drop out old template values
dt = dt[!grepl('Template', week)]

# there is a mistake in one weekly value for week 33 - fix so code applies
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

# fix weeks for files in which program did not exist
# for example, says january 2020 but means january 2021
if (sheet==2) dt[start_wk=='2020-02-17', start_wk:=as.Date('2021-02-15')]
if (sheet==5 | sheet==6) {
dt[start_wk=='2020-01-13', start_wk:=as.Date('2021-01-18')]
dt[end_wk=='2020-01-19', end_wk:=as.Date('2021-01-24')] }

# fix the years to go with the amended start weeks
dt[ , start_year:=year(start_wk)]
dt[month(start_wk)!=12, end_year:=start_year] # except in december 

# drop extraneous variables
dt[ ,c('start_date', 'end_date', 'start_year', 'end_year'):=NULL]

# add monthly date 
dt[ , month:=paste0(year(start_wk), '-', month(start_wk), '-01')]
dt[ , month:=as.Date(month, '%Y-%m-%d')]

# add quarter
dt[month(start_wk) %in% c(10:12), qtr:=1]
dt[month(start_wk) %in% c(1:3), qtr:=2]
dt[month(start_wk) %in% c(4:6), qtr:=3]
dt[month(start_wk) %in% c(7:9), qtr:=4]

# exception - one week in FY21 begins in september
# no instances observed so far
dt[month(start_wk)==9 & month(end_wk)==10, qtr:=1]

# check each start week has a single associated quarter
test_q = dt[ ,.(start_wk = unique(start_wk)), by = qtr]
test_q[ , count:=.N, by = start_wk] # count should equal 1

# set quarter as a text variable including fiscal year
dt['2020-09-30' <= start_wk, fy:=21]
dt[start_wk < '2020-09-30', fy:=20]

# format quarter name
dt[ , qtr:=paste0('Q', qtr, ' FY', fy)]
dt[ , fy:=NULL] # drop fiscal year (included in quarter)

#------------------
# add a quarterly sorting variable

# fiscal year 2020
dt[qtr=='Q1 FY20', qtr_st:=as.Date('2019-10-01')]
dt[qtr=='Q2 FY20', qtr_st:=as.Date('2020-01-01')]
dt[qtr=='Q3 FY20', qtr_st:=as.Date('2020-04-01')]
dt[qtr=='Q4 FY20', qtr_st:=as.Date('2020-07-01')]

# fiscal year 2021
dt[qtr=='Q1 FY21', qtr_st:=as.Date('2020-10-01')]
dt[qtr=='Q2 FY21', qtr_st:=as.Date('2021-01-01')]
dt[qtr=='Q3 FY21', qtr_st:=as.Date('2021-04-01')]
dt[qtr=='Q4 FY21', qtr_st:=as.Date('2021-07-01')]

# --------------------
# logic checks

if (sheet==5) {
dt[ppl_seen < on_tb_tx] # 4
dt[ppl_seen < eligible_for_screening] # 4
dt[ppl_seen < screened] # 3
dt[ppl_seen < presumptive_tb] # 9 
}

# --------------------------------------------
# FACILITY NAME STANDARDIZATION

# --------------------
# import the list of datim health facilities
hf = data.table(readRDS(paste0(mapDir, 'pepfar_org_units/prepped/datim_health_facilities.rds')))

# subset to egpaf facilities in eswatini
hf = hf[country=='ESW' & egpaf==TRUE]

# drop unecessary variables for PBI
hf[ ,c('level', 'type', 'sub_dist', 'country', 'egpaf'):=NULL]

# --------------------
# facility names entered incorrectly in the CCD data 

# create an original facility variable to track changes
dt[ ,original_name:=facility]

# alter the facility names to match DATIM names
dt[ , facility:=gsub("Center", "Centre", facility)]
dt[grepl("ane Nazarene", facility), facility:='Bhalekane Nazarene  Clinic']
dt[facility=='EMKHUZWENI H C', facility:='Emkhuzweni Health Centre']
dt[grepl("Ezulwini", facility), facility:='Ezulwini Satellite Clinic']
dt[grepl("herefo", tolower(facility)), facility:='Herefords  Clinic']

dt[grepl("Hhukw", facility), facility:='Hhukwini Clinic']
dt[grepl("Horo", facility), facility:='Horo Clinic']
dt[grepl("Hlathikhulu", facility) & grepl("Hospital", facility), facility:='Hlatikhulu Hospital']
dt[grepl("Lobamba", facility), facility:='Lobamba Clinic']
dt[grepl("mahwalala", tolower(facility)), facility:='Mahwalala Clinic']

dt[grepl("mangweni", tolower(facility)), facility:='Mangweni Clinic']
dt[grepl("mar", tolower(facility)) & grepl("st", tolower(facility)), facility:='St Mary\'S Clinic']
dt[grepl("maguga", tolower(facility)), facility:='Maguga Clinic ']
dt[grepl("malandzela", tolower(facility)), facility:='Malandzela Nazarine Clinic']
dt[grepl("matsanjeni", tolower(facility)), facility:='Matsanjeni Health Center']

dt[grepl("mbabane", tolower(facility)) & grepl("gov", tolower(facility)),
   facility:='Mbabane Government Hospital']
dt[grepl("mbabane p", tolower(facility)), facility:='Mbabane PHU']
dt[grepl("mgazini", tolower(facility)), facility:='Mgazini Nazarene clinic']
dt[grepl("mhlosheni", tolower(facility)), facility:='Mhlosheni Clinic ']
dt[grepl("moti", tolower(facility)), facility:='Moti Clinic']

dt[facility=='Motshane ART Clinic', facility:='Motshane Clinic']
dt[grepl('Mshingishin', facility), facility:='Mshingishingini Nazarine Clinic']
dt[grepl('wabangen', facility), facility:='Ndvwabangeni Nazarine Clinic']
dt[grepl('nkhaba', tolower(facility)), facility:='Nkaba Clinic']
dt[facility=='Ndzingeni Clinic', facility:='Ndzingeni Nazarine Clinic']

dt[grepl('new haven', tolower(facility)), facility:='New Heaven Clinic']
dt[grepl('nkwene', tolower(facility)), facility:='Nkwene Clinic']
dt[grepl('ntjanini', tolower(facility)), facility:='Ntshanini Clinic']
dt[grepl('ngowane', tolower(facility)), facility:='Ngowane Clinic']

dt[grepl('nhletjeni', tolower(facility)), facility:='Nhletjeni Clinic']
dt[facility=='Ntfonjeni clinic', facility:='Ntfonjeni Clinic']
dt[facility=='Nyonyane clinic', facility:='Nyonyane Clinic']

dt[grepl("olos", tolower(facility)), facility:='Our Lady of Sorrow (OLOs)  clinic']
dt[facility=='Piggs Peak Hospital' | facility=='Pigg\'s Peak Hospital',
   facility:='Pigg\'S Peak Hospital']
dt[facility=='Piggs Peak PHU', facility:='Piggs Peak Public Health Unit']
dt[facility=='Piggs Peak Correctional Clinic', facility:='Piggs Peak Correctional Services Clinic']
dt[facility=='Piggs Peak Nazarene Clinic', facility:='Pigg\'S Peak Nazarine Clinic']

dt[grepl("regina", tolower(facility)), facility:='Regina Mundi Clinic']
dt[grepl("sidwash", tolower(facility)) & grepl("correct", tolower(facility)), 
   facility:='Sidwashini Correctional Services Clinic']
dt[grepl("sigang", tolower(facility)), facility:='Sigangeni Clinic']
dt[grepl("silel", tolower(facility)), facility:='Silele Clinic']

# A total of either 3 or 4 (two may be the same) sites not found in DATIM
dt[facility=='Salvation Army Clinic', facility:='Msunduza Salvation Army ']
dt[facility=='Phunga Nazarene', facility:='Ka-Phunga Nazarene Clinic']
dt[facility=='Phunga Clinic', facility:='Ka-Phunga Gov Clinic']
dt[facility=='AHF Piggs Peak ', facility:='AHF Piggs Peak']
dt[facility=='Mkhuzweni HC', facility:='Emkhuzweni Health Centre']
dt[facility=='AHF PPK Clinic', facility:='AHF Piggs Peak']

# check that every facility matched
dt[!(facility %in% hf$name), unique(facility)]

# --------------------
# add orgUnitID to the data set match on ID in PBI

ids = hf[ ,.(facility = name, id)]
dt = merge(dt, ids, by = 'facility', all.x=TRUE)

# --------------------
# export a list of facilities with geographic data for use in PBI

# alter the names to match the future data 
setnames(hf, c('orgUnitID', 'Facility', 'Parent ID', 'District', 'Region',
               'Latitude', 'Longitude'))

# export the list of facilities in the data
write.csv(hf, paste0(outDir, 'List of DATIM Sites.csv'))

#------------------

# --------------------------------------------
# drop duplicate entries

dt = dt[!duplicated(dt)]

# 2 - 7 rows dropped
# 3 - 48 rows dropped
# 4 - 8 rows dropped
# 5 - 1 rows drop9ed
# 6 - 18 rows dropped

# --------------------------------------------

# --------------------------------------------
# reformat the export file for use in PBI

# --------------------
# put the variables in the desired order - ART INDICATORS
if (sheet==2) {
  dt_export = dt[ ,.(facility, id, location, region, sex, age, week,
                  month, qtr, qtr_st, year = year(start_wk), start_wk, end_wk,
                  tx_curr_art_clients_refill_due, 
                  cum_clients_enrolled, offered_chcd,                          
                  accepted_enrolled, exited_chcd, opted_out,                           
                  died, transferred, stopped_tx, ltfu, other,
                  chcd_clt_due_for_refill, received_arvs_chcd,                    
                  received_3mos_arvs, received_6mos_arvs, missed_chcd_appt,                      
                  missed_followed_up, missed_fu_reached, missed_fu_reapp,                      
                  missed_fu_reapp_received_arvs_chcd, 
                  missed_fu_reapp_received_arvs_facility, 
                  vl_eligible, vl_chcd, vl_chcd_received_results)] 

# reset the variable names for PBI
setnames(dt_export, names(dt_export), c('Facility', 'orgUnitID', 'Location',
      'Region', 'Sex', 'Age', 'Week', 'Month', 'Quarter', 'Quarter Start', 'Year',
      'Start Week', 'End Week', 'ART clients due for a refill (TX_CURR)',
      'Cumulative clients enrolled in CCD', 'Clients offered CCD', 
      'Accepted or enrolled in CCD', 'Exited CCD', 'Opted out',
      'Deceased', 'Transferred out', 'Stopped treatment', 'LTFU', 'Other',
      'CCD client due for a refill', 'Received ARVs in the CCD',
      'Received 3 mos of ARVs', 'Received 6 mos of ARVs', 'Missed CCD appt',
      'Missed and followed up', 'Missed, followed up, and reached', 
      'Missed, followed up, and re-appointed', 'Missed, re-appointed, received ARVs by CCD',
      'Missed, re-appointed, received ARVs in the facility',
      'Eligible for VL test', 'Clients who submitted a VL test via CCD',
      'Clients who submitted a VL test via CCD and received the results'))

# export the file for PBI
write.csv(dt_export, paste0(outDir, 'ART Indicators.csv'), na = "") 

}

# --------------------
# put the variables in the desired order - PREP INDICATORS
if (sheet==3) {
  dt_export = dt[ ,.(facility, id, location, region, sex, age, week,
                     month, qtr, qtr_st, year = year(start_wk), start_wk, end_wk, prep_due_for_refill, received_prep_in_chcd,               
                     cumulative_clients_enrolled, offerd_chcd, accepted_chcd, exited_chcd,
                     no_longer_at_risk, no_longer_want_prep, pers_side_effects_adr, seroconversion,                      
                     transferred_out, ltfu, due_for_refill_chcd, received_prep_via_chcd,           
                     tested_for_hiv, tested_hiv_pos, tested_hiv_neg, missed_chcd_appts,                
                     missed_followed_up, missed_followed_up_reached, missed_followed_up_re_appt, missed_fu_re_appt_received_medicines)] 
  
  # reset the variable names for PBI
  setnames(dt_export, names(dt_export), c('Facility', 'orgUnitID', 'Location',
                                          'Region', 'Sex', 'Age', 'Week', 'Month', 'Quarter', 'Quarter Start', 'Year',
                                          'Start Week', 'End Week','Due for a refill', 'Received PrEP in CHCD', 
                                          'Cumulative clients enrolled', 'Offered CCD', 'Accepted or enrolled in CCD', 'Exited the CCD option', 
                                          'No longer at risk', 'No longer wante PrEP', 'Persistent side effects/ADR', 'Seroconversion',
                                          'Transferred out', 'LTFU', 'Due for a refill - CCD', 'Received PrEP in the CHCD',
                                          'Tested for HIV', 'Tested HIV+', 'Tested HIV-', 'Missed CCD appt',
                                          'Missed and followed up', 'Missed, followed up, reached', 'Missed, followed up, re-appt.',
                                          'Missed, followed up, received medicines'
                                          ))
  
  # export the file for PBI
  write.csv(dt_export, paste0(outDir, 'PrEP Indicators.csv'), na = "") 
  
}

# --------------------
# put the variables in the desired order - COMMODITIES TAB
if (sheet==4) { dt_export = dt[ ,.(facility, id, location, region, sex, age, week, 
         month, qtr, qtr_st, year = year(start_wk), start_wk, end_wk,
         tb_clients_enrolled_prev_mo, additional_offered_ccd,
         accepted_enrolled_mo, total_tb_pts_booked, tb_pts_refilled_tb_art_cdp,
         tb_pts_refilled_tb_only_cdp, tb_pts_missed_appts, opted_out,
          ltfu, death, back_to_facility_compl,
          back_to_facility_completed_tx, transferred, 
          back_to_facility_monitoring, other)]

# reset the variable names for PBI
setnames(dt_export, names(dt_export),
         c('Facility', 'orgUnitID', 'Location',
           'Region', 'Sex', 'Age', 'Week', 'Month', 'Quarter', 'Quarter Start', 'Year',
           'Start Week', 'End Week', 'TB clients already enrolled in the previous month',
           'Offered CCD', 'Accepted and Enrolled', 'TB Patients Booked', 'TB Patients Refilled TB & ART',
           'TB Patients Refilled TB Tx Only', 'TB Patients Missed Appts', 'Opted Out', 'LTFU',
           'Died', 'Returned to Facility - Complications', 'Returned to Facility - Completed Tx/Cured',
           'Transferred', 'Returned to Facility - Clinical Monitoring', 'Other'))

# export a csv of the data 
write.csv(dt_export, paste0(outDir, 'TB Indicators.csv'), na = "") }

# --------------------
# put the variables in the desired order - TB SCREENING TAB
if (sheet==5) { dt_export = dt[ ,.(facility, id, location, region, sex, age, week, 
                                   month, qtr, qtr_st, year = year(start_wk),
                                   start_wk, end_wk, ppl_seen, on_tb_tx,
                                   eligible_for_screening, screened, 
                                   presumptive_tb, presum_tb_referred,       
                                   samples_collected, samples_sent_to_lab,
                                   results_received, bact_confirmed, 
                                   bact_confirmed_started_tx)]


# fix single value - all other values 0; one missing field (must be 0)
# sample collected = NA; samples sent to the lab = 0
dt_export[is.na(samples_collected), samples_collected:=0]
dt_export[!is.na(Age)] # drop this errant row

# reset the variable names for PBI
setnames(dt_export, names(dt_export),
         c('Facility', 'orgUnitID', 'Location',
         'Region', 'Sex', 'Age', 'Week', 'Month', 'Quarter', 'Quarter Start', 'Year',
          'Start Week', 'End Week', 'People seen at the CCD', 
            'On TB treatment', 'Eligible for screening',
              'Screened', 'Presumptive TB', 'Presumptive TB - referred', 
                'Samples collected', 'Samples sent to the lab', 'Lab results received',
                  'Bacteriologically confirmed', 'Confirmed and started treatment'))

# export a csv of the data 
write.csv(dt_export, paste0(outDir, 'TB Screening.csv'), na = "") }

# --------------------
# put the variables in the desired order - COMMODITIES TAB
if (sheet==6) { dt_export = dt[ ,.(facility, id, location, region, sex, age, week, 
                                   month, qtr, year = year(start_wk),
                                   start_wk, end_wk, ipt_initiations, ipt_refills, 
                                   hivst, fp, condoms, qtr_st)]

# reset the variable names for PBI
setnames(dt_export, names(dt_export),
         c('Facility', 'orgUnitID', 'Location',
           'Region', 'Sex', 'Age', 'Week', 'Month', 'Quarter','Year',
           'Start Week', 'End Week', 'IPT Initiations', 'IPT Refills',
           'HIVST', 'Family Planning', 'Condoms', 'Quarter Start'))

# export a csv of the data 
write.csv(dt_export, paste0(outDir, 'Commodities.csv'), na = "") }

# --------------------

# --------------------------------------------
# CREATE DATA SETS FOR R
#
# dt is preserved, as PBI files are formatted on dt_export

# --------------------
# organize the variables in a preferred order 

# --------------------
# shape the data long for visualization

# create the list of id variables
idVars = c('original_name','facility', 'id', 'region', 
           'sex', 'age', 'location', 'week', 'start_wk', 'end_wk',
           'month', 'qtr')

# shape the data long using the id variables
dt_long = melt(dt, id.vars = idVars)

# use a set name in case you combine the data sets
if (sheet==2) set = 'art'
if (sheet==3) set = 'prep'
if (sheet==4) set = 'tb_indic'
if (sheet==5) set = 'tb_screening'
if (sheet==6) set = 'commodities'

dt_long[ , set:=set]

# --------------------
# export the data

saveRDS(dt_long, paste0(outDir, 'r_files/', set, '.rds'))

# --------------------
# -------------------------------------------

# -------------------------------------------
# Aggregate all of the master files into a single workbook

if (create_master==TRUE) {
  files = list.files(paste0(outDir, 'r_files/'))
  i = 1
  for (f in files) {
    tab_dt = readRDS(paste0(outDir, 'r_files/', f))
    if (i==1) full_rds = tab_dt
    if (1 < i) full_rds = rbind(full_rds, tab_dt)
    i = i+1
  }
  write.csv(full_rds, paste0(outDir, 'full_ccd_data.csv'))
  saveRDS(full_rds, paste0(outDir, 'full_ccd_data.rds'))
}

# -------------------------------------------














