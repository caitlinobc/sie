# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# Import the tabs and aggregate the files
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/eswatini/ccd/prospective_data_raw/'


# output directory 
pdf_out = paste0(dir, 'outputs/tb_services_diagnostic_plots.pdf')

# output directory for prepped data 
outDir = paste0('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/eswatini/ccd/prepped/prospective_data/')

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
sheet = 'ART Indicators'

# --------------------------------------------
# import the data by sheetand by individual file

# list all of the weekly folders within the raw data 
weekly_folders = list.files(paste0(dir, 'april_may_2021/'))


# weekly_folders = weekly_folders[3]

# set the index for the run
i = 1

# extract the files for the weeekly folders and name
for (w in weekly_folders) {

  # list the specific files within each weekly folder
  weekly_files = list.files(paste0(dir, 'april_may_2021/', w, '/'))
  
  # loop through the weekly files within each folder and aggregate
  for (v in weekly_files) {
    dt = read.xlsx(paste0(dir, 'april_may_2021/', w, '/', v), sheet = sheet)
    print(v)
    print(length(dt))
    
    # make the file into a data table
    dt = data.table(dt)
    
    # subset to the correct number of columns
    if (sheet=='ART Indicators') dt = dt[,1:30]
    
    # add identifying information for each file
    dt[ , folder:=w] # add the weekly folder name to confirm the week number
    dt[ , file_name:=v] # add the file name for data quality checks
    
    # rbind the data together
    if (i==1) full_data = dt
    if (1 < i) full_data = rbind(full_data, dt, use.names=FALSE)
    i = i+1
  }
  
  
}

# --------------------------------------------
# perform some data cleaning and brief checks

# explore the data
head(full_data, n = 2)
View(full_data)


if (sheet=='ART Indicators')  {setnames(full_data, c('region', 'facility', 'location',
               'week', 'sex', 'age', 'tx_curr_art_clients_refill_due',
               'cum_clients_enrolled', 'offered_chcd', 'accepted_enrolled',
               'exited_chcd', 'opted_out', 'died', 'transferred', 'stopped_tx',
               'ltfu', 'other', 'chcd_clt_due_for_refill', 'received_arvs_chcd', 
               'received_3mos_arvs', 'received_6mos_arvs', 'missed_chcd_appt',
               'missed_followed_up', 'missed_fu_reached', 'missed_fu_reapp',
               'missed_fu_reapp_received_arvs_chcd', 'missed_fu_reapp_received_arvs_facility',
               'vl_eligible', 'vl_chcd', 'vl_chcd_received_results', 'folder', 'file_name'))

# drop out the rows that entirely consist of missing data 
full_data = full_data[!(is.na(location) & is.na(week) & is.na(tx_curr_art_clients_refill_due))] 


}

# --------------------------------------------
# output the file as a csv to run the prep on
write.csv(full_data, paste0(outDir, sheet, '.csv' ))

