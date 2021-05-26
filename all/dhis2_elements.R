# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# Examine the variables in the Data Lake for PrEP information
# 5/3/21
# --------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace

# load the packages
library(data.table)
library(tools)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(openxlsx)

# --------------------

# --------------------------------------------

# set the main directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/prep/'

# import the list of variables in the data lake
dt = data.table(read_xlsx(paste0(dir, 'dhis2DataElement_2021-05-05.xlsx')))

# source the function that fixes diacritical marks
source('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Documents/GitHub/sie/all/fix_diacritics_function.r')

# --------------------------------------------

# --------------------------------------------
# format the data table for ease of use

# rename the variables
setnames(dt, c('source_id', 'country', 'data_set', 'element_id', 'variable',
               'frequency', 'inform_id', 'data_element'))

# create a data element that does not include special characters
# this function simply drops accent marks, etc. from the data element
dt[ , flat_element:=fix_diacritics(data_element)]

# --------------------------------------------
# explore the data set 

dt[, unique(country)]
dt[, unique(frequency)]

# --------------------

# --------------------------------------------
# search for indicators involving prep and tag them
dt[grepl("prep", tolower(flat_element)) | grepl("prep", tolower(flat_element)), prep:=TRUE]

# drop indicators that include the word "prepare" but are not prep-related
dt[grepl("prepare", tolower(flat_element))] # no variables include prepare
dt[grepl("prepare", tolower(flat_element))] # 8 elements include prepare
dt[grepl("preparation", tolower(flat_element))] # 4 additional elements include preparation
dt[grepl("prepare", tolower(flat_element)) | grepl("preparation", tolower(data_element)) |  is.na(prep), prep:=FALSE]

# check that the short name variable does not include information that the element does not
dt[grepl('prep', tolower(variable)) & prep==FALSE] # should equal 0

# --------------------
# export the prep-related elements

write.csv(dt[prep==TRUE], paste0(dir, 'prep_elements.csv'))

# --------------------

# --------------------------------------------
# search for indicators involving advanced hiv disease and tag them

# mentions ahd specifically
dt[grepl("advanced", tolower(flat_element)), ahd:=TRUE] # one element - not sure if ahd-related
dt[grepl("advanced", tolower(variable)), ahd:=TRUE]
dt[grepl("ahd", tolower(flat_element)), ahd:=TRUE] #adds nothing due to variable inclusion

# cd4 count
dt[grepl("cd4", tolower(flat_element)), cd4:=TRUE]
dt[grepl("cd4", tolower(variable)), cd4:=TRUE] #0


# mentions under 5
dt[grepl("<5", tolower(flat_element)), under5:=TRUE]
dt[grepl("< 5", tolower(flat_element))] #0
dt[grepl("under 5", tolower(flat_element))] #0
dt[grepl("5", tolower(flat_element))]


dt[grepl("4", tolower(flat_element))]
















