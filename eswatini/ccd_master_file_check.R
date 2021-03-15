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

# --------------------
# examine the TB Screening tab

length(dt)









