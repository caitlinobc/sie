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
sheets = excel_sheets(paste0(dir, 'master_file/Reviewed DDD Reporting Template 29 Sept (20).xlsx'))

# output directory for pdfs
pdf_out = paste0(dir, 'outputs/tb_services_diagnostic_plots.pdf')

# output directory for prepped data 
outDir = paste0(dir, 'prepped/')

# --------------------
# import the data by sheet 

# week 5, tb services
dt = data.table(read.xlsx(paste0(dir, 'master_file/Reviewed DDD Reporting Template 29 Sept (20).xlsx'),
                          sheet = 5))
# --------------------------------------------
