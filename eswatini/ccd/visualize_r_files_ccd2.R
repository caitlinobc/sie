# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# Run DQ checks and descriptive stats on CCD data
# 5/3/21
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/eswatini/ccd/'

# output directory for pdfs
PDFout = paste0(dir, 'outputs/diagnostic_plots.pdf')

# output directory for prepped/aggregated data 
outDir = paste0(dir, 'prepped/')

# --------------------
# table of contents of files

# art: 2. "ART Indicators"     
# prep: 3. "PrEP Indicators " #no current data   
# tb_indic: 4. "TB Indicators "         
# tb_screening: 5. "TB Screening"         
# commodities: "Commodities"           

# --------------------------------------------

# --------------------
# import the files and create a single long file

# list the files
files = list.files(paste0(dir, 'prepped/r_files/'))

# read in the files and rbind
i = 1
for (f in files) {
  x = data.table(readRDS(paste0(dir, 'prepped/r_files/', f)))
  if (i==1) dt = x
  if (i!=1) dt = rbind(dt, x)
  i = i+1
}

# check that each set is included
dt[ ,unique(set)]

# --------------------




# --------------------------------------------


# --------------------
# export the final, aggregate file

# includes art indicators, commodities, and tb indicators and screening
write.csv(dt, paste0(outDir, 'full_ccd_data.csv'))

# --------------------




