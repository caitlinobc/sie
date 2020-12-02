# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/2/20
# Visualize vira load coverage and suppression
# All SIE supported countries for 12/7 data review
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(broom)
library(ggrepel)
library(gridExtra)
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/Documents/data/'

# set the working directory for the shape file
setwd('C:/Users/ccarelli/Documents/data/prepped/')

# set output director
OutDir = paste0(dir, 'outputs/')

# --------------------
# read in the shape files, district names, and data

shape = readRDS('shape_files_all_countries.rds')
names = readRDS('district_names_ids.rds')
dt = data.table(read.csv(paste0(dir, 'raw_data/vls_vlc_peds_q4_20_all_countries.csv')))

# ----------------------------------------
# FORMAT THE DATA

# --------------------
# rename the columns
setnames(dt, names(dt), c('district', 'vlc', 
                  'vls', 'vls_denom'))
# drop the first six rows that delineate the filters
dt = dt[-c(1:6)]

# drop the final row which represents a summary total
dt = dt[1:(nrow(dt) - 1)]

# drop the row that lacks administrative data
dt = dt[district!=""]

# drop the percentage signs and commas in counts
dt[ , vlc:=as.numeric(gsub('%', '', vlc))]
dt[ , vls:=as.numeric(gsub('%', '', vls))]
dt[, vls_denom:=as.numeric(gsub(',', '', vls_denom))] #some counts have commas

# --------------------
# format the admin levels

# add a label for the country level totals
countries = c('Cameroon','Cote d\'Ivoire', 'DRC', 'Kenya', 'Lesotho', 
              'Mozambique', 'Malawi', 'Eswatini', 'Tanzania', 'Uganda')
dt[district %in% countries, level:='country']
dt[level=='country', code:=seq(1:10)]

# create a series of codes to fill in country names
codes = data.table(cbind(country = dt[level=='country']$district, 
                         code = dt[level=='country']$code))
codes[ , code:=as.numeric(code)]

# fill in the countries for the data
dt[ , code:=nafill(code, type = 'locf')]
dt = merge(dt, codes, by = 'code', all.x=T)
dt[ , code:=NULL]

# --------------------
# format the districts
