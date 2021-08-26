# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/26/21
# 2021 OHA Meeting Analysis 
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(openxlsx)
library(ggplot2)
library(RColorBrewer)
library(raster)
# --------------------

# ----------------------------------------------
# Files and directories

# set the home directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/oha_21/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Global/Presentations/OHA Meeting 2021/'

# mapping director
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps/r_shape_files/'

# read in the data 
dt = data.table(read.csv(paste0(dir, 'oha_21_prep.csv')))


# ----------------------------------------------

# --------------------
# prep the data for analysis

# rename the variables for analysis 
setnames(dt, c('country', 'sex', 'age', 'prep_curr', 'prep_new', 'fq'))

# shape the data long for visualization
dt = melt(dt, id.vars = c('country', 'sex', 'age', 'fq'))

# --------------------
