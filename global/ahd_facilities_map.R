# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/4/21
# Map health facilities in the AHD Study
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
library(readxl)
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/AHD/'

# set the working directory for the shape file
setwd('C:/Users/ccarelli/Documents/data/prepped/')

# set output director
OutDir = paste0(dir, 'outputs/')

# ---------------------------------------
# import data and subset

dt = data.table(read_xlsx(paste0(dir, 'data/global_facility_inventory.xlsx')))

# rename variables to usable format
setnames(dt, new = c('country','admin1', 'facility', 'facility_type', 
                     'owner', 'lat', 'long', 'll_source'))

# subset to the relevant countries
dt = dt[country=='Tanzania' | country=='Malawi']

# --------------------
# read in the shape file 

shape = readRDS('shape_files_all_countries.rds')
# shape = shape[country=='Malawi' |country=='Tanzania']

shape = shape[country=='Malawi']

ggplot(shape[country==c], 
       aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'),
                       na.value='#d9d9d9') + 
  theme_void(base_size = 14) +
  labs(title="Viral suppression ratio",
       subtitle = country_name,
       fill="Percent (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6),
        legend.title = element_text(vjust=2),
        text=element_text(size=16)) 


# facilities in the study


