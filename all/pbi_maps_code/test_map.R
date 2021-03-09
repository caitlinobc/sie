# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# Create a working Eswaitni map for use in PBI
# Create Admin 1 and 2 maps 
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(broom)
library(ggrepel)
library(gridExtra)

# ----------------------------------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pbi_maps/'

# set the output directory
OutDir = paste0(dir, 'outputs/' )

# set the directory for the DATIM facility coordinates
pep_dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/'

# set the working directory for the shape file
setwd(paste0(dir, '/rds_shape_files/'))

# --------------------
# load the function that fixes diacritical marks
source('C:/Users/ccarelli/Documents/GitHub/sie/all/fix_diacritics_function.r')

# --------------------

# ----------------------------------------------
# import the data 

dt = data.table(read.csv(paste0(dir, 'pepfar_test_data/esw_test_data_district.csv')))

# import the list of pepfar facilities
pp = data.table(readRDS(paste0(pep_dir, 'prepped/datim_health_facilities.rds')))

# --------------------
# import the shape file, check, and fortify

# import and plot the shape file
shape = readRDS('gadm36_SWZ_2_sp.rds') # district level
plot(shape)

# list the specific districts
shp = data.table(fortify(shape, region = 'GID_2'))

# --------------------
# determine the shape names for the merge

# create a list of names
names = data.table((cbind(district = shape@data$NAME_2, id = shape@data$GID_2)))
shp = merge(shp, names, by = 'id', all.x = TRUE)

# fix diacritical marks in the shape file 
# shp[ , district:=fix_diacritics(district)]

# --------------------
# check if the names match

# is each region included in the data 
dt[ ,unique(district)] %in% shp[,unique(district)]   

# print the names that are in the data but not the shape file
shape_names = shp[,unique(district)]   
dt[!(district %in% shape_names), unique(district)]

# --------------------

# ----------------------------------------------
# map the pepfar org units

pp = pp[country=='ESW' & egpaf==TRUE]


pp[ , lat:=as.numeric(lat)]
pp[ , long:=as.numeric(long)]

egpaf_dists = dt[,unique(district)]


alt = data.table(country = 'ESW', lat = -27.1211, long = 31.2079)


ggplot(shp, aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#fff7fb')+
  geom_point(data = pp[grepl('Shis', district)], aes(x = long, y =lat, color = district, group = factor(country)),
              inherit.aes = FALSE,
             size = 2, alpha = 0.5)+
  geom_point(data = alt, aes(x = long, y =lat, group = factor(country)),
             inherit.aes = FALSE,
             size = 6, alpha = 0.5)+
  theme_void(base_size = 14) 


scale_fill_gradientn(colors = '#7fcdbb')
colour = '#253494'





  labs(title="AHD study facilities: Tanzania",
       subtitle = expression(italic("n = 9")), 
       caption = '*Locations jittered to show sites in similar locations') +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(face = "italic"),
        text=element_text(size=16),
        legend.position = 'none') 


# ----------------------------------------------
# code to change the shape file
  
  
  
# ESWATINI
# no changes to region 
# changes to 8 districts:
  
  


