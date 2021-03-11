# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# Function to diagnose areas that do not match
# Compared EGPAF admin levels to shape files
# creates a list of "unmatching" administrative areas
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pbi_maps/sample_map/'

# set the output directory
OutDir = paste0(dir, 'outputs/' )

# set the directory for the DATIM facility coordinates
pep_dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/'

# set the working directory for the shape file
setwd(dir)

# --------------------
# import the data 
dt = data.table(read.csv(paste0(dir, 'esw_test_data_district.csv')))

# import the list of pepfar facilities
pp = data.table(readRDS(paste0(pep_dir, 
            'prepped/datim_health_facilities.rds')))

# --------------------
# import the shape file, check, and fortify

# import and plot the shape file
shape = readRDS(paste0(dir, 'gadm36_ESW_2_sp.rds')) 
plot(shape)

# list the specific districts
shp = fortify(shape, region = paste0('GID_', level))

# --------------------
# determine the shape names for the merge

# create a list of names for the districts to add to the shape file
# if statement based on shape file administrative level
if (level==1) {names = data.table(cbind(district = shape@data$NAME_1,
    id = shape@data$GID_1))} else if (level==2) {
      names = data.table(cbind(district = shape@data$NAME_2,
      id = shape@data$GID_2))} else {
        names = data.table(cbind(district = shape@data$NAME_3,
       id = shape@data$GID_3))}

# if mapping at the regional level, use region for the column name
setnames(names, 'district', admin ) 

# merge in the names
shp = merge(shp, names, by = 'id', all.x = TRUE)

# ----------------------------------------------
# check if the names match


# REGIONAL CHECK
# is each region included in the data 
dt[ ,unique(region)] %in% shp[,unique(region)] 

# print the names that are in the data but not the shape file
shape_names = shp[,unique(region)]   
dt[!(region %in% shape_names), unique(region)]


# # is each region included in the data 
# dt[ ,unique(district)] %in% shp[,unique(district)]   
# 
# 
# # print the names that are in the data but not the shape file
# shape_names = shp[,unique(district)]   
# dt[!(district %in% shape_names), unique(district)]
# --------------------

# ----------------------------------------------
# map the pepfar org units

pp = pp[country=='CDI' & egpaf==TRUE]


pp[ , lat:=as.numeric(lat)]
pp[ , long:=as.numeric(long)]


ggplot(shp, aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#fff7fb')+
  geom_point(data = pp, aes(x = long, y =lat, color = region, group = factor(country)),
              inherit.aes = FALSE,
             size = 2, alpha = 0.5)+
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
  
  # fix diacritical marks in the shape file 
  # shp[ , district:=fix_diacritics(district)]
  
  
# ESWATINI
# no changes to region 
# changes to 8 districts:
  
  


