# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# Sample map for MESO meeting
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace

# install the packages
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
pp = pp[country=='ESW']
pp[ ,lat:=as.numeric(lat)]
pp[ ,long:=as.numeric(long)]

View(dt)

# --------------------
# import the shape file, check, and fortify

# import and plot the shape file
shape = readRDS(paste0(dir, 'gadm36_ESW_2_sp.rds')) 
plot(shape)

# list the specific districts
shp = fortify(shape, region = 'GID_2')
shp = data.table(shp)

# --------------------
# add the names of the districts to the shape file
# this allows you to match the data and the shape file by district name

names = data.table(cbind(district = shape@data$NAME_2, id = shape@data$GID_2))
shp = merge(shp, names, by = 'id')

# --------------------

# ----------------------------------------------
# merge the data and the shape file

shp = merge(shp, dt, by ='district', all.x = TRUE)

# ----------------------------------------------


# ----------------------------------------------
# make nice maps

# map of q4 hts_tst
ggplot(shp, aes(x=long, y=lat, group=group, fill=fy20_q3)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = brewer.pal(9, 'Greens'), na.value = '#f0f0f0')+
  theme_void(base_size = 14) +
  labs(title="Number of people tested for HIV (HTS_TST), FY20 Q4")

# map of q4 hts_tst
ggplot(shp, aes(x=long, y=lat, group=group, fill=fy20_q4)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), na.value = '#f0f0f0')+
  geom_point(data = pp, aes(x = long, y =lat, group = country), 
              size = 4, colour = '#253494', alpha= 0.5, inherit.aes=FALSE)+
  theme_void(base_size = 14) +
  labs(title="Number of people tested for HIV (HTS_TST), FY20 Q4")


# map of q4 hts_tst
ggplot(shp, aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#c7eae5', na.value = '#f0f0f0')+
  geom_point(data = pp[egpaf==TRUE], aes(x = long, y =lat, group = country), 
             size = 2, colour = '#253494', alpha= 0.5, inherit.aes=FALSE)+
  theme_void(base_size = 14) +
  labs(title="EGPAF implementing facilities, FY20 Q4")+
  theme(legend.position = "none")


# ----------------------------------------------



