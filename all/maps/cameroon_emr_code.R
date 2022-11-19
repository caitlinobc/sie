# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/9/2022
# Map of Cameroon EMR sites
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
library(jsonlite)
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/maps/'

# set the working directory for the shape file
setwd('C:/Users/ccarelli/Documents/data/maps/shape_files/')

# set output director
OutDir = paste0(dir, 'emr_maps/')

# ---------------------------------------
# import data and subset to malawi and tanzania

dt = data.table(read.csv(paste0(dir, 'datim_org_units/cameroon_emr_facilities.csv')))

# import the regional shape file

# --------------------
# read in the shape file 

# read in the shape file
shape = shapefile(paste0(dir, 'shape_files/region/cameroon_regions_hdx/cameroon_regions_hdx.shp'))
plot(shape)
coord = data.table(fortify(shape))
coord[ , id:=as.numeric(id)]

# add the regions for each polygon
names = data.table(cbind(region = shape@data$ADM1_EN,
                         id = c(0,seq(1:9))))

names[ , id:=as.numeric(id)]
# 
# names[ , id:=gsub('CM0', '', id)]
# names[region!='South-West' , id:=gsub('0', '', id)]
# names[ ,id:=as.numeric(id)]
# # --------------------

# --------------------
# merge in the region names and check
coord = merge(coord, names, by='id')

# add a binary for the regions where egpaf works
coord[region=='Littoral' | region=='South' , egpaf:='EGPAF Implementation Area']
coord[region!='Littoral' & region!='South' , egpaf:='Other IP']


ggplot(coord, aes(x=long, y=lat, group=group, fill=egpaf)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_manual(values = c('#41ab5d', '#d9f0a3'), na.value = '#f0f0f0')+
  geom_point(data = dt, aes(x = long, y =lat), 
             size = 4, colour = '#ffc641', alpha= 0.9, inherit.aes=FALSE)+
  theme_void(base_size = 14) +
  labs(title="", fill="")

















# --------------------








