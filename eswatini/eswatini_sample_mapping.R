# ----------------------------------------------
# Caitlin O'Brien-Carelli
# Sample Map File for Eswatini
# Generates maps using sample data 
# 3/9/2022
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

# set the directory for the data 
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/pepfar_org_units/'

# set the working directory to the cameroon data
MapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps/shape_files/district/'

# set the working directory for the shape file
setwd(MapDir)

# set output director
outDir = paste0('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Eswatini/Geospatial Analysis/')

# ---------------------------------------
# import the locations of the health facilities
dt = data.table(read.csv(paste0(dir, 'prepped/esw_health_facilities.csv')))

# import and merge the accompanying data
hts = data.table(read.csv(paste0(outDir, 'esw_hts_sample.csv')))
setnames(hts, c('name', 'value', 'fq'))
hts = dcast(hts, name~fq)
setnames(hts, c('name', 'q1', 'q2', 'q3', 'q4'))
dt = merge(dt, hts, by = 'name', all.x=T)
# --------------------

# --------------------
# read in the shape file 
shape = shapefile('eswatini_districts_hdx/eswatini_districts_hdx.shp')
coord = data.table(fortify(shape))
names = data.table(cbind(region = c(shape@data$ADM2_EN), id =  c(0, seq(1:54))))
coord = merge(coord, names, by = 'id')
setnames(coord, 'region', 'district')
# --------------------

# --------------------
# First sample map - regions only

# sample map - subset by country, sites
ggplot(shape, aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#f7fbff')+
  theme_void(base_size = 14) +
  labs(title="Sample Map of eSwatini",
       subtitle = "Districts Displayed") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6),
        text=element_text(size=16)) 
# --------------------

# --------------------
# Second sample map - sites displayed

# sample map - subset by country, sites
ggplot(shape, aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#a1d99b')+
  geom_point(data = dt[egpaf_facility=="Yes"], 
             aes(x = long, y =lat, group = country), 
             size = 3, colour = '#feb24c', alpha= 0.5)+
  theme_void(base_size = 14) +
  labs(title="Sample Map of eSwatini",
       subtitle = "Sites displayed on districts") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6),
        text=element_text(size=16),
        legend.position="none")

# --------------------

# ----------------------------------------------
# PDF MAPS
# ----------------------------------------------

# --------------------
# sample map 1 - example of sites on districts
m1 = ggplot(shape, aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_gradientn(colors = '#addd8e')+
  geom_point(data = dt[egpaf_facility=="Yes"], 
             aes(x = long, y =lat, group = country), 
             size = 3, colour = '#feb24c', alpha= 0.9)+
  theme_void(base_size = 14) +
  labs(title="Sample Map of eSwatini",
       subtitle = "Sites displayed on districts") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6),
        text=element_text(size=16),
        legend.position="none")

# --------------------

# --------------------
# sample map 2 - hts data 

m2 = ggplot(shape, aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01, color = '#737373')+
  scale_fill_gradientn(colors = '#ccebc5')+
  scale_color_manual(values = c('#238b45', '#feb24c'))+
  geom_point(data = dt[egpaf_facility=="Yes"], 
             aes(x = long, y =lat, group = country, 
            color = region, size = q4), 
            alpha = 0.9)+
  theme_void(base_size = 14) +
  guides(fill = FALSE)+
  labs(color = 'Region', size = "Tests Performed")+
  labs(title="HIV testing by site (HTS_TST), Q4 FY21",
       subtitle = "Size corresponds to testing volume") +
  theme(text=element_text(size=16))

# --------------------
# sample map 3 - hts data over quarters

# subset data and reshape long
dt2 = dt[egpaf_facility=='Yes', c('name', 'district', 'region', 'country',
       'lat', 'long', 'q1', 'q2', 'q3', 'q4')]
dt2 = melt(dt2, id.vars = c('name', 'district', 'region', 'country',
                      'lat', 'long'))

dt2[variable=='q1', variable:='Q1 FY21']
dt2[variable=='q2', variable:='Q2 FY21']
dt2[variable=='q3', variable:='Q3 FY21']
dt2[variable=='q4', variable:='Q4 FY21']

# create a long shape file
coord2 = copy(coord)
coord3 = copy(coord)
coord4 = copy(coord)
coord[ , variable:='Q1 FY21']
coord2[ , variable:='Q2 FY21']
coord3[ , variable:='Q3 FY21']
coord4[ , variable:='Q4 FY21']
coord = rbind(coord, coord2, coord3, coord4)

m3 = ggplot(coord, aes(x=long, y=lat, group=group, fill=1)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  facet_wrap(~variable)+
  geom_path(size=0.01, color = '#737373')+
  scale_fill_gradientn(colors = '#ccebc5')+
  scale_color_manual(values = c('#238b45', '#feb24c'))+
  geom_point(data = dt2, 
             aes(x = long, y =lat, group = country, 
                 color = region, size = value), 
             alpha = 0.9)+
   theme_void(base_size = 14) +
  guides(fill = FALSE)+
  labs(color = 'Region', size = "Tests Performed")+
  labs(title="HIV testing by site (HTS_TST), Q4 FY21",
       subtitle = "Size corresponds to testing volume") +
  theme(text=element_text(size=16))
# --------------------

# --------------------
# create a district level map
dt3 = dt2[ , .(value = sum(value)), by = .(district, variable)]
coord_hts = merge(coord, dt3, by = c('district', 'variable'), all = T)

m4 = ggplot(coord_hts, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  facet_wrap(~variable)+
  geom_path(size=0.01, color = '#737373')+
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), 
                       na.value = '#f0f0f0')+
  theme_void(base_size = 14) +
  labs(title="HIV testing by district (HTS_TST)",
       fill = 'HTS_TST') +
  theme(text=element_text(size=16))
# --------------------

# --------------------
# OUTPUT PDF SAMPLE MAPS 

pdf(paste0(outDir, 'sample_maps.pdf'), width = 12, height = 9)

m1
m2
m3
m4

dev.off()
# --------------------





