# ----------------------------------------------
# Caitlin O'Brien-Carelli
# Sample Map File for Eswatini
# Generates maps using sample data 
# Creates maps for USAID 
# 7/7/2022
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/eswatini/usaid_maps/'

# set the working directory to the cameroon data
MapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/maps/shape_files/'

# set the working directory for the shape file
setwd(MapDir)

# set output director
outDir = paste0('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Eswatini/Geospatial Analysis/')

# ---------------------------------------
# import the locations of the health facilities
dt = data.table(read_excel(paste0(dir, 'site_locations.xlsx')))

# import and merge the accompanying data
hts = data.table(read.csv(paste0(dir, 'hts_data.csv')))
setnames(hts, c('Region', 'District', 'Site', 'HTS_TST'))
hts[ , c('Region', 'District'):=NULL]
dt = merge(dt, hts, by = 'Site', all.x=T)

# add a binary for working in the site
dt[ , Work:=TRUE]

# convert testing to a numeric
dt[ , HTS_TST:=as.numeric(HTS_TST)]
# --------------------

# --------------------
# read in the shape file - district level 
shape = shapefile('district/eswatini_districts_hdx/eswatini_districts_hdx.shp')
coord = data.table(fortify(shape))
names = data.table(cbind(region = c(shape@data$ADM2_EN), id =  c(0, seq(1:54))))
coord = merge(coord, names, by = 'id')
setnames(coord, 'region', 'district')
# --------------------

# --------------------
# read in the shape file - regions
shape2 = shapefile('region/eswatini_regions_hdx/eswatini_regions_hdx.shp')
coord2 = data.table(fortify(shape2))
names2 = data.table(cbind(region = c(shape2@data$ADM1_EN), id =  c(0, seq(1:3))))
coord2 = merge(coord2, names2, by = 'id')
# --------------------

# --------------------
# add the regions to the district map file 
regions = dt[ ,.(district = unique(District)), by = .(region = Region)]
coord = merge(coord, regions, by = 'district', all.x=TRUE)
# --------------------

# ---------------------------------------
# EDIT THE FILES TO ADD AESTHETICS
# ---------------------------------------

# --------------------
# factor the mother-baby pairs
dt$Mother = factor(dt$Mother, c('Mother', 'Baby'), c('Mother', 'Baby'))
# --------------------

# --------------------
# add a variable for donor
coord2[region=='Hhohho' | region=='Shiselweni', donor:='USAID Supported Regions']
coord2[is.na(donor), donor:='CDC Supported Regions']
coord2$donor = factor(coord2$donor, c('USAID Supported Regions', 'CDC Supported Regions'),
                      c('USAID Supported Regions', 'CDC Supported Regions'))
# --------------------

# --------------------
# add a size aesthetic for mother-baby pairs
dt[Mother=='Mother', size:=2]
dt[Mother=='Baby', size:=1]
# --------------------

# ---------------------------------------

# ----------------------------------------------
# REGIONAL MAPS 
# ----------------------------------------------

# ----------------------------------------------

# --------------------
# Regions in which EGPAF works - with sites

r1 = ggplot(coord2, aes(x=long, y=lat, group=group, fill=factor(donor))) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_manual(values = c('#4292c6', '#969696'), 
                    na.value = '#969696')+
  geom_point(data = dt, 
             aes(x = Longitude, y = Latitude, group = Work), 
             size = 3, colour = '#feb24c', alpha = 0.9, 
             inherit.aes = FALSE)+
  theme_void(base_size = 14) +
  labs(fill = '') +
  theme(plot.title=element_text(vjust=-1), 
        text=element_text(size=16)) 
# --------------------

# --------------------
# Regions in which EGPAF works - with mother/baby pairs

r2 = ggplot(coord2, aes(x=long, y=lat, group=group, fill=NA)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_manual(values = c('#4292c6', '#f0f0f0'), 
                    na.value = '#ffffff')+
  geom_point(data = dt, 
             aes(x = Longitude, y = Latitude, group = Work,
                 size = size, colour = Mother), 
             alpha = 0.9, show.legend = TRUE,
             inherit.aes = FALSE)+
  scale_size(guide = 'none')+
  theme_void(base_size = 14) +
  labs(color = 'Type of Site') +
  theme(plot.title=element_text(vjust=-1), 
        text=element_text(size=16)) 
# --------------------

# --------------------
# Regions in which EGPAF works - with mother/baby pairs

r3 = ggplot(coord2, aes(x=long, y=lat, group=group, fill=NA)) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_manual(values = c('#4292c6', '#f0f0f0'), 
                    na.value = '#ffffff')+
  geom_point(data = dt, 
             aes(x = Longitude, y = Latitude, group = Work,
                 size = size, colour = Mother), 
             alpha = 0.9, show.legend = TRUE,
             inherit.aes = FALSE)+
  scale_size(guide = 'none')+
  theme_void(base_size = 14) +
  labs(color = '') +
  theme(plot.title=element_text(vjust=-1), 
        text=element_text(size=16)) 
# --------------------


# ----------------------------------------------
# DISTRICT MAPS 
# ----------------------------------------------
# Regions Implemented on a District Level Map
p1 = ggplot(coord, aes(x=long, y=lat, group=group, fill=factor(region))) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_manual(values = c('#e5f5e0', '#74c476'), 
                    na.value = '#f0f0f0',
                    breaks = c('Hhohho', 'Shiselweni'))+
  geom_point(data = dt, 
             aes(x = Longitude, y = Latitude, group = Work), 
             size = 3, colour = '#feb24c', alpha = 0.3, 
             inherit.aes = FALSE)+
  theme_void(base_size = 14) +
  labs(title="ASPIRE Project Implementing Sites",
       fill = 'Region') +
  theme(plot.title=element_text(vjust=-1), 
        text=element_text(size=16)) 
# --------------------


# --------------------
# Regions in which EGPAF works - with sites

# Regions Implemented on a District Level Map
p2 = ggplot(coord, aes(x=long, y=lat, group=group, fill=factor(region))) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_manual(values = c('#c6dbef', '#6baed6'), 
                    na.value = '#f0f0f0',
                    breaks = c('Hhohho', 'Shiselweni'))+
  geom_point(data = dt, 
             aes(x = Longitude, y = Latitude, group = Work,
                 color = Mother), 
             size = 3, alpha= 0.9, 
             inherit.aes = FALSE)+
  scale_color_manual(values = c('#fec44f', '#a50f15'))+
  theme_void(base_size = 14) +
  labs(title="ASPIRE Project Implementing Sites",
       fill = 'Region',
       color = 'Type of Site') +
  theme(plot.title=element_text(vjust=-1), 
        text=element_text(size=16)) 
  
# --------------------

# --------------------
# Regions in which EGPAF works

# Regions Implemented on a District Level Map
p3 = ggplot(coord, aes(x=long, y=lat, group=group, fill=factor(region))) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_manual(values = c('#d6604d', '#2166ac'), 
                       na.value = '#f0f0f0',
                    breaks = c('Hhohho', 'Shiselweni'))+
  theme_void(base_size = 14) +
  labs(title="ASPIRE Project Implementing Regions",
       fill = 'Region') +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6),
        text=element_text(size=16)) 
# -------------------


# --------------------
# Regions in which EGPAF works - with sites by volume

# Regions Implemented on a District Level Map
p4 = ggplot(coord, aes(x=long, y=lat, group=group, fill=factor(region))) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_manual(values = c('#e5f5e0', '#74c476'), 
                    na.value = '#f0f0f0',
                    breaks = c('Hhohho', 'Shiselweni'))+
  geom_point(data = dt, 
             aes(x = Longitude, y = Latitude, group = Work,
                 size = HTS_TST), 
             colour = '#feb24c', alpha= 0.9, 
             inherit.aes = FALSE)+
  theme_void(base_size = 14) +
  labs(title="ASPIRE Project Implementing Sites",
       subtitle = 'Size indicates HIV testing volume, Q2 FY22',
       fill = 'Region') +
  theme(plot.title=element_text(vjust=-1), 
        text=element_text(size=16)) 
# --------------------

# --------------------
ggplot(coord, aes(x=long, y=lat, group=group, fill=factor(region))) + 
  coord_fixed()+                         
  geom_polygon()+ 
  geom_path(size=0.01)+
  scale_fill_manual(values = c('#e5f5e0', '#74c476'), 
                    na.value = '#f0f0f0',
                    breaks = c('Hhohho', 'Shiselweni'))+
  geom_point(data = dt, 
             aes(x = Longitude, y = Latitude, group = Work,
                 size = HTS_TST, color = Mother),  alpha= 0.8, 
             inherit.aes = FALSE)+
  scale_color_manual(values = c('#006d2c', '#fec44f'))+
  theme_void(base_size = 14) +
  labs(title="ASPIRE Project Implementing Sites",
       subtitle = 'Size indicates HIV testing volume, Q2 FY22',
       fill = 'Region',
       color = 'Type of Site') +
  theme(plot.title=element_text(vjust=-1), 
        text=element_text(size=16)) 
# --------------------


pdf(paste0(outDir, 'site_maps.pdf'), width = 12, height = 9)

r1
r2
r3
p1
p2
p3
p4

dev.off()





# 
# 
# 
# 
# 
# 
# # --------------------
# # Second sample map - sites displayed
# 
# # sample map - subset by country, sites
# ggplot(shape, aes(x=long, y=lat, group=group, fill=1)) + 
#   coord_fixed()+                         
#   geom_polygon()+ 
#   geom_path(size=0.01)+
#   scale_fill_gradientn(colors = '#a1d99b')+
#   geom_point(data = dt[egpaf_facility=="Yes"], 
#              aes(x = long, y =lat, group = country), 
#              size = 3, colour = '#feb24c', alpha= 0.5)+
#   theme_void(base_size = 14) +
#   labs(title="Sample Map of eSwatini",
#        subtitle = "Sites displayed on districts") +
#   theme(plot.title=element_text(vjust=-1), 
#         plot.caption=element_text(vjust=6),
#         text=element_text(size=16),
#         legend.position="none")
# 
# # --------------------
# 
# # ----------------------------------------------
# # PDF MAPS
# # ----------------------------------------------
# 
# # --------------------
# # sample map 1 - example of sites on districts
# m1 = ggplot(shape, aes(x=long, y=lat, group=group, fill=1)) + 
#   coord_fixed()+                         
#   geom_polygon()+ 
#   geom_path(size=0.01)+
#   scale_fill_gradientn(colors = '#addd8e')+
#   geom_point(data = dt[egpaf_facility=="Yes"], 
#              aes(x = long, y =lat, group = country), 
#              size = 3, colour = '#feb24c', alpha= 0.9)+
#   theme_void(base_size = 14) +
#   labs(title="Sample Map of eSwatini",
#        subtitle = "Sites displayed on districts") +
#   theme(plot.title=element_text(vjust=-1), 
#         plot.caption=element_text(vjust=6),
#         text=element_text(size=16),
#         legend.position="none")
# 
# # --------------------
# 
# # --------------------
# # sample map 2 - hts data 
# 
# m2 = ggplot(shape, aes(x=long, y=lat, group=group, fill=1)) + 
#   coord_fixed()+                         
#   geom_polygon()+ 
#   geom_path(size=0.01, color = '#737373')+
#   scale_fill_gradientn(colors = '#ccebc5')+
#   scale_color_manual(values = c('#238b45', '#feb24c'))+
#   geom_point(data = dt[egpaf_facility=="Yes"], 
#              aes(x = long, y =lat, group = country, 
#             color = region, size = q4), 
#             alpha = 0.9)+
#   theme_void(base_size = 14) +
#   guides(fill = FALSE)+
#   labs(color = 'Region', size = "Tests Performed")+
#   labs(title="HIV testing by site (HTS_TST), Q4 FY21",
#        subtitle = "Size corresponds to testing volume") +
#   theme(text=element_text(size=16))
# 
# # --------------------
# # sample map 3 - hts data over quarters
# 
# # subset data and reshape long
# dt2 = dt[egpaf_facility=='Yes', c('name', 'district', 'region', 'country',
#        'lat', 'long', 'q1', 'q2', 'q3', 'q4')]
# dt2 = melt(dt2, id.vars = c('name', 'district', 'region', 'country',
#                       'lat', 'long'))
# 
# dt2[variable=='q1', variable:='Q1 FY21']
# dt2[variable=='q2', variable:='Q2 FY21']
# dt2[variable=='q3', variable:='Q3 FY21']
# dt2[variable=='q4', variable:='Q4 FY21']
# 
# # create a long shape file
# coord2 = copy(coord)
# coord3 = copy(coord)
# coord4 = copy(coord)
# coord[ , variable:='Q1 FY21']
# coord2[ , variable:='Q2 FY21']
# coord3[ , variable:='Q3 FY21']
# coord4[ , variable:='Q4 FY21']
# coord = rbind(coord, coord2, coord3, coord4)
# 
# m3 = ggplot(coord, aes(x=long, y=lat, group=group, fill=1)) + 
#   coord_fixed()+                         
#   geom_polygon()+ 
#   facet_wrap(~variable)+
#   geom_path(size=0.01, color = '#737373')+
#   scale_fill_gradientn(colors = '#ccebc5')+
#   scale_color_manual(values = c('#238b45', '#feb24c'))+
#   geom_point(data = dt2, 
#              aes(x = long, y =lat, group = country, 
#                  color = region, size = value), 
#              alpha = 0.9)+
#    theme_void(base_size = 14) +
#   guides(fill = FALSE)+
#   labs(color = 'Region', size = "Tests Performed")+
#   labs(title="HIV testing by site (HTS_TST), Q4 FY21",
#        subtitle = "Size corresponds to testing volume") +
#   theme(text=element_text(size=16))
# # --------------------
# 
# # --------------------
# # create a district level map
# dt3 = dt2[ , .(value = sum(value)), by = .(district, variable)]
# coord_hts = merge(coord, dt3, by = c('district', 'variable'), all = T)
# 
# m4 = ggplot(coord_hts, aes(x=long, y=lat, group=group, fill=value)) + 
#   coord_fixed()+                         
#   geom_polygon()+ 
#   facet_wrap(~variable)+
#   geom_path(size=0.01, color = '#737373')+
#   scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), 
#                        na.value = '#f0f0f0')+
#   theme_void(base_size = 14) +
#   labs(title="HIV testing by district (HTS_TST)",
#        fill = 'HTS_TST') +
#   theme(text=element_text(size=16))
# # --------------------
# 
# # --------------------
# # OUTPUT PDF SAMPLE MAPS 
# 
# pdf(paste0(outDir, 'sample_maps.pdf'), width = 12, height = 9)
# 
# m1
# m2
# m3
# m4
# 
# dev.off()
# # --------------------
# 
# 
# 
# 
# 
