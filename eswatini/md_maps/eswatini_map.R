# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/25/20
# Initial visualizations of Cameroon Attendiere 95 weekly data
# For testing and data quality checks
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
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/Documents/data/'

# set the working directory for the shape file
setwd('C:/Users/ccarelli/Documents/data/shape_files')

# set output director
OutDir = paste0(dir, 'outputs/')

# --------------------
# import the data 

dt = data.table(read.csv(paste0(dir, 
          'eswatini_map_test/Eswatini Peds Viral Suppression and Coverage Rates.csv')))

# rename the columns
setnames(dt, c('country', 'region', 'district', 'vls', 'vlc'))

# --------------------
# import the shape file, check, and fortify

shape1 = readRDS('gadm36_SWZ_1_sp.rds') # regional level
shape2 = readRDS('gadm36_SWZ_2_sp.rds') # district level
plot(shape1)
plot(shape2)

# list and keep the names
reg_names = data.table(cbind(region = shape1@data$NAME_1,
                             id = shape1@data$GID_1))
dist_names = data.table(cbind(district = shape2@data$NAME_2,
                              id = shape2@data$GID_2))

# --------------------
# create labels for maps 

# regional labels
labels1 = data.table(coordinates(shape1))
setnames(labels1, c('long', 'lat'))
labels1 = cbind(labels1, region = reg_names$region)

# district labels
labels2 = data.table(coordinates(shape2))
setnames(labels2, c('long', 'lat'))
labels2 = cbind(labels2, district = dist_names$district)

# --------------------
# fortify the shape file
reg_coord = data.table(fortify(shape1, region = 'GID_1'))
dist_coord = data.table(fortify(shape2, region = 'GID_2'))

# merge in the names of the districts
reg_coord = merge(reg_coord, reg_names, by = 'id', all.x = TRUE)
dist_coord = merge(dist_coord, dist_names, by = 'id', all.x = TRUE)

# --------------------

# ----------------------------------------------
# COLOR PALETTES

ratio_colors = brewer.pal(8, 'Spectral')
cyans = c('cyan', 'cyan1', 'cyan2', 'cyan3', 'cyan4')

# --------------------
# minor data cleaning

# fill in the country and regions 
dt[ ,country:='Eswatini']
dt[1:16, region:='Hhohho']
dt[17:27, region:='Shiselweni']

# create nation and regional data sets
dt[district=="", district:=NA]
natl_dt = dt[1,]
natl_dt[ , c('region', 'district'):=NULL]
dt = dt[-1,]
reg_dt = dt[is.na(district)]
reg_dt[ , district:=NULL]
dt = dt[!is.na(district)]

# remove percent signs from district level data
reg_dt[ , vls:=as.numeric(gsub('%', '', vls))]
reg_dt[ , vlc:=as.numeric(gsub('%', '', vlc))]
dt[ , vls:=as.numeric(gsub('%', '', vls))]
dt[ , vlc:=as.numeric(gsub('%', '', vlc))]

# --------------------
# merge the data to the shape file

reg_coord = merge(reg_coord, reg_dt, by = 'region', all=T)
dist_coord = merge(dist_coord, dt, by = 'district', all=T)

# --------------------
# shape data long for facet wrapped maps

# regional level long data 
reg_IDvars = c("region", "id", "long", "lat", "order",
           "hole", "piece", "group", "country")
reg_long = melt(reg_coord,id.vars = reg_IDvars)

# factor the variable with appropriate labels
reg_long$variable = factor(reg_long$variable, c('vlc', 'vls'),
                            c('Viral Load Coverage', 'Viral Suppression'))

# district level long data 
IDvars = c("district", "id", "long", "lat", "order",
           "hole", "piece", "group", "country", "region")
dist_long = melt(dist_coord,id.vars = IDvars)

# factor the variable with appropriate labels
dist_long$variable = factor(dist_long$variable, c('vlc', 'vls'),
                  c('Viral Load Coverage', 'Viral Suppression'))

# --------------------

# --------------------
# add data to labels for maps

# regional labels
labels1 = merge(labels1, reg_dt, by = 'region', all = T)
labels1[region=='Hhohho' | region=='Shiselweni', vlc_label:=paste0(region, ': ',
                                                as.character(vlc), '%')]
labels1[region=='Hhohho' | region=='Shiselweni', vls_label:=paste0(region, ': ',
                                                as.character(vls), '%')]

labels1[region=='Lubombo' | region=='Manzini', vlc_label:=region]
labels1[region=='Lubombo' | region=='Manzini', vls_label:=region]

labels1[ ,c('vlc', 'vls', 'country'):=NULL]

# district labels
labels2 = merge(labels2, dt, by = 'district', all = T)
labels2[!is.na(vlc) , vlc_label:=paste0(district, ': ', as.character(vlc), '%')]
labels2[!is.na(vls) , vls_label:=paste0(district, ': ', as.character(vls), '%')]
labels2[ ,c('vlc', 'vls', 'country', 'region'):=NULL]

# --------------------

# ----------------------------------------------
# REGIONAL LEVEL MAPS

#---------------
# annual coverage ratios

g1 = ggplot(reg_coord, aes(x=long, y=lat, group=group, fill=vlc)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(3, 'BuPu'), na.value='#d9d9d9',
                       limits = c(min(reg_coord$vlc), 100),
                       breaks = c(90, 95, 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral load coverage by region, Eswatini", 
       caption="Source: EGPAF monitoring data \n *Legend capped at 100% coverage",
       fill="Received a VL test (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=18))  +
  geom_label_repel(data = labels1, aes(label = vlc_label, 
                  x=long, y=lat, group=region), inherit.aes=FALSE, size=5)

#---------------
# annual suppression ratios

g2 = ggplot(reg_coord, aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(3, 'Purples'),
                       na.value='#d9d9d9',
                       limits = c(min(dist_coord$vls), 100), 
                       breaks = c(90, 95, 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral suppression ratios by region, Eswatini", 
       caption="Source: EGPAF monitoring data",
       fill="Percent suppressed (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=18)) +
  geom_label_repel(data = labels1, aes(label = vls_label, 
              x=long, y=lat, group=region), inherit.aes=FALSE, size=5)

#---------------
# alternate colors for combined version

g1a = ggplot(reg_coord, aes(x=long, y=lat, group=group, fill=vlc)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Oranges'), na.value='#d9d9d9',
                       limits = c(min(reg_coord$vlc), 100),
                       breaks = c(90, 95, 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral load coverage by region, Eswatini", 
       caption="Source: EGPAF monitoring data \n *Legend capped at 100% coverage",
       fill="Received a VL test (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=16))  +
  geom_label_repel(data = labels1, aes(label = vlc_label, 
                      x=long, y=lat, group=region), inherit.aes=FALSE, size=5)

g2a = ggplot(reg_coord, aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'YlOrBr'),
                       na.value='#d9d9d9',
                       limits = c(min(dist_coord$vls), 100), 
                       breaks = c(90, 95, 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral suppression ratios by region, Eswatini", 
       caption="Source: EGPAF monitoring data",
       fill="Percent suppressed (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=16)) +
  geom_label_repel(data = labels1, aes(label = vls_label, 
                           x=long, y=lat, group=region), inherit.aes=FALSE, size=5)

#---------------
# vlc and vls on the same scale

g3 = ggplot(reg_long, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(8, 'Greens'),
                       na.value='#d9d9d9',
                       limits = c(min(dist_long$value), 100), 
                       breaks = c(85, 90, 95, 100)) + 
  theme_void(base_size =16) +
  facet_wrap(~variable) +
  labs(fill="Percent (%)", 
       caption="Source: EGPAF monitoring data \n *Legend bounded at 100%") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=18))

#---------------

# ----------------------------------------------
# DISTRICT LEVEL MAPS

#---------------
# annual coverage ratios

g4 = ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=vlc)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), na.value='#d9d9d9',
                       limits = c(min(dist_coord$vlc), 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral load coverage by district, Eswatini", 
       caption="Source: EGPAF monitoring data \n *Legend capped at 100% coverage",
       fill="Percent receiving a VL test (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=18)) 

#---------------
# annual suppression ratios

g5 = ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(6, 'Greens'), na.value='#d9d9d9',
                       limits = c(min(dist_coord$vls), 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral suppression ratios by district, Eswatini", 
       caption="Source: EGPAF monitoring data",
       fill="Percent suppressed (%)") +
         theme(plot.title=element_text(vjust=-1), 
               plot.caption=element_text(vjust=6), 
               text=element_text(size=18)) 

#----------------------------------
# labelled district level maps 

g4a = ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=vlc)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), na.value='#d9d9d9',
                       limits = c(min(dist_coord$vlc), 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral load coverage by district, Eswatini", 
       caption="Source: EGPAF monitoring data \n *Legend capped at 100% coverage",
       fill="Percent receiving a VL test (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=18))+
  geom_label_repel(data = labels2, aes(label = vls_label, 
             x=long, y=lat, group=district), inherit.aes=FALSE, size=3)

#---------------
# annual suppression ratios

g5a = ggplot(dist_coord, aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(6, 'Greens'), na.value='#d9d9d9',
                       limits = c(min(dist_coord$vls), 100)) + 
  theme_void(base_size =16) +
  labs(title="Viral suppression ratios by district, Eswatini", 
       caption="Source: EGPAF monitoring data",
       fill="Percent suppressed (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=18))+
  geom_label_repel(data = labels2, aes(label = vls_label, 
          x=long, y=lat, group=district), inherit.aes=FALSE, size=3)

#---------------
# vlc and vls on the same scale

g6 = ggplot(dist_long, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(8, 'Greens'),
                       na.value='#d9d9d9',
                       limits = c(min(dist_long$value), 100)) + 
  theme_void(base_size =16) +
  facet_wrap(~variable)+
  labs(fill="Percent (%)", 
       caption="Source: EGPAF monitoring data \n *Legend bounded at 100%") +
  theme(plot.title=element_text(vjust=1), 
        plot.caption=element_text(vjust=6), 
        strip.text = element_text(size = 16),
        text=element_text(size=18)) 

#---------------
# ----------------------------------------------

# ----------------------------------------------
# print a pdf of all maps 

pdf(paste0(OutDir, 'eswatini_vlc_vls_annual_maps.pdf'), width=12, height=9)

g1 
g2
g3 
grid.arrange(g1a,g2a, ncol=2)
g4
g5
g4a
g5a
g6

dev.off()
# ------------------

# ------------------
# subset of district level maps

# include only maps with labels included
pdf(paste0(OutDir, 'eswatini_vlc_vls_district_maps.pdf'), width=12, height=9)

g4a
g5a
g6

dev.off()
# ------------------





