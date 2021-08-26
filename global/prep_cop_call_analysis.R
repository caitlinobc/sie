# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/17/21
# PrEP COP Meeting Analysis
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/prep_cop/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Global/PrEP COP Meeting/'

# mapping director
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps/r_shape_files/africa_shape_file/'

# read in the data 
dt = data.table(read.csv(paste0(dir, 'prep_data_fy20_21.csv')))

# load the prep watch data 
pw = data.table(read.xlsx(paste0(dir, 'prep_watch.xlsx')))

# ----------------------------------------------

# --------------------
# prep the data for analysis

# rename the variables for analysis 
setnames(dt, c('country', 'sex', 'age', 'prep_curr', 'prep_new', 'fq'))

# shape the data long for visualization
dt = melt(dt, id.vars = c('country', 'sex', 'age', 'fq'))

# --------------------

# ----------------------------------------------
# create summary tables to plot 

df = dt[ , .(value = sum(value, na.rm = T)), by = .(country, fq, variable)]

# --------------------
# country comparisons - prep_curr and prep_new
# 
# ggplot(df[variable=='prep_curr' & fq=='FY21 Q3' & country!='Lesotho'], 
#        aes(x=country, y=value)) +
#   geom_bar(position = 'dodge', stat = 'identity') +
#   geom_text(position = position_dodge(width=0.9), 
#             aes(label=value), vjust=-0.5, size = 7)+
#   scale_fill_manual(values = c('#73afb6', '#a6bddb')) +
#   theme_bw()+
#   labs(x = 'Age Category', y = 'Enrolled on PrEP',
#        fill ='') +
#   theme(text = element_text(size=28),
#         legend.position = "top", 
#         legend.title = element_blank()) 

# --------------------

# ----------------------------------------------
# MAPS

# --------------------
# import the shape file and convert to a data table
map = shapefile(paste0(mapDir, '1.shp'))
coord = data.table(fortify(map))

# merge in the names of the countries to match the data 
names = data.table(cbind(country = c(map@data$COUNTRY), id =  c(0, seq(1:47))))
coord = merge(coord, names, by = 'id')

# sum the data to the appropriate level and merge 
map_df = df[fq=='FY21 Q3']
map_df = dcast(map_df, country~variable)
coord = merge(coord, map_df, by = 'country', all = T)

# remove prep_curr for lesotho
coord[country=='Lesotho', prep_curr:=NA]

# --------------------
# merge in the prep watch data 
coord = merge(coord, pw, by = 'country', all = T)
coord[ , label:= NULL]

# --------------------
# merge in the rates of change

pn = dt[variable=='prep_new' & fq %in% c('FY21 Q1', 'FY21 Q2'),
   .(value = sum(value, na.rm=T)),
   by = .(fq, country)]
pn = dcast(pn, country~fq)
setnames(pn, c('country', 'q1', 'q2'))
pn[ , roc:=round((q2/q1)*100)]
pn[ , c('q1', 'q2'):=NULL]

# manually set malawi's 
# uses q3/q2 as no reporting in q1
pn[country=='Malawi', roc:=round(100*(5.044248))]

# merge to the data
coord = merge(coord, pn, by = 'country', all = T)

# --------------------
# create labels

# create the labels
labels = data.table(coordinates(map))
setnames(labels, c('long', 'lat'))
labels = cbind(labels, country = names$country)

# add specific variables by indicator
labels = merge(labels, map_df, by = 'country', all = T)
labels[!is.na(prep_curr) & country!='Lesotho', prep_curr_label:=paste0(country, ': ', prep_curr)]
labels[!is.na(prep_new) & country!='DRC', prep_new_label:=paste0(country, ': ', prep_new)] # no value for DRC

# merge in prep watch labels
labels = merge(labels, pw, by = 'country', all = T)
labels[!is.na(prep), label:=paste0(country, ': ', label)]
setnames(labels, 'label', 'pw_label')

# merge in rates of change
labels = merge(labels, pn, by = 'country', all = T)
labels[!is.na(roc), pn_roc_label:=paste0(country, ': ', roc, '%')]

# drop excess variables
labels[ ,c('prep_curr', 'prep_new', 'prep', 'roc'):=NULL]

# --------------------
# fix the map by creating a South Africa specific map
s_africa_layer = geom_polygon(aes(x = long, y = lat, group = group),
                              data = coord[country=='South Africa' & hole==F], fill = NA, 
                              color = 'black')

# ----------------------------------------------
# MAP THE INDICATORS

# map of Prep Curr
ggplot(coord, aes(x=long, y=lat, group=group, fill=prep_curr)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="PREP_CURR")+
  theme( text=element_text(size=18))+
  geom_label_repel(data = labels, aes(label = prep_curr_label, 
           x=long, y=lat, group=country), inherit.aes=FALSE, size=5)
  

# map of Prep New
  ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=prep_new)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Purples'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="PREP_NEW")+
  theme( text=element_text(size=18))+
  s_africa_layer+
  geom_label_repel(data = labels, aes(label = prep_new_label, 
           x=long, y=lat, group=country), inherit.aes=FALSE, size=5)

# map of prep_watch data 
  ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=prep)) + 
    coord_fixed() +
    geom_polygon() + 
    geom_path(size=0.01) + 
    scale_fill_gradientn(colors = brewer.pal(9, 'RdYlBu'), na.value='#ffffff') + 
    theme_void(base_size =16) +
    labs(fill="PrEPWatch Estimate")+
    theme( text=element_text(size=18))+
    s_africa_layer+
    geom_label_repel(data = labels, aes(label = pw_label, 
        x=long, y=lat, group=country), inherit.aes=FALSE, size=4)


  # map of rates of change in new enrollments
  ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=roc)) + 
    coord_fixed() +
    geom_polygon() + 
    geom_path(size=0.01) + 
    scale_fill_gradientn(colors = brewer.pal(9, 'Reds'), na.value='#ffffff') + 
    theme_void(base_size =16) +
    labs(fill="Rate of change (%)")+
    theme( text=element_text(size=18))+
    s_africa_layer+
    geom_label_repel(data = labels, aes(label = pn_roc_label, 
            x=long, y=lat, group=country), inherit.aes=FALSE, size=5)


  # ----------------------------------------------















