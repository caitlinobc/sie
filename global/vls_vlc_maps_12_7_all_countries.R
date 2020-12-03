# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/2/20
# Visualize vira load coverage and suppression
# All SIE supported countries for 12/7 data review
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
# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/Documents/data/'

# set the working directory for the shape file
setwd('C:/Users/ccarelli/Documents/data/prepped/')

# set output director
OutDir = paste0(dir, 'outputs/')

# --------------------
# load the ihme function to remove diacritical marks

fix_diacritics = function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
       'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
       'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
        'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  replace_me = paste(names(replacement_chars), collapse='')
  replace_with = paste(replacement_chars, collapse = '')    
  return(chartr(replace_me, replace_with, x)) }

# --------------------
# read in the shape files, district names, and data

shape = readRDS('shape_files_all_countries.rds')
names = readRDS('district_names_ids.rds')
dt = data.table(read.csv(paste0(dir, 'raw_data/vls_vlc_peds_q4_20_all_countries.csv')))

# ----------------------------------------
# FORMAT THE DATA

# --------------------
# rename the columns
setnames(dt, names(dt), c('country', 'region',
        'district', 'level_6', 'vlc', 'vls', 'vls_denom'))

# drop the first six rows that delineate the filters
dt = dt[-c(1:5)]

# drop the final row which represents a summary total
dt = dt[1:(nrow(dt) - 1)]

# drop empty rows
dt = dt[!(country=="" & region=="" & district=="")]

# convert "" values to missing
dt[region=="", region:=NA]
dt[district=="", district:=NA]
dt[level_6=="", level_6:=NA]

# if district is missing, the row represents a country total
dt[is.na(region) & is.na(district) & is.na(level_6), level:='country']
dt[!is.na(region) & is.na(district)& is.na(level_6), level:='region']
dt[is.na(region) & !is.na(district)& is.na(level_6), level:='district']
dt[!is.na(level_6), level:='level_6']

# --------------------
# drop the percentage signs and commas in counts
dt[ , vlc:=as.numeric(gsub('%', '', vlc))]
dt[ , vls:=as.numeric(gsub('%', '', vls))]
dt[, vls_denom:=as.numeric(gsub(',', '', vls_denom))] #some counts have commas

# drop district and subcount
dt[country=='Uganda', district:=trimws(gsub('District', '', district), 'both')]
dt[country=='Kenya', district:=trimws(gsub('Sub County', '', district), 'both')]
dt[country=='Malawi', level_6:=trimws(gsub('District', '', level_6), 'both')]
#---------------------

# -------------------------------------
# ADMIN LEVEL MATCH

#---------------------
# format the levels

c = 'Malawi'

dt_names = sort(dt[country==c & level=='district']$district)
dist_names = fix_diacritics(sort(names[country==c]$district))
dt_names
length(dt_names)
dt_names[dt_names %in% dist_names]
dt_names[!(dt_names %in% dist_names)]

mal = dt[country=='Malawi' & !is.na(level_6), unique(level_6)]
mal[mal %in% dist_names]


# create a data set to merge to the shape files
dt_merge = dt[(level=='district' & is.na(level_6)) | level=='level_6']
dt_merge[country=='Malawi', district:=level_6]
dt_merge = dt_merge[!is.na(district)]
dt_merge[ ,c('region', 'level_6', 'level'):=NULL]

# merge in the ids
dt_merge = merge(dt_merge, names, by=c('country', 'district'), all.x = T)
dt_merge[ , country:=NULL]

# merge in the shape file
shape = merge(shape, dt_merge, by = 'id', all.x = T)


ggplot(shape[country=='Malawi'], aes(x=long, y=lat, group=group, fill=vls)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(8, 'BuPu'), 
                       na.value='#d9d9d9') + 
  theme_void(base_size =16) +
  labs(title="Viral suppression ratios by region, Eswatini", 
       fill="Percent suppressed (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=18))







