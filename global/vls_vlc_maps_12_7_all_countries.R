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

# format the names to remove diacritical marks
names[ , district:=fix_diacritics(district)]
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
# DRC DATA SWITCH

# drop the provincial data and swap in the health zone level data
dt = dt[country!='DRC']

# rbind in the health zone data
drc = data.table(read.csv(paste0(dir, 'raw_data/vls_vlc_pedsq4_20_drc_hz.csv')))

# format to rbind
drc = drc[-c(1:5)]
setnames(drc, names(drc), c('district', 'vlc', 'vls', 'vls_denom'))
drc = drc[-nrow(drc)] # drop the totals row

# create identical columns with the main data set
drc[ , country:='DRC']
drc[district=='DRC', level:='country']
drc[district!='DRC', level:='district']
drc[district=='DRC', district:=NA]
drc[ , region:=NA] # blank columns for the rbind
drc[ , level_6:=NA]

# drop the percentage signs and commas in counts
drc[ , vlc:=as.numeric(gsub('%', '', vlc))]
drc[ , vls:=as.numeric(gsub('%', '', vls))]
drc[, vls_denom:=as.numeric(gsub(',', '', vls_denom))]

# bind in the data 
dt = rbind(dt, drc)

# -------------------------------------

# -------------------------------------
# ADMIN LEVEL MATCH

#---------------------
# format the levels

# cameroon
names[country=='Cameroon' & district=='Ebolowa 1', district:='Ebolowa']
names[country=='Cameroon' & district=='Ebolowa 2', district:='Ebolowa']
names[country=='Cameroon' & district=='Edéa 1', district:='Edea']
names[country=='Cameroon' & district=='Edéa 2', district:='Edea']
names[country=='Cameroon' & district=='Kribi 2', district:='Kribi']
names[country=='Cameroon' & district=='Kribi 2', district:='Kribi']
names[country=='Cameroon' & district=='Nkongsamba 1', district:='Nkongsamba']
names[country=='Cameroon' & district=='Nkongsamba 2', district:='Nkongsamba']

# cote d'ivoire - no districts can be mapped

# drc
names[district=='Masina II', district:='Masina 2']

# eswatini
dt[country=='Eswatini' & district=='Madlangampisi', district:='Madlangempisi']
dt[country=='Eswatini' & district=='Maphalaleni', district:="Mphalaleni"]
dt[country=='Eswatini' & district=='Motshane', district:='Motjane']
dt[country=='Eswatini' & district=='Piggs Peak', district:='Pigg\'s Peak']
dt[country=='Eswatini' & district=='Shiselweni 1', district:='Shiselweni']
dt[country=='Eswatini' & district=='Timphisini', district:='Timpisini']
names[country=='Eswatini' & district=="Matsanjeni North", district:='Matsanjeni']
names[country=='Eswatini' & district=="Matsanjeni South", district:='Matsanjeni']

# kenya - 100% match

# lesotho
dt[country=='Lesotho' & region=='Thaba Tseka', region:='Thaba-Tseka']

# malawi - districts are one level down (level_6), but 100% match

# mozambique
names[country=='Mozambique' & district=='Chokwc', district:='Chokwe']
dt[country=='Mozambique' & district=='Mandlakaze', district:='Mandlakazi']
dt[country=='Mozambique' & district=='Vilankulo', district:='Vilanculos']

# uganda
dt[country=='Uganda'& district=='Kanungu', district:='Kinkiizi']
dt[country=='Uganda'& district=='Rubirizi', district:='Bunyaruguru']
# Bufumbira is a county in Rukungiri, along with Rubabo
dt[country=='Uganda'& district=='Rukungiri', district:='Bufumbira']

# -------------------------------------
# test code for district match

# c = 'DRC'
# dists = dt[country==c & !is.na(district), unique(district)]
# dists
# length(dists)
# names[country == c & district %in% dists]
# dists[!dists %in% names[country==c, district]]
# 
# # Tanzania uses regions, not districts (for now)
# regs = dt[country=='Lesotho' & !is.na(region), unique(region)]
# regs
# length(regs)
# names[country == 'Lesotho' & district %in% regs]
# regs[!regs %in% names[country=='Lesotho', district]]

# -------------------------------------
# DATA MERGE

#----------------------
# mark the data that will be merged with the shape file
odd_countries = c('Tanzania', 'Lesotho' , 'Malawi')
dt[level=='district' & !(country %in% odd_countries), merge_row:=TRUE]

dt[country=='Malawi'& level=='level_6', merge_row:=TRUE]
# use regions for both tanzania and lesotho
dt[country=='Tanzania' & level=='region', merge_row:=TRUE]
dt[country=='Lesotho' & level=='region', merge_row:=TRUE]
dt[is.na(merge_row), merge_row:=FALSE]

# create a data set to merge to the shape files
dt_merge = dt[merge_row==TRUE]
dt_merge[country=='Malawi', district:=level_6]
dt_merge[country=='Tanzania', district:=region]  
dt_merge[country=='Lesotho', district:=region]  
dt_merge[ , c('region', 'level_6', 'level', 'merge_row'):=NULL]

# merge the data in the ids
# some districts will duplicate because they are assigned to multiple polygons
dt_merge = merge(dt_merge, names, by = c('country', 'district'), all.x=T)
dt_merge[ , country:=NULL] # drop country so as not to duplicate shape

#----------------------

# -------------------------------------
# FINAL PLOTS

# merge in the shape file
shape = merge(shape, dt_merge, by = 'id', all.x = T)

# list of countries to loop through
countries = c('Cameroon','Cote d\'Ivoire', 'DRC', 'Eswatini',
             'Kenya', 'Lesotho', 'Mozambique', 'Malawi',
             'Tanzania', 'Uganda')

# shape the data long
IDvars = c('country', 'district', 'id', 'long',
           'lat', 'order', 'hole', 'piece', 'group')
shape_long = melt(shape, id.vars = IDvars)

# factor the variables for labelling
shape_long$variable = factor(shape_long$variable, 
            c('vlc', 'vls', 'vls_denom'), 
            c('Viral Load Coverage', 'Viral Suppression', 'VL Test Results'))

#----------------------
# plot viral load testing coverage

vlc_plots = NULL
i=1

for (c in countries) {
  # set the country name for a subtitle
  country_name = as.character(c)
  if (country_name=='DRC') country_name = 'Kinshasa'
  
# plot the indicator in a list
vlc_plots[[i]] = ggplot(shape[country==c], 
      aes(x=long, y=lat, group=group, fill=vlc)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'BuPu'),
    na.value='#d9d9d9') + 
  theme_void(base_size = 14) +
  labs(title="Viral load coverage",
       subtitle = country_name,
       fill="Percent (%)") +
  theme(plot.title=element_text(vjust=-1), 
        legend.title = element_text(vjust=2),
        plot.caption=element_text(vjust=6), 
        text=element_text(size=16)) 

i =i+1} # reset the index

#----------------------

#----------------------
# plot viral suppression 

vls_plots = NULL
i=1

for (c in countries) {
  # set the country name for a subtitle
  country_name = as.character(c)
  if (country_name=='DRC') country_name = 'Kinshasa'
  
  # plot the indicator in a list
  vls_plots[[i]] = ggplot(shape[country==c], 
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
  
  i =i+1} # reset the index

#----------------------

#----------------------
# plot viral load tests performed

vls_denom_plots = NULL
i=1

for (c in countries) {
  # set the country name for a subtitle
  country_name = as.character(c)
  if (country_name=='DRC') country_name = 'Kinshasa'
  
  # plot the indicator in a list
  vls_denom_plots[[i]] = ggplot(shape[country==c], 
                          aes(x=long, y=lat, group=group, fill=vls_denom)) + 
    coord_fixed() +
    geom_polygon() + 
    geom_path(size=0.01) + 
    scale_fill_gradientn(colors = brewer.pal(9, 'Greens'),
                         na.value='#d9d9d9') + 
    theme_void(base_size = 14) +
    labs(title="Count of viral load tests performed",
         subtitle = country_name,
         fill="") +
    theme(plot.title=element_text(vjust=-1), 
          plot.caption=element_text(vjust=6), 
          text=element_text(size=16)) 
  
  i =i+1} # reset the index

#----------------------


#-------------------------------------------
# EXPORT THE PLOTS

#----------------------
# one page per country, indicator 

# pdf of viral load testing coverage by country
pdf(paste0(OutDir, 'vlc_f20_q4_all_countries.pdf'), width = 12, height = 9)
vlc_plots
dev.off()

# pdf of viral suppression by country
pdf(paste0(OutDir, 'vls_f20_q4_all_countries.pdf'), width = 12, height = 9)
vls_plots
dev.off()

# pdf of total viral load results available (count)
pdf(paste0(OutDir, 'vl_results_counts_f20_q4_all_countries.pdf'), width = 12, height = 9)
vls_denom_plots
dev.off()
#----------------------

#-------------------------------------------
# FACET PLOTS

#----------------------
# plot viral load tests performed

viral_plots = NULL
i=1

for (c in countries) {
  # set the country name for a subtitle
  country_name = as.character(c)
  if (country_name=='DRC') country_name = 'Kinshasa'
  
  # plot the indicator in a list
  viral_plots[[i]] = ggplot(shape_long[country==c & variable!='VL Test Results'], 
      aes(x=long, y=lat, group=group, fill=value)) + 
    coord_fixed() +
    geom_polygon() + 
    geom_path(size=0.01) +
    facet_wrap(~variable) +
    scale_fill_gradientn(colors = brewer.pal(9, 'Blues'),
                         na.value='#d9d9d9') + 
    theme_void(base_size = 14) +
    labs(title = country_name, 
         fill="") +
    theme(plot.title = element_text(vjust=2), 
          strip.text = element_text(vjust=1, size = 16),
          text=element_text(size=16)) 
  
  i =i+1} # reset the index

#----------------------

# pdf of total viral load results available (count)
pdf(paste0(OutDir, 'vl_results_counts_f20_q4_all_countries.pdf'), width = 12, height = 9)
viral_plots
dev.off()
#----------------------

