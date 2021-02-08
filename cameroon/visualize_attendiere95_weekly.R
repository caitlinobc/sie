# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/8/21
# Initial visualizations of Cameroon Attendiere 95 weekly data
# For testing and data quality checks
# Grapsh only
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(rgdal)
# --------------------

# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Cameroon/data/'

# set the output directory
OutDir = paste0(dir, 'att95_outputs/')

# --------------------
# import the data 

dt = readRDS(paste0(dir, 'prepped/cameroon_weekly_fy21_no_sex.rds'))

# drop unecessary variables (for now)
dt [ , c('fiscal_yr', 'file_name'):=NULL]

# ----------------------------------------------
# color palettes for maps and plots

# store colors
ratio_colors = brewer.pal(8, 'Spectral')
blues = brewer.pal(6, 'Blues')
reds = brewer.pal(6, 'Reds')

ladies = brewer.pal(11, 'RdYlBu')
gents = brewer.pal(9, 'Purples')

# red colors for bar graph
bar_colors = c('Not Suppressed'='#de2d26', 'Suppressed'='#fc9272')

graph_colors = c('#bd0026', '#fecc5c', '#74c476','#3182bd', '#8856a7')
tri_sex = c('#bd0026', '#74c476', '#3182bd')
wrap_colors = c('#3182bd', '#fecc5c', '#bd0026', '#74c476', '#8856a7', '#f768a1')
sex_colors = c('#bd0026', '#3182bd', '#74c476', '#8856a7') # colors by sex plus one for facilities
single_red = '#bd0026'
# ----------------------------------------------

# --------------------
# import shape file 
# 
# reg_coord =  readRDS('gadm36_CMR_2_sp.rds')
# shape_names = cbind(reg_coord@data$NAME_1, reg_coord@data$GID_1)
# reg_coord= fortify(reg_coord)
# 
# dist_coord = shapefile('gadm36_CMR_2.shp')
# dist_coord = fortify(dist_coord)
# 
# dist_coord$id

# ----------------------------------------------
# VISUALIZE THE DATA
# ----------------------------------------------

# --------------------
# FACILITY COUNTS
# --------------------
# counts of facilities by geographic region 


reg_fac = dt[ , .(facilities = length(unique(facility))), by = .(region, tier)] 

dist_fac = dt[ , .(facilities = length(unique(facility))),
               by = .(district, region, tier)] 

# Number of facilities by region, tier
p1 = ggplot(reg_fac, aes(x=region, y=facilities, fill=tier)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name='Tier', values=ratio_colors) + theme_minimal() +
  labs(title = "Total number of health facilities by region, tier", 
       x='Region', y="Number of sites", 
       caption="Source: Attendiere 95 weekly reporting")

# Number of facilities by district, tier: Sud
p2 = ggplot(dist_fac[region=='Sud'], aes(x=district, y=facilities, fill=tier)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name='Tier', values=reds) + theme_minimal() +
  labs(title = "Number of health facilities by district: Sud", 
       x='District', y="Number of sites", 
       caption="Source: Attendiere 95 weekly reporting")

# Number of facilities by district, tier: Littoral
p3 = ggplot(dist_fac[region=='Littoral'], aes(x=district, y=facilities, fill=tier)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name='Tier', values=blues) + theme_minimal() +
  labs(title = "Number of health facilities by district: Littoral", 
       x='District', y="Number of sites", 
       caption="Source: Attendiere 95 weekly reporting")


# --------------------
# LOOPS BY INDICATOR


# loop through all indicators by region
reg_loop = dt[ , .(value = sum(value)),
         by = .(region, variable, date)]


list_of_plots = NULL
i=1

for(v in unique(reg_loop$variable)) {
  
  list_of_plots[[i]] = ggplot(reg_loop[variable==v], 
                              aes(x=date, y=value, color=factor(region), group=region)) + 
    geom_point(size=0.5) + 
    geom_line(alpha=0.5) + 
    scale_color_manual(values = c('#b2182b', '#2166ac'))+
    facet_wrap(~region, scales='free_y') +
    labs(title=v, x="Date (weekly)", y="Count", color="Region") + theme_bw()
  
  i=i+1
  
}

pdf(paste0(OutDir, 'regional_indicators.pdf'), width = 12, height = 9)
p1
p2
p3
list_of_plots
dev.off()



# --------------------
list_of_plots2 = NULL
i=1




for(v in unique(reg_loop$variable)) {
  
  list_of_plots2[[i]] = ggplot(reg_loop[variable==v], 
                              aes(x=date, y=value, color=factor(region), group=region)) + 
    geom_point(size=0.5) + 
    geom_line(alpha=0.5) + 
    scale_color_manual(values = c('#b2182b', '#2166ac'))+
    labs(title=v, x="Date (weekly)", y="Count", color="Region") + theme_bw()
  
  i=i+1
  
}




# loop through all indicators by region
dist_loop = dt[ , .(value = sum(value)),
               by = .(district, region, variable, date)]


list_of_plots3 = NULL
i=1

for(v in unique(dist_loop$variable)) {
  
  list_of_plots3[[i]] = ggplot(dist_loop[variable==v], 
                              aes(x=date, y=value, color=factor(district), group=district)) + 
    geom_point(size=0.5) + 
    geom_line(alpha=0.5) + 
    facet_wrap(~region, scales='free_y') +
    labs(title=v, x="Date (weekly)", y="Count", color="Region") + theme_bw()
  
  i=i+1
  
}

pdf(paste0(OutDir, 'district_indicators.pdf'), width = 12, height = 9)

list_of_plots3
dev.off()



