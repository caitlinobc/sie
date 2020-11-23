# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/23/20
# Initial visualizations of Cameroon Attendiere 95 weekly data
# For testing and data quality checks
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

# --------------------
# Files and directories

# set the working directory to the cameroon data
dir = 'C:/Users/ccarelli/Documents/data/'

# import the prepped data shaped long
dt = readRDS(paste0(dir, 'prepped/cameroon_weekly_fy21.rds'))

# --------------------
# prep the data set for visualization 

dt[ ,tier:=factor(tier)]

# ----------------------------------------------
# color palettes for maps and plots

# store colors
ratio_colors = brewer.pal(8, 'Spectral')
blue_colors = brewer.pal(6, 'Blues')
red_colors = brewer.pal(6, 'Reds')

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

# ----------------------------------------------
# VISUALIZE THE DATA 

# --------------------
# counts of facilities by geographic region 

reg_fac = dt[ , .(facilities = length(unique(facility))), by = .(region, tier)] 

dist_fac = dt[ , .(facilities = length(unique(facility))),
               by = .(district, region, tier)] 

# Number of facilities by region, tier
ggplot(reg_fac, aes(x=region, y=facilities, fill=tier)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name='Tier', values=ratio_colors) + theme_minimal() +
  labs(title = "Total number of health facilities by region, tier", 
       x='Region', y="Number of sites", 
       caption="Source: Attendiere 95 weekly reporting")

# Number of facilities by district, tier: Sud
ggplot(dist_fac[region=='Sud'], aes(x=district, y=facilities, fill=tier)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name='Tier', values=red_colors) + theme_minimal() +
  labs(title = "Number of health facilities by district: Sud", 
       x='District', y="Number of sites", 
       caption="Source: Attendiere 95 weekly reporting")

# Number of facilities by district, tier: Littoral
ggplot(dist_fac[region=='Sud'], aes(x=district, y=facilities, fill=tier)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(name='Tier', values=red_colors) + theme_minimal() +
  labs(title = "Number of health facilities by district: Sud", 
       x='District', y="Number of sites", 
       caption="Source: Attendiere 95 weekly reporting")









