# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/10/20
# Cameroon map for mini-POART 
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/'

# set the output directory
OutDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Cameroon/maps/'

# set the working directory for the shape file
setwd(paste0(dir, '/district_rds/'))

# --------------------
# load the function that fixes diacritical marks
source('C:/Users/ccarelli/Documents/GitHub/sie/all/fix_diacritics_function.r')

# --------------------
# import the data and shape long

dt = data.table(read.csv(paste0(dir, 'cameroon_poart.csv')))
dt[ ,yield:=round(yield*100, 1)]

# --------------------
# import the shape file, check, and fortify

# import and plot the shape file
shape = readRDS('gadm36_CMR_3_sp.rds') # district level
plot(shape)

# list the specific districts
shp = data.table(fortify(shape, region = 'GID_3'))

# --------------------
# determine the shape names for the merge

# create a list of names
names = data.table((cbind(district = shape@data$NAME_3, id = shape@data$GID_3)))
shp = merge(shp, names, by = 'id', all.x = TRUE)

# fix diacritical marks in the shape file 
shp[ , district:=fix_diacritics(district)]

# --------------------
# fix the names that don't match

# districts sub-divided into multiple communes (map the same data to each commune)
shp[district=='Ebolowa 1' | district=='Ebolowa 2', district:='Ebolowa']
shp[district=='Edea 1' | district=='Edea 2', district:='Edea']
shp[district=='Kribi 1' | district=='Kribi 2', district:='Kribi']
shp[district=='Nkongsamba 1' | district=='Nkongsamba 2' | district=='Nkongsamba 3', district:='Nkongsamba']
shp[grepl("Douala", district), district:='Douala']

# each of these districts represents a neighborhood in Douala
# these will be summed to a single value for Douala
dt[district=='Bangue', district:='Douala']
dt[district=='Bonassama', district:='Douala'] # actually in Bonaberi - check
dt[district=='Cite Des Palmiers', district:='Douala']
dt[district=='Deido', district:='Douala']
dt[district=='Japoma', district:='Douala']
dt[district=='Logbaba', district:='Douala']
dt[district=='New Bell', district:='Douala']
dt[district=='Nylon', district:='Douala']

# sum douala and recalculate yield
douala = dt[district=='Douala']
douala = douala[ ,.(hts_tst = sum(hts_tst), hts_tst_pos = sum(hts_tst_pos)),
                 by = .(region, district)]
douala[ , yield:=round(hts_tst_pos/hts_tst*100, 1)]

dt = dt[district!='Douala']
dt = rbind(dt, douala)
# --------------------
# import the egpaf regions

labels = shp[ ,.(district = unique(district))]
dt$district %in% labels$district

# ----------------------------------------------------
# merge the data and the shape file and shape long

shp = merge(shp, dt, by='district', all=T)
shp_long = melt(shp, id.vars = c('district', 'id', 'long',
                'lat', 'order', 'hole', 'piece', 'group', 'region'))

# --------------------
# MAPS
# --------------------
# excluding Douala in the counts

# HST_TST
p1 = ggplot(shp[district!='Douala'], aes(x=long, y=lat, group=group, fill=hts_tst)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Purples'), 
        na.value='#d9d9d9') + 
  theme_void(base_size =16) +
  labs(title="Number of HIV tests performed in Q1 FY21", 
       fill="HTS_TST",
       caption = '*Excludes Douala for scale (large counts obscure smaller values)') +
  theme(plot.title=element_text(vjust=-1, size = 18), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))


# HST_TST_POS
p2 = ggplot(shp[district!='Douala'], aes(x=long, y=lat, group=group, fill=hts_tst_pos)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), 
                       na.value='#d9d9d9') + 
  theme_void(base_size =16) +
  labs(title="Number of patients identified as HIV-positive in Q1 FY21", 
       fill="HTS_TST_POS",
       caption = '*Excludes Douala for scale (large counts obscure smaller values)') +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))


p3 = ggplot(shp, aes(x=long, y=lat, group=group, fill=yield)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Reds'), 
                       na.value='#d9d9d9') + 
  theme_void(base_size =16) +
  labs(title="Test positivity rates in Q1 FY21", 
       fill="Yield (%)") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))

p4 = ggplot(shp, aes(x=long, y=lat, group=group, fill=hts_tst)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Purples'), 
                       na.value='#d9d9d9') + 
  theme_void(base_size =16) +
  labs(title="Number of HIV tests performed in Q1 FY21", 
       fill="HTS_TST") +
  theme(plot.title=element_text(vjust=-1, size = 18), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))

p5 = ggplot(shp, aes(x=long, y=lat, group=group, fill=hts_tst_pos)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'Blues'), 
                       na.value='#d9d9d9') + 
  theme_void(base_size =16) +
  labs(title="Number of patients identified as HIV-positive in Q1 FY21", 
       fill="HTS_TST_POS") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))


# --------------------
# douala alone


shp_dou = shp[district!='Douala', hts_tst:=NA]
shp_dou = shp[district!='Douala', hts_tst_pos:=NA]

p6 = ggplot(shp_dou, aes(x=long, y=lat, group=group, fill=hts_tst)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = c('#3f007d', '#3f007d', '#3f007d'), 
                       na.value='#d9d9d9') + 
  theme_void(base_size =16) +
  labs(title="Number of HIV tests performed in Q1 FY21", 
       fill="HTS_TST") +
  theme(plot.title=element_text(vjust=-1, size = 18), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))

p7 = ggplot(shp_dou, aes(x=long, y=lat, group=group, fill=hts_tst_pos)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = c('#08306b', '#08306b', '#08306b'), 
                       na.value='#d9d9d9') + 
  theme_void(base_size =16) +
  labs(title="Number of patients identified as HIV-positive in Q1 FY21", 
       fill="HTS_TST_POS") +
  theme(plot.title=element_text(vjust=-1), 
        plot.caption=element_text(vjust=6), 
        text=element_text(size=14))



# --------------------
# maps all on the same 

pdf(paste0(OutDir, 'hts_maps.pdf'), width = 12, height = 9 )
p1
p2
p3
p4
p5
p6

dev.off()
