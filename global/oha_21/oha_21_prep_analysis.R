# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/26/21
# 2021 OHA Meeting Analysis 
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
library(ggrepel)
# --------------------

# ----------------------------------------------
# Files and directories

# set the home directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/oha_21/'
setwd(dir)

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Global/Presentations/OHA Meeting 2021/'

# mapping directory
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/maps/r_shape_files/'

# read in the data 
dt = data.table(read.csv(paste0(dir, 'oha_21_prep.csv')))

# ----------------------------------------------

# --------------------
# prep the data for analysis

# rename the variables for analysis 
setnames(dt, c('award','country', 'prep_new', 'prep_curr',
               'fq', 'date', 'sex', 'age'))
               
# shape the data long for visualization
idVars = c('award','country', 'sex', 'age', 'fq', 'date')
dt = melt(dt, id.vars = idVars)

# --------------------
# factor the variables

dt$fq = factor(dt$fq, c("FY20 Q2", "FY20 Q4",
        "FY21 Q1", "FY21 Q2", "FY21 Q3"),
        c("Q2 FY20", "Q4 FY20",
          "Q1 FY21", "Q2 FY21", "Q3 FY21"))

dt$date = factor(dt$date, c('Jan-Mar 2020', 'Oct-Dec 2020',
      'Jul-Sep 2020', 'Jan-Mar 2021', 'Apr-Jun 2021'), 
      c('Jan-Mar 2020', 'Oct-Dec 2020',
        'Jul-Sep 2020', 'Jan-Mar 2021', 'Apr-Jun 2021'))

dt$age = factor(dt$age, c("Unknown Age", "15-19", "20-24", "25-29",
                          "30-34", "35-39", "40-44", "45-49", "50+"),
                c("Unknown", "15-19", "20-24", "25-29",
                  "30-34", "35-39", "40-44", "45-49", "50+")) 

# format the grant names to match style guide
dt[award=='USAID RHITES SW', award:='USAID RHITES-SW']
dt[award=='USAID Eswatini', award:='USAID EHPCS']

# create an award name variable without the donor name
dt[ , award_alt:=trimws(gsub('USAID', '', award))]

# rearrange the order
dt = dt[ ,.(value), by = .(country, award, award_alt, fq, date,
        sex, age, variable)]

# --------------------

# ----------------------------------------------
# export an Excel file for Excel graphs 
dt_excel = copy(dt)

# change the names of variables to appear correctly on graphs
dt_excel[variable=='prep_curr', variable:='PREP_CURR']
dt_excel[variable=='prep_new', variable:='PREP_NEW']
setnames(dt_excel, c('Country', 'Award Full', 'Award', 'Quarter',
                     'Fiscal Quarter', 'Sex', 'Age', 'Variable',
                     'Value'))

# export the data, over writing the previous version 
write.xlsx(dt_excel, paste0(outDir, 'OHA_21_PREP_figures.xlsx'), overwrite = T)

# delete the data set
dt_excel = NULL

# ----------------------------------------------

# --------------------
# CREATE MAPS 

# ----------------------------------------------
# PREP MAPS

# --------------------
# import the shape file and convert to a data table
map = shapefile(paste0(mapDir, 'africa_shape_file/1.shp'))
coord = data.table(fortify(map))

# merge in the names of the countries to match the data 
names = data.table(cbind(country = c(map@data$COUNTRY), id =  c(0, seq(1:47))))
coord = merge(coord, names, by = 'id')

# --------------------
# create a summed table for new prep enrollments

# create each summed variable separately 
pn_20_21 = dt[variable=='prep_new',.(pn_20_21 = sum(value, na.rm = T)), 
   by = c('country', 'award_alt')]
pn_20 = dt[variable=='prep_new' & grepl('20', fq),.(pn_20 = sum(value, na.rm = T)), 
              by = c('country')]
pn_21 = dt[variable=='prep_new' & grepl('21', fq),.(pn_21 = sum(value, na.rm = T)), 
           by = c('country')]
pn_q1 = dt[variable=='prep_new' & fq=='Q1 FY21',.(pn_q1 = sum(value, na.rm = T)), 
           by = c('country')]
pn_q2 = dt[variable=='prep_new' & fq=='Q2 FY21',.(pn_q2 = sum(value, na.rm = T)), 
           by = c('country')]
pn_q3 = dt[variable=='prep_new' & fq=='Q3 FY21',.(pn_q3 = sum(value, na.rm = T)), 
           by = c('country')]

# bind them together
x = merge(pn_20_21, pn_20, by = 'country', all = T)
y = merge(pn_21, pn_q1, by = 'country', all = T)
z = merge(pn_q2, pn_q3, by = 'country', all = T)
pn = merge(x, y, by = 'country', all = T)
pn = merge(pn, z, by = 'country', all = T)

# fix the missing value for Q3 in DRC (summed to 0)
pn[country=='DRC', pn_q3:=NA]

# drop the useless data tables
rm(x, y, z, pn_20_21, pn_20, pn_21, pn_q1, pn_q2, pn_q3)

# --------------------
# create a summed table for the current enrollments
pc = dt[variable=='prep_curr',.(value = sum(value, na.rm = T)), 
         by = c('country', 'fq')]
pc = dcast(pc, country~fq)
setnames(pc, c('country', 'pc2_20', 'pc4_20',
               'pc1_21', 'pc2_21', 'pc3_21'))

# --------------------
# MERGE THE PREP_NEW DATA WITH THE SHAPE FILE
coord = merge(coord, pn, by = 'country', all = T)
coord = merge(coord, pc, by = 'country', all = T)

# --------------------
# create a map file shaped long
idVars = c('country', 'id', 'long', 'lat', 'order', 'hole', 'piece', 'group', 'award_alt')
coord_long = melt(coord, id.vars = idVars)

# ----------------------------------------------

# --------------------
# calculate rates of change





# --------------------

# --------------------
# # create labels

# create the labels
pn_labels = data.table(coordinates(map))
setnames(pn_labels, c('long', 'lat'))
pn_labels = cbind(pn_labels, country = names$country)

# add specific variables by indicator and time period
pn_labels = merge(pn_labels, pn, by = 'country', all = T)

# prep new labels
pn_labels[!is.na(pn_20_21), pn_20_21_label:=paste0(country, ': ', pn_20_21)]
pn_labels[!is.na(pn_q1), pn_q1_label:=paste0(country, ': ', pn_q1)]
pn_labels[!is.na(pn_q2), pn_q2_label:=paste0(country, ': ', pn_q2)]
pn_labels[!is.na(pn_q3), pn_q3_label:=paste0(country, ': ', pn_q3)]

# melted labels for the facet wrap
pn_labels_long = pn_labels[ ,.(country, long, lat, 
                               pn_q1_label, pn_q2_label, pn_q3_label)]
pn_labels_long = melt(pn_labels_long, id.vars = c('country', 'long', 'lat'))

# facto these specific labels to match the facet of the data 
pn_labels_long$variable = factor(pn_labels_long$variable,
                                 c('pn_q1_label', 'pn_q2_label', 'pn_q3_label'),
                                 c('Oct.-Dec. 2020', 'Jan.-March 2021', 'Apr.-June 2021'))

# --------------------
# fix the map by creating a South Africa specific map
s_africa_layer = geom_polygon(aes(x = long, y = lat, group = group),
                              data = coord[country=='South Africa' & hole==F], fill = NA, 
                              color = 'black')

# ----------------------------------------------
# MAPS
# ----------------------------------------------

# ----------------------------------------------
# NEWLY ENROLLED ON PREP

# new clients enrolled on prep, Q3 FY21
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=pn_q3)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(9, 'YlGn'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="PREP_NEW", 
       title = 'New clients enrolled on PrEP (PREP_NEW), Apr.-June 2021',
       caption = '*IHAP has not yet reported new PrEP enrollments in the third quarter of Fiscal Year 21')+
  theme(text=element_text(size=24), plot.caption = element_text(size = 12))+
  s_africa_layer+
  geom_label_repel(data = pn_labels, aes(label = pn_q3_label, 
              x=long, y=lat, group=country), inherit.aes=FALSE, size=5)

# total new clients enrolled, FY20 - FY21
ggplot(coord[country!='South Africa'], aes(x=long, y=lat, group=group, fill=pn_20_21)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(5, 'Blues'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  labs(fill="PREP_NEW", 
       title = 'Total new clients enrolled on PrEP, Q1 FY20 - Q3 FY21',
       caption = '*IHAP has not yet reported new PrEP enrollments in the third quarter of Fiscal Year 21')+
  theme(text=element_text(size=24), 
  plot.caption = element_text(size = 12))+
  s_africa_layer+
  geom_label_repel(data = pn_labels, aes(label = pn_20_21_label, 
       x=long, y=lat, group=country), inherit.aes=FALSE, size=5)






# facet map by quarter
ggplot(coord_long_1_3[country!='South Africa'], 
       aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors = brewer.pal(5, 'Blues'), na.value='#ffffff') + 
  theme_void(base_size =16) +
  facet_wrap(~variable)+
  labs(fill="PREP_NEW", 
       title = 'Total new clients enrolled on PrEP (PREP_NEW)',
       caption = '*IHAP has not yet reported new PrEP enrollments in the third quarter of Fiscal Year 21')+
  theme(text=element_text(size=24), 
        plot.caption = element_text(size = 12))+
  s_africa_layer+
  geom_label_repel(data = pn_labels_long, aes(label = value , 
     x=long, y=lat, group=country), inherit.aes=FALSE, size=5)

coord_long_1_3 = coord_long[variable %in% c('pn_q1', 'pn_q2', 'pn_q3')]

coord_long_1_3$variable = factor(coord_long_1_3$variable,
                                 c('pn_q1', 'pn_q2', 'pn_q3'),
                                 c('Oct.-Dec. 2020', 'Jan.-March 2021', 'Apr.-June 2021'))


