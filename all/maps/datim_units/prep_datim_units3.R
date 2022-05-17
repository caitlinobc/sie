# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/22/21
# PREP FUNCTION FOR DATIM UNITS
# Preps the DATIM units for analysis
# includes adding latitude/longitude for sites
# sourced by extract_all_datim_sites2.R

# ----------------------------------------------

# ----------------------------------------------
# initial data cleaning

# rename the data set for ease of coding
dt = full_data
# remove(full_data)

# country and higher level region are the same for all countries
dt[level==3, type:='country']
dt[level==4,type:='region']
dt[level==5,type:='district']

# level 7 is always site
dt[level==7, type:='site']

# some countries have site at level 6, some at 7
cs = dt[ , .(max = max(level)), by = country]
cs = cs[max==6]

# in countries with no seventh level, set type = site
dt[country %in% cs$country & level==6, type:='site']

# in countries with a seventh level, set type = site
dt[country %in% cs$country & level==6, type:='site']


