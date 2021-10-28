#---------------------
# Exploring then EZQI Data
# Caitlin O'Brien-Carelli
# 10/25/21

#---------------------
# load the packages

rm(list=ls())
library(data.table)
library(stringr)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(readxl)
library(RColorBrewer)
#---------------------

#-------------------------------------------
# set working directories

# set the main directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/prep_ezqi/'

# set the output directory
outDir = paste0(dir, 'outputs/')

# set the output directory
dt = data.table(read.xlsx(paste0(dir, 'QI PrEP Data_13.Oct.21.xlsx')))
#-------------------------------------------

#-------------------------------------------
# CLEAN AND PREP THE DATA
#---------------------

# rename the columns for ease of analysis
setnames(dt, c('proj', 'tech_area', 'objective', 'indicator', 'numerator', 'denominator',
               'country', 'region', 'district', 'facility', 'lead', 'freq',
               'project_start_dt', 'project_end_dt', 'status',
               'period', 'num', 'denom', 'percentage', 'annotation'))

#---------------------
# convert dates to date type variables
dt[ , project_start_dt:=as.Date(dt$project_start_dt, '%m/%d/%Y')]
dt[ , project_end_dt:=as.Date(dt$project_end_dt, '%m/%d/%Y')]

# create variables displaying the project duration
dt[ , length_days:=(project_end_dt - project_start_dt)]
dt[ , length_mos:=round(length_days/30, 1)]

# convert period to date
dt[ , month:=unlist(lapply(str_split(period, '-'), '[', 1))]
dt[ , year:=unlist(lapply(str_split(period, '-'), '[', 2))]

dt[month=='Jan', month1:='01']
dt[month=='Feb', month1:='02']
dt[month=='Mar', month1:='03']
dt[month=='Apr', month1:='04']
dt[month=='May', month1:='05']
dt[month=='Jun', month1:='06']

dt[month=='Jul', month1:='07']
dt[month=='Aug', month1:='08']
dt[month=='Sep', month1:='09']
dt[month=='Oct', month1:='10']
dt[month=='Nov', month1:='11']
dt[month=='Dec', month1:='12']

# set the month as a date
dt[ , date:=as.Date(paste0(month1, '-01-', year), '%m-%d-%Y')]

# check that there are no duplicate months within projects
dt[ , check:=.N, by = .(date, proj)]

# delete interim variables
dt[ , c('month', 'month1', 'year', 'check'):=NULL]

# add a sequential variable to delineate endline and baseline
dt[ , order:=seq(1:length(date)), by = proj]

# add binaries for endline and baseline
dt[order==1, baseline:=TRUE]
dt[order!=1, baseline:=FALSE]

#---------------------
# endline - data set includes values for all months 
# regardless of whether they happened yet

# if the month is after september 2021, drop the row 
# there are no NAs in these data - just 0s
dt = dt[!('2021-09-01' < date)]

#---------------------
# subset the data to the last month any value was reported

# create a list of the last month in which data were reports
reports = dt[num==0 & denom==0 & order!=1 & proj!='LES-G2BPP', .(order_end = min(order)), by = proj]

# merge in the number - this is the week they stopped reporting
dt = merge(dt, reports, by = 'proj', all = T)

# drop any values after the last non-zero value is reported
dt = dt[order < order_end | is.na(order_end)]

#---------------------
# set the latest as the max order by project
dt[ , check:=.N, by = proj]
dt[order==check, latest:=TRUE]
dt[is.na(latest), latest:=FALSE]

# drop unnecessary variables
dt[ ,c('check', 'order_end'):=NULL]
#---------------------

#---------------------

# sort by indicator
dt[indicator=="% of tested coupples with discordant results and successfully linked into care", ind:='sero']
dt[indicator!="% of tested coupples with discordant results and successfully linked into care", ind:='prep']

# drop indicator for ease of viewing data 
dt[ ,indicator:=NULL]

# delete the percentage indicator and re-calculate
dt[ , percentage:=NULL]
dt[ , percentage:=round(100*num/denom, 1)]

#-------------------------------------------
# EXPLORE THE DATA

# basic descriptive statistics - how many of each?
nrow(dt)
dt[ ,unique(country)]
dt[ ,unique(region)]
dt[ ,unique(facility)]

# determine the number of projects by technical area
dt[ ,length(unique(proj))]
dt[ ,length(unique(proj)), by = ind]

# basic descriptive statistics - how many of each?
dt[,unique(tech_area)]
dt[,unique(objective)]
dt[,unique(ind)]
dt[,unique(proj), by = tech_area]
dt[,unique(freq)]

# what is the current status of all projects
dt[, length(unique(proj)), by = status]

#--------------------
# PROJECT DURATION

# run the range of dates 
dt[,range(project_start_dt)]
dt[,range(project_end_dt)]

# data quality check - start date is before end date
dt[ project_end_dt < project_start_dt]

# data quality check - project length
days_tbl = dt[proj!='LES-G2BPP', .(days = unique(length_days)), by = proj] # project LES-G2BPP has the same start/end dates
mos_tbl = dt[proj!='LES-G2BPP', .(months = unique(length_mos)), by = proj]

# mean and median duration
days_tbl[ , mean(days)]
mos_tbl[ , mean(months)]
days_tbl[ , range(days)]
mos_tbl[ , range(months)]
days_tbl[ , median(days)]
mos_tbl[ , median(months)]

#-------------------------------------------
# OUTCOME ANALYSIS

#--------------------
# check that all start dates are in the month following the first reported value
base = dt[order==1, .(proj, project_start_dt, date, num, denom, percentage)]

# baseline statistics - clients eligible for prep
dt[order==1, mean(denom)]
dt[order==1, range(denom)]
dt[order==1, median(denom)]

# baseline statistics - clients newly enrolled on prep
dt[order==1, mean(num)]
dt[order==1, range(num)]
dt[order==1, median(num)]

# baseline statistics - percentage enrollment
dt[order==1 & num==0 & denom==0]
dt[order==1 & denom!=0, mean(percentage)]
dt[order==1 & denom!=0, range(percentage)]
dt[order==1 & denom!=0, median(percentage)]

#--------------------
# endline statistics (most recent month within current data)

# latest statistics - clients eligible for prep
dt[latest==T, mean(denom)]
dt[latest==T, range(denom)]
dt[latest==T, median(denom)]

#--------------------
# endline versus baseline

base = dt[baseline==T, .(proj, base_num = num, base_denom =  denom, base_per = percentage)]
lat = dt[latest==T, .(proj, end_num = num, end_denom =  denom, end_per = percentage)]
comp = merge(base, lat, by = 'proj')

# calculate the differences
comp[ , num_dif:=end_num - base_num]
comp[ , denom_dif:=end_denom - base_denom]
comp[ , per_dif:=(end_per - base_per)]

# check the change
comp[0 < denom_dif]
comp[denom_dif < 0]
comp[denom_dif == 0]
comp[ , mean(denom_dif)]

#--------------------
# endline statistics - clients newly enrolled on prep
dt[latest==T, mean(num)]
dt[latest==T, range(num)]
dt[latest==T, median(num)]

# check the change
comp[0 < num_dif]
comp[num_dif < 0]
comp[num_dif == 0]
comp[ , mean(num_dif)]

# change in percentage enrollment over time
comp[ ,mean(base_per, na.rm=T)]
comp[ ,mean(end_per, na.rm=T)]

comp[!is.na(per_dif), mean(per_dif)]
comp[ , per_dif_alt:=per_dif]
comp[is.na(per_dif_alt), per_dif_alt:=end_per]
comp[is.na(per_dif_alt), per_dif_alt:=0] # one project has a 0 for base and endline
comp[ , mean(per_dif_alt)]

comp[0 < per_dif]
comp[per_dif < 0]
comp[per_dif==0]

#-------------------------------------------
# export a list of unique objectives

objs = dt[ ,unique(objective), by = proj]
write.xlsx(objs, paste0(outDir, 'project_objectives.xlsx'))


#-------------------------------------------
# DATA VISUALIZATIONS
#-------------------------------------------
# export a table for most figures for report

#--------------------
# number of projects by implementation status
status_tbl =  dt[, .(total = length(unique(proj))), by = status][rev(order(total))]
write.xlsx(status_tbl, paste0(outDir, 'project_status.xlsx'))

p1_status = ggplot(status_tbl, aes(x = reorder(status, -total), y = total, fill = status))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = total),
          vjust = -0.5, position = position_dodge(0.8))+
  scale_fill_manual(values = brewer.pal(4, 'YlOrRd'))+
  theme_minimal()+
  labs(title = 'Number of projects by implementation status', 
       x = '', y = 'Number of projects')+
  theme(legend.position = 'none', text = element_text(size=16)) 

#--------------------
# project duration 

write.xlsx(days_tbl, paste0(outDir, 'duration_days.xlsx'))
write.xlsx(mos_tbl, paste0(outDir, 'duration_months.xlsx'))

# project duration by project in days 
p2_dur_days = ggplot(days_tbl, aes(x = reorder(proj, -days), y = days, fill = '#cb181d'))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = '#cb181d')+
  theme_minimal()+
  labs(title = 'Duration of project implementation in days', 
       x = '', y = 'Days')+
  theme(legend.position = 'none', text = element_text(size=16),
        axis.text.x = element_text(size = 10, angle = 90)) 

# project duration by project in months
p3_dur_mos = ggplot(mos_tbl, aes(x = reorder(proj, -months), y = months, fill = '#cb181d'))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = '#6baed6')+
  theme_minimal()+
  labs(title = 'Duration of project implementation in months', 
       x = '', y = 'Months')+
  theme(legend.position = 'none', text = element_text(size=16),
        axis.text.x = element_text(size = 10, angle = 90)) 


#--------------------
# BASELINE VALUES

write.xlsx(base, paste0(outDir, 'baseline_values.xlsx'), overwrite = T)
write.xlsx(comp, paste0(outDir, 'compare_values.xlsx'), overwrite = T)

#--------------------
# melt the data to loop through visualizations

dt_sub = dt[ ,.(proj, region, district, facility, project_start_dt, project_end_dt, 
           status, length_days, date, order, baseline, latest,
           num, denom, percentage)]

dt_long = melt(dt_sub, id.vars = c('proj', 'region', 'district', 'facility',
                     'project_start_dt', 'project_end_dt', 
                      'status', 'length_days', 'date', 'order',
                     'baseline', 'latest'))

dt_long$variable = factor(dt_long$variable, c('denom', 'num', 'percentage'), 
       c('Eligible for PrEP', 'Enrolled on PrEP', 'Percent Enrolled (%)'))

list_of_plots = NULL
i = 1

for (p in unique(dt$proj)) {

project_name = as.character(p)
site = as.character(dt_long[proj==p, unique(facility)])

list_of_plots[[i]] = ggplot(dt_long[proj==p & variable!='Percent Enrolled (%)'], 
  aes(x = date, y = value, color = variable))+
  geom_line()+
  geom_point()+
  scale_color_manual(values = c('#a50f15', '#2171b5'))+
  theme_minimal()+
   labs(title = paste0('Project ', project_name),
         subtitle = site, x = '',
         y = 'Clients', color = ' ')+
  theme(text = element_text(size=18))

i = i+1

}

#-------------------------------------------
# export a pdf of the values
pdf(paste0(outDir, 'prep_ezqi_viz.pdf'), width = 12, height = 9)
#--------------------

p1_status 
p2_dur_days
p3_dur_mos

# print a list of plots that shows trends by project
for (p in seq(1:length(list_of_plots))) {
  print(list_of_plots[[p]]) }
#--------------------
dev.off()
#-------------------------------------------
