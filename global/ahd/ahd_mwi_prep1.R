#---------------------
# Malawi AHD Initial Exploratory Analysis
# Caitlin O'Brien-Carelli
# 10/5/21

#---------------------
# load the packages

rm(list=ls())
library(data.table)
library(stringr)
library(dplyr)
library(openxlsx)
lobrary(ggplot2)
#---------------------

#-----------------------------------
# read in the files

# set the working directory
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/malawi/'

# set the output directory
outDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Global/AHD/outputs/malawi/'

# read in the master data file
df = data.table(read.csv(paste0(dir, 'AHD_M&E_Valueslabels.csv')))

#-----------------------------------

#---------------------
# set up the data for analysis

# check for repeat patient ids
df[ , pt_count:=.N, by = participant_number]
df[pt_count==2][order(participant_number)]

# there are 8 repeat ids - none have the same DOB
# do not appear to be duplicate entries
# create alternate IDs with no duplicates
df[pt_count==2, rank:=seq(1:2), by = participant_number]
df[pt_count==2 & rank==2, 
   alt_participant_number:=paste0(participant_number, 'b')]
df[is.na(alt_participant_number),
   alt_participant_number:=as.character(participant_number)]
df[ ,c('pt_count', 'rank'):=NULL]

#-----------------------------------
# EXPLORATORY ANALYSIS
#---------------------

#---------------------
# initial analysis of participants

# subset to just participant ids and study eligibility
dt = cbind(df[ , 3:24], alt_id = df$alt_participant_number)

# rename the variables for ease of analysis
setnames(dt, c('site_id', 'capture_date', 'pre_post',
  'id', 'studyid', 'p_elig', 
  'ahd_diag_dt_class', 'ahd_diag_dt', 'dob',
  'sex', 'fsex', 'pt_hiv_knowledge',
  'hiv_tested', 'dt_tested', 'hiv_test_result',
  'cd4_done', 'dt_cd4_done', 'cd4_results_returned_dt',
  'cd4_count', 'who_staged', 'pt_who_stage', 
  'dt_who_staged', 'alt_id'
))

# replace ids with dq issue with unique ids
dt[ , id:=alt_id]
dt[ ,alt_id:=NULL]

#---------------------
# prepare the variables for analysis

# convert dates to date type variables
dt[ , capture_date:=as.Date(capture_date, '%m/%d/%Y')]
dt[ , ahd_diag_dt:=as.Date(ahd_diag_dt, '%m/%d/%Y')]
dt[ , dob:=as.Date(dob, '%m/%d/%Y')]
dt[ , dt_tested:=as.Date(dt_tested, '%m/%d/%Y')]
dt[ , dt_cd4_done:=as.Date(dt_cd4_done, '%m/%d/%Y')]
dt[ , cd4_results_returned_dt:=as.Date(cd4_results_returned_dt, '%m/%d/%Y')]
dt[ , dt_who_staged:=as.Date(dt_who_staged, '%m/%d/%Y')]

# calculate age
dt[ , age:=as.numeric(floor((ahd_diag_dt - dob)/365))]
dt[age < 0, age:=0] # one mistaken dob (before ahd_elig)
# when ahd eligibility date is missing, sub in who staging date
dt[is.na(age), age:=as.numeric(floor((dt_who_staged - dob)/365))]

# factor the variables with associated labels
dt$pre_post = factor(dt$pre_post, c(1, 2), c('Baseline', 'Endline'))
dt$sex = factor(dt$sex, c(1, 2), c('Male', 'Female'))

# label the site names
dt[ , site:=unlist(lapply(str_split(studyid, '-'), '[', 1))]

#---------------------
# create an age category variable
dt[age < 1 , age_cat:='<1']
dt[0 < age & age < 5 , age_cat:='1-4']
dt[4 < age & age < 10 , age_cat:='5-9']
dt[9 < age & age < 15 , age_cat:='10-14']
dt[14 < age & age < 20 , age_cat:='15-19']
dt[19 < age & age < 25 , age_cat:='20-24']

dt[24 < age & age < 30 , age_cat:='25-29']
dt[29 < age & age < 35 , age_cat:='30-34']
dt[34 < age & age < 40 , age_cat:='35-39']
dt[39 < age & age < 45 , age_cat:='40-44']
dt[44 < age & age < 50 , age_cat:='45-49']
dt[49 < age, age_cat:='50+']

dt$age_cat = factor(dt$age_cat, c('<1', '1-4', '5-9', '10-14', '15-19',
                     '20-24', '25-29', '30-34',
                     '35-39', '40-44', '45-49', '50+'))

#-----------------------------------
# descriptive statistics
#-----------------------------------
# sex distribution between cohorts 
dt[, length(unique(id)), by = sex]
dt[, length(unique(id)), by = pre_post]
dt[, length(unique(id)), by = .(sex, pre_post)]

# age distribution between cohorts 
dt[ , median(age), by = pre_post]
dt[ , mean(age), by = pre_post]
dt[ , range(age), by = pre_post]

# count of under 5s
dt[age < 5, length(unique(id)), by = pre_post]


#-----------------------------------
# TABLES
#-----------------------------------
# table of cohort distribution by sex, site
tbl1 = dt[, .(value = length(unique(id))), by = .(site, sex, pre_post)]
tbl1[ , variable:=paste(pre_post, sex)]
tbl1 = dcast(tbl1, site~variable)

# oputput the table 
write.xlsx(tbl1, paste0(outDir, 'site_sex_cohort.xlsx'))

#---------------------
# table of cohort distribution by sex, who stage 
tbl2 = dt[, .(value = length(unique(id))), 
          by = .(pt_who_stage, sex, pre_post)]
tbl2[ , variable:=paste(pre_post, sex)]
tbl2 = dcast(tbl2, pt_who_stage~variable)

# oputput the table 
write.xlsx(tbl2, paste0(outDir, 'stage_sex_cohort.xlsx'))

#---------------------
# table of cohort distribution by cohort, sex age category
tbl3 = dt[, .(value = length(unique(id))), 
          by = .(age_cat, sex, pre_post)]
tbl3[ , variable:=paste(pre_post, sex)]
tbl3 = dcast(tbl3, age_cat~variable)

# oputput the table 
write.xlsx(tbl3, paste0(outDir, 'age_sex_cohort.xlsx'))

#-----------------------------------
# VISUALIZATIONS
#-----------------------------------
# overall picture of the cohort 
#---------------------

#---------------------
# plot of the cohorts by sex
distr = dt[ ,.(Participants = length(unique(id))), by = .(pre_post)]
sex_distr = dt[ ,.(Participants = length(unique(id))), by = .(sex, pre_post)]

# total size of cohorts
p1 = ggplot(distr, aes(x = pre_post, y = Participants, fill = pre_post))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = c('#ffc641', '#73afb6'))+
  geom_text(aes(label = Participants),
            vjust = -0.5, position = position_dodge(0.8))+
  theme_bw()+
  labs(title = 'Total participants by cohort (n = 1,118)',
       x = 'Cohort')+
  theme(legend.position = "none")

# sex distribution of cohorts
p2 = ggplot(sex_distr, aes(x = pre_post, y = Participants, fill = sex))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = c('#ffc641', '#c02034'))+
  geom_text(aes(label = Participants),
            vjust = -0.5, position = position_dodge(0.8))+
  theme_bw()+
  labs(title = 'Sex distribution by cohort', x = 'Cohort',
       fill = 'Sex')

# sex distribution of cohorts - stacked 
p3 = ggplot(sex_distr, aes(x = pre_post, y = Participants, fill = sex,
                      label = Participants))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c('#ffc641', '#c02034'))+
  geom_text(position = position_stack(0.8))+
  theme_bw()+
  labs(title = 'Sex distribution by cohort - stacked', x = 'Cohort',
       fill = 'Sex')

#---------------------
# age distributions (cannot save as a vector, added to PDF)
hist(dt$age, xlab = 'Age', main = 'Histogram of Age')

hist(dt[pre_post=='Baseline']$age, xlab = 'Age', 
          main = 'Histogram of Age: Baseline Cohort')

hist(dt[pre_post=='Endline']$age, xlab = 'Age', 
          main = 'Histogram of Age: Endline Cohort')

#---------------------
# distributions by site 

fac_distr = dt[ ,.(Participants = length(unique(id))), by = .(sex, pre_post, site)]

# total size of cohorts
p7 = ggplot(fac_distr, aes(x = site, y = Participants, fill = sex))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c('#c02034', '#2896d0' ))+
   facet_wrap(~pre_post)+
  theme_minimal()+
  labs(title = 'Total participants site, sex',
       x = 'Site', fill = 'Sex')

 
# total size of cohorts
p8 = ggplot(fac_distr, aes(x = pre_post, y = Participants, fill = sex))+
   geom_bar(stat = "identity", position = position_dodge())+
   geom_text(aes(label = Participants),
             vjust = -0.5, position = position_dodge(0.8))+
   scale_fill_manual(values = c('#c02034', '#2896d0' ))+
   facet_wrap(~site)+
   theme_minimal()+
   labs(title = 'Total participants by site, sex', x = '', fill = 'Sex')
 
#---------------------
 # site based age histograms 
 
age_distr = dt[ ,.(Participants = length(unique(id))), by = .(age, pre_post, site)]

# age distribution by site - baseline
p9 = ggplot(age_distr[pre_post=='Baseline'], aes(x = age, y = Participants, fill = '#c02034'))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = c('#c02034'))+
  facet_wrap(~site)+
  theme_minimal()+
  labs(title = 'Age distribution by site: baseline cohort', x = 'Age')+
  theme(legend.position = "none")


# age distribution by site - endline
p10 = ggplot(age_distr[pre_post=='Endline'], aes(x = age, y = Participants, fill = '#ffc641'))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual(values = c('#ffc641'))+
  facet_wrap(~site)+
  theme_minimal()+
  labs(title = 'Age distribution by site: endline cohort', x = 'Age')+
  theme(legend.position = "none")

#---------------------
# PRINT THE PLOTS

pdf(paste0(outDir, 'exploratory_plots.pdf'), width = 12, height = 9 )
p1
p2
p3

hist(dt$age, xlab = 'Age', main = 'Histogram of Age')

hist(dt[pre_post=='Baseline']$age, xlab = 'Age', 
     main = 'Histogram of Age: Baseline Cohort')

hist(dt[pre_post=='Endline']$age, xlab = 'Age', 
     main = 'Histogram of Age: Endline Cohort')

p7
p8
p9
p10
dev.off()

#---------------------


