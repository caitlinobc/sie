# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/15/2021
# Malawi tables
# Create and export tables for analysis
# Sources the data prepped in ahd_mwi_prep1.R
# ----------------------------------------------

# ------------------------
# load packages

rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(plyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)
# ------------------------

# ------------------------
# files and directories

# set the working directory to the ahd data sets
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/malawi/'
setwd(dir)

# set the output directory for prepped data 
prepDir = paste0(dir, 'prepped/')

# set the output directory for tables
outDir = paste0(dir, 'outputs/tables/')

# ------------------------
# import the data
dt = readRDS(paste0(prepDir, 'full_data.RDS'))

# ------------------------

# ----------------------------------------------
# DEMOGRAPHIC & ELIGIBILITY TABLES 
# ----------------------------------------------
# create some demographic tables to show study eligibility

# ------------------------
# participants by sex, age category, period
dt_sa = dt[ ,.(value = length(unique(pid))), by =.(sex, age_cat, period)]
dt_sa = dcast(dt_sa, age_cat~period+sex)
dt_sa[ , b_Total:=(b_Female+b_Male)]
dt_sa[ , e_Total:=(e_Female+e_Male)]
dt_sa = dt_sa[ ,.(age_cat,b_Female, b_Male, b_Total, e_Female, e_Male, e_Total)]

# export the table
write.csv(dt_sa, paste0(outDir, 'participants_age_sex.csv'))
# ------------------------

# ------------------------
# participants by site, sex, period
dt_ss = dt[ ,.(value = length(unique(pid))), by =.(sex, site, period)]
dt_ss = dcast(dt_ss, site~period+sex)
dt_ss[ , b_Total:=(b_Female+b_Male)]
dt_ss[ , e_Total:=(e_Female+e_Male)]
dt_ss = dt_ss[ ,.(site,b_Female, b_Male, b_Total, e_Female, e_Male, e_Total)]

# export the table
write.csv(dt_ss, paste0(outDir, 'participants_site_sex.csv'))
# ------------------------

# ------------------------
# age distribution by site, period
dt_as = dt[ ,.(value = length(unique(pid))), by =.(age_cat, site, period)]
dt_as = dcast(dt_as, age_cat~period+site)

# export the table
write.csv(dt_as, paste0(outDir, 'participants_site_age.csv'))
# ------------------------

# ------------------------
# study eligibility by sex, period
dt_elig = dt[, .(value = length(unique(pid))) , by = .(ahd_elig, sex, period)]
dt_elig2 = dt[, .(value = length(unique(pid))) , by = .(ahd_elig, period)]
dt_elig2[ , sex:='Total']
dt_elig = rbind(dt_elig, dt_elig2)
dt_elig = dcast(dt_elig, ahd_elig~period+sex)

# export the table
write.csv(dt_elig, paste0(outDir, 'eligibilty_sex.csv'))
# ------------------------

# ----------------------------------------------
# OUTCOME TABLES: HIV TESTING/STATUS
# ----------------------------------------------

# ------------------------
# calculate the extent of missingness in the dates of positive hiv tests
dt[!is.na(dtpos), length(unique(pid)),by = period]
dt[!is.na(dtpos), length(unique(pid))]
dt[ , range(dtpos, na.rm=T)]

# length of time between positive test and study eligibility date 
dt[ , time2ahd_days:=(ahd_dt-dtpos)]

# 19 people had an ahd diagnosis before the positive test - likely confirmatory
dt[time2ahd_days <0, time2ahd_days:=NA]

# now calculate in months
dt[ , time2ahd_mos:=round(((ahd_dt-dtpos)/30), 1)]

# create a table of mean and median length of time to diagnosis - all patients
pos_tot1 = dt[ , .(mean = round(mean(time2ahd_mos, na.rm=T), 1)), by = period]
pos_tot2 = dt[ , .(median = round(median(time2ahd_mos, na.rm=T), 1)), by = period]
pos_tot = merge(pos_tot1, pos_tot2, by = 'period', all.x=T)
pos_tot[ , sex:='Total']

# table of mean and median length of time by sex
pos_tot3 = dt[ , .(mean = round(mean(time2ahd_mos, na.rm=T), 1)), by = .(period, sex)]
pos_tot4 = dt[ , .(median = round(median(time2ahd_mos, na.rm=T), 1)), by = .(period, sex)]
pos_tot5 = merge(pos_tot3, pos_tot4, by = c('period', 'sex'), all = T)

# bind them together and create the table
pos_tot = rbind(pos_tot5, pos_tot)
pos_tot[ , mean:=as.numeric(mean)]
pos_tot[ , median:=as.numeric(median)]

# reshape 
mpt = dcast(pos_tot, sex~period, value.var = 'mean')
mpt2 = dcast(pos_tot, sex~period, value.var = 'median')
mpt = merge(mpt, mpt2, by = 'sex') # .y will indicate median
mpt = mpt[ ,.(Sex = sex, Mean = b.x, Median = b.y, Mean = e.x, Median = e.y)]

# export the table
write.csv(mpt, paste0(outDir, 'hiv_diag_dt.csv'))
# ------------------------

# ------------------------
# know status - write in
dt[period=='b', table(knwstat)]
dt[period=='e', table(knwstat)]
dt[is.na(knwstat), length(unique(pid)), by = period]
# ------------------------

# ------------------------
# tested for HIV - write in
dt[period=='b', table(hivtest)]
dt[period=='e', table(hivtest)]
dt[is.na(hivtest), length(unique(pid)), by = period]

dt[!is.na(hivtest) & is.na(hivresult)]
# ------------------------

# ------------------------
# calculate age at diagnosis - use days and divide by 365 for years

# length of time between positive test and study eligibility date 
dt[ , time2diag:=(dtpos-dob)]

# create a table of mean and median length of time to diagnosis - all patients
d_tot1 = dt[ , .(mean = mean(time2diag, na.rm=T)), by = period]
d_tot2 = dt[ , .(median = median(time2diag, na.rm=T)), by = period]
d_tot = merge(d_tot1, d_tot2, by = 'period', all.x=T)
d_tot[ , sex:='Total']

# table of mean and median length of time by sex
d_tot3 = dt[ , .(mean = mean(time2diag, na.rm=T)), by = .(period, sex)]
d_tot4 = dt[ , .(median = median(time2diag, na.rm=T)), by = .(period, sex)]
d_tot5 = merge(d_tot3, d_tot4, by = c('period', 'sex'), all = T)

# bind them together and create the table
d_tot = rbind(d_tot5, d_tot)
d_tot[ , mean:=as.numeric(mean)]
d_tot[ , median:=as.numeric(median)]

# change the units to years for ease of interpretation
d_tot[ , mean:=round((mean/365), 1)]
d_tot[ , median:=round((median/365), 1)]

# reshape 
dpt = dcast(d_tot, sex~period, value.var = 'mean')
dpt2 = dcast(d_tot, sex~period, value.var = 'median')
dpt = merge(dpt, dpt2, by = 'sex')
dpt = dpt[ ,.(Sex = sex, Mean = b.x, Median = b.y, Mean = e.x, Median = e.y)]

# export the table
write.csv(dpt, paste0(outDir, 'age_at_hiv_diag_dt.csv'))

# ------------------------

# ----------------------------------------------
# OUTCOME TABLES: CD4 TESTING 
# ----------------------------------------------

#-------------------
# check the cd4 data
#-------------------

# cd4done_after_ahdelig           
# cd4_after_ahdelig_dt           
# cd4_afterahdelig_res_ret_dt    
# cd4_after_ahdelig_result

#-------------------

#-------------------
# basic descriptive statistics for cd4
table(dt$cd4done_after_ahdelig)
dt[!is.na(cd4done_after_ahdelig), length(unique(pid)), by = period]
dt[is.na(cd4done_after_ahdelig), length(unique(pid))]
dt[is.na(cd4done_after_ahdelig), length(unique(pid))]/nrow(dt)
#-------------------

#-------------------
# number receiving cd4 after ahd by sex, period
cd4_s1 = dt[!is.na(cd4done_after_ahdelig), .(value = length(unique(pid))), by = .(type = cd4done_after_ahdelig,  sex, period)]
cd4_s2 = dt[is.na(cd4done_after_ahdelig), .(value = length(unique(pid))), by = .(sex, period)]
cd4_s2[ , type:='Missing']
cd4_s = rbind(cd4_s1, cd4_s2)
cd4_s =  dcast(cd4_s, type~period+sex)

# export the table
write.csv(cd4_s, paste0(outDir, 'cd4_done_sex.csv'))
#-------------------

#-------------------
# mean cd4 by sex
cd4_m1 = dt[!is.na(cd4_after_ahdelig_result), .(value = round(mean(cd4_after_ahdelig_result), 1)), 
            by = .(sex, period)]
cd4_m2 = dt[!is.na(cd4_after_ahdelig_result), .(value = round(mean(cd4_after_ahdelig_result), 1),
                                                sex = 'Total'), by = period]
cd4_m = rbind(cd4_m1, cd4_m2)
cd4_m =  dcast(cd4_m, sex~period)

# export the table
write.csv(cd4_m, paste0(outDir, 'cd4_result_sex_mean.csv'))
#-------------------

#-------------------
# median cd4 by sex
cd4_md1 = dt[!is.na(cd4_after_ahdelig_result), .(value = round(median(cd4_after_ahdelig_result), 1)), 
            by = .(sex, period)]
cd4_md2 = dt[!is.na(cd4_after_ahdelig_result), .(value = round(median(cd4_after_ahdelig_result), 1),
                                                sex = 'Total'), by = period]
cd4_md = rbind(cd4_md1, cd4_md2)
cd4_md =  dcast(cd4_md, sex~period)

# export the table
write.csv(cd4_md, paste0(outDir, 'cd4_result_sex_median.csv'))
#-------------------

#-------------------
# mean cd4 by age
cd4_ma1 = dt[!is.na(cd4_after_ahdelig_result), .(value = round(mean(cd4_after_ahdelig_result, na.rm=T), 1)), 
             by = .(age_cat, sex, period)]
cd4_ma2 = dt[!is.na(cd4_after_ahdelig_result), .(value = round(mean(cd4_after_ahdelig_result, na.rm=T), 1),
                                                 sex ='Total'), by = .(age_cat, period)]

cd4_ma3 = dt[!is.na(cd4_after_ahdelig_result), .(value = round(mean(cd4_after_ahdelig_result, na.rm=T), 1),
                                                 age_cat = 'Total'), by = .(sex, period)]
cd4_ma4= dt[!is.na(cd4_after_ahdelig_result), .(value = round(mean(cd4_after_ahdelig_result, na.rm=T), 1),
                                                age_cat = 'Total', sex ='Total'), by = period]
cd4_ma = rbind(cd4_ma1, cd4_ma2, cd4_ma3, cd4_ma4)
cd4_ma =  dcast(cd4_ma, age_cat~period+sex)

# export the table
write.csv(cd4_ma, paste0(outDir, 'cd4_result_sex_age.csv'))
#-------------------

# ----------------------------------------------
# OUTCOME TABLES: WHO STAGING
# ----------------------------------------------

# check for missing data 
dt[is.na(whostage1_done)] # 105 missing entries
dt[!is.na(whostage1_done), length(unique(pid))]
dt[, table(whostage1_done), by = .(period, sex)]
dt[is.na(whostage1_done)][order(period)]

# ------------------------
# who stage by sex, period
who1 = dt[, .(pts = length(unique(pid))), by = .(sex, period, whostage1st)]
who2 = dt[, .(pts = length(unique(pid))), by = .(period, whostage1st)]
who2[ , sex:='Total']
who = rbind(who1, who2)
who = dcast(who, whostage1st~period+sex, value.var = 'pts')

# export the table
write.csv(who, paste0(outDir, 'who_stage_sex.csv'))
# ------------------------

# ------------------------
# who stage by age category, period
who_a1 = dt[, .(pts = length(unique(pid))), by = .(age_cat, period, whostage1st)]
who_a2 = dt[, .(pts = length(unique(pid))), by = .(period, whostage1st)]
who_a2[ , age_cat:='Total']
who_a = rbind(who_a1, who_a2)
who_a = who_a[!is.na(whostage1st)] # drop patients missing stage
who_a = dcast(who_a, age_cat~period+whostage1st, value.var = 'pts')

# rename the columns for ease of use
setnames(who_a, c('Age Category', '1', '3', '4', 
                  '1', '2', '3', '4')) # no data for criterion 2 in baseline
 
# export the table
write.csv(who_a, paste0(outDir, 'who_stage_age.csv'))
# ------------------------


# ----------------------------------------------
# OUTCOME TABLES: TB SCREENING
# ----------------------------------------------
# no missing data in tb symptom screening

# ------------------------
# screened and not screened for tb by period
tb_scrn = dt[, .(scrn = sum(tbsympscrn, na.rm=T), type = 'Screened'), by = period]
tb_scrn2 = dt[tbsympscrn==F, .(scrn = length(unique(pid)), type = 'Not screened'), by = period]
tb_scrn = rbind(tb_scrn, tb_scrn2)
tb_scrn3 = dt[is.na(tbsympscrn),.(scrn = length(unique(pid)), type = 'Missing'), by = period]
tb_scrn = rbind(tb_scrn, tb_scrn3)
tb_scrn = dcast(tb_scrn, type~period, value.var = 'scrn')
setnames(tb_scrn, c('Screened for TB', 'Pre-intervention', 'Post-intervention'))

# export the table
write.csv(tb_scrn, paste0(outDir, 'screened_for_tb.csv'))
# ------------------------

# ------------------------
# screened for tb by sex, age, period
tb_scrn_as = dt[, .(scrn = sum(tbsympscrn, na.rm=T)), by = .(sex, age_cat, period)]
tb_scrn_as = dcast(tb_scrn_as, age_cat~period+sex, value.var = 'scrn')

# export the table
write.csv(tb_scrn_as, paste0(outDir, 'screened_for_tb_sex_age.csv'))

# ------------------------

# ------------------------
# screened and not screened for tb by sex, age, period
setnames(tb_scrn_as,c('age_cat', 'b_Female_s', 'b_Male_s', 
                      'e_Female_s', 'e_Male_s'))

# table of not screened for tb
tb_ns_as = dt[tbsympscrn==FALSE, .(scrn = length(unique(pid))), by = .(sex, age_cat, period)]
tb_ns_as = dcast(tb_ns_as, age_cat~period+sex, value.var = 'scrn')
tb_all = merge(tb_scrn_as, tb_ns_as, by = 'age_cat', all = T)

# no men were "not screened" in the baseline in malawi
tb_all[ , b_Male:=0]

# rearrange variables in order of table - by sex, then screening status
tb_all = tb_all[ ,.(age_cat, b_Female_s, b_Female, b_Male_s, b_Male,
                    e_Female_s, e_Female, e_Male_s, e_Male)]

# export the table
write.csv(tb_all, paste0(outDir, 'screened_ns_for_tb_sex_age.csv'))

# ------------------------

# ------------------------
# calculate positive result

# number by screened by sex, age, period, include totals
tb_scrn_tot = dt[, .(scrn = sum(tbsympscrn, na.rm=T)), by = .(age_cat, period)]
tb_scrn_tot = dcast(tb_scrn_tot, age_cat~period, value.var = 'scrn')
screened = merge(tb_scrn_as, tb_scrn_tot, id.vars = 'age_cat')

# number positive by sex, age, period, include totals
tb_pos = dt[, .(scrn = sum(tbsympscrn_result, na.rm=T)), by = .(sex, age_cat, period)]
tb_pos = dcast(tb_pos, age_cat~period+sex, value.var = 'scrn')
tb_post = dt[, .(scrn = sum(tbsympscrn_result, na.rm=T)), by = .(age_cat, period)]
tb_post = dcast(tb_post, age_cat~period, value.var = 'scrn')
positives = merge(tb_pos, tb_post, id.vars = 'age_cat')

setnames(positives, c('age_cat', 'b_Femalepos', 'b_Male_pos', 'e_Female_pos',
                      'e_Male_pos', 'b_pos', 'e_pos'))

# merge screened and positive
tot = merge(screened, positives, id.vars = 'age_cat')

# rearrange the data set
tot = tot[ ,.(age_cat, b_Female_s, b_Femalepos, b_Male_s, b_Male_pos,
              b, b_pos, e_Female_s, e_Female_pos, e_Male_s, e_Male_pos,
              e, e_pos)]

# export the table
write.csv(tot, paste0(outDir, 'screened_for_tb_tbpos_sex_age.csv'))


# ------------------------







# ----------------------------------------------
# OUTCOME TABLES: TPT
# ----------------------------------------------

# ------------------------
# examine the tb preventive therapy variables
# some patients received tb preventive therapy even if no screening outcome recorded
dt[ , table(tptstart)]
dt[ , table(tptalready)]
dt[tptstart==T & tptalready==T] # no patients were both started and already on
# ------------------------

# ------------------------
# create a table of anyone started on or already on TB preventive therapy
tp1 = dt[ ,.(value = sum(tptstart)), by = .(sex, period)]
tp2 = dt[ ,.(value = sum(tptstart)), by = .(period)]
tp2[ , sex:='Total']
tp = rbind(tp1, tp2)
tp[ , variable:='Started TB preventive therapy']

tp3 = dt[ ,.(value = sum(tptalready)), by = .(sex, period)]
tp4 = dt[ ,.(value = sum(tptalready)), by = .(period)]
tp4[ , sex:='Total']
tp5 = rbind(tp3, tp4)
tp5[ , variable:='Already on TB preventive therapy']
tp = rbind(tp, tp5)

# create a full data set and output
tp = dcast(tp, variable~period+sex)

# export the table
write.csv(tp, paste0(outDir, 'on_tpt_sex.csv'))
# ------------------------

# ------------------------
# add a table for no tb preventive therapy 

no_tp = dt[!(tptstart==T | tptalready==T), .(value = length(unique(pid))), by = .(sex, period)]
no_tp2 = dt[!(tptstart==T | tptalready==T), .(value = length(unique(pid))), by = .(period)]
no_tp2[ , sex:='Total']
no_tp = rbind(no_tp, no_tp2)
no_tp[ , variable:='No TB preventive therapy']
no_tp = dcast(no_tp, variable~period+sex)

# export the table
write.csv(no_tp, paste0(outDir, 'no_tpt_sex.csv'))
# ------------------------

# ------------------------
# time on tpt

dt[(tptstart==T | tptalready==T), .(tbsympscrn,  tptstart,
                                    tptalready, tptstart_dt,  tptalready_dt, 
                                    tptcplt, tptcplt_dt, tptcplt_impute), 
   by = .(pid, period, dob, age, age_cat, sex)]

tpt_duration 



# ------------------------


# ----------------------------------------------
# OUTCOME TABLES: ART & VIRAL LOAD
# ----------------------------------------------

# data quality checks - art and viral load variables
dt[ ,is.na(everart)]
dt[everart==F] # if ever on art is false, all subsuequent variables are missing

# ------------------------
# ever on art by sex, period
art1 = dt[, .(pts = length(unique(pid))), by = .(sex, period, everart)]
art2 = dt[, .(pts = length(unique(pid))), by = .(period, everart)]
art2[ , sex:='Total']
art = rbind(art1, art2)
art = dcast(art, sex~period+everart, value.var = 'pts')

# change the order
art = art[ ,.(sex, b_TRUE, b_FALSE, e_TRUE, e_FALSE)]

# export the table
write.csv(art, paste0(outDir, 'ever_on_art_sex.csv'))
# ------------------------

# ------------------------
# on art at six months by sex, period
art61 = dt[, .(pts = length(unique(pid))), by = .(sex, period, art6m)]
art62 = dt[, .(pts = length(unique(pid))), by = .(period, art6m)]
art62[ , sex:='Total']
art6 = rbind(art61, art62)
art6 = dcast(art6, sex~period+art6m, value.var = 'pts')
art6[ ,c('b_NA', 'e_NA'):=NULL]

# change the order
art6 = art6[ ,.(sex, b_TRUE, b_FALSE, e_TRUE, e_FALSE)]

# export the table
write.csv(art6, paste0(outDir, 'on_art_6m_sex.csv'))
# ------------------------

# ------------------------
# ever on art compared to on art at six months


# ------------------------

# ----------------------------------------------
# OUTCOME TABLES: TB TESTING
# ----------------------------------------------

# ------------------------
# create a table of genexpert testing by sex, age 
# no missing data for genexpert testing; no results data in emr

# there are almost no values for 
dt[, sum(gxtest)]

gx1 = dt[ ,.(value = sum(gxtest)), by = .(age_cat, sex, period)]
gx2 = dt[ ,.(value = sum(gxtest)), by = .(age_cat, period)]
gx2[ , sex:='Total']
gx = rbind(gx1, gx2)
gx = dcast(gx, age_cat~period+sex)

# export the table
write.csv(gx, paste0(outDir, 'genexpert_tested_age_sex.csv'))
# ------------------------


# ------------------------
# create a table of sputum smear microscopy testing and positive ss test by sex
ss1 = dt[ ,.(value = sum(sstest)), by = .(sex, period)]
ss2 = dt[ ,.(value = sum(sstest)), by = .(period)]
ss2[ , sex:='Total']
ss = rbind(ss1, ss2)
ss[ , variable:='Tested']

ss3 = dt[sstest==T,.(value = sum(sspos)), by = .(sex, period)]
ss4 = dt[sstest==T,.(value = sum(sspos)), by = .(period)]
ss4[ , sex:='Total']
ss5 = rbind(ss3, ss4)
ss5[ , variable:='Positive']
ss = rbind(ss, ss5)

# create a full data set and output
ss = dcast(ss, sex~period+variable)

# add the percentage and reorder
ss[ , b_PP:=round(100*(b_Positive/b_Tested), 1)]
ss[ , e_PP:=round(100*(e_Positive/e_Tested), 1)]

ss = ss[ ,.(b_Positive, b_Tested, b_PP, e_Positive, e_Tested, e_PP), by = sex]

# export the table
write.csv(ss, paste0(outDir, 'ss_tested_sex.csv'))
# ------------------------

# ------------------------
# create a table of sputum smear microscopy testing and positive ss test by age
ssa1 = dt[ ,.(value = sum(sstest)), by = .(age_cat, period)]
ssa2 = dt[ ,.(value = sum(sstest)), by = .(period)]
ssa2[ , age_cat:='Total']
ssa = rbind(ssa1, ssa2)
ssa[ , variable:='Tested']

ssa3 = dt[sstest==T,.(value = sum(sspos)), by = .(age_cat, period)]
ssa4 = dt[sstest==T,.(value = sum(sspos)), by = .(period)]
ssa4[ , age_cat:='Total']
ssa5 = rbind(ssa3, ssa4)
ssa5[ , variable:='Positive']
ssa = rbind(ssa, ssa5)

# create a full data set and output
ssa = dcast(ssa, age_cat~period+variable)

# add the percentage and reorder
ssa[ , b_PP:=round(100*(b_Positive/b_Tested), 1)]
ssa[ , e_PP:=round(100*(e_Positive/e_Tested), 1)]

ssa = ssa[ ,.(b_Positive, b_Tested, b_PP, e_Positive, e_Tested, e_PP), by = age_cat]

# export the table
write.csv(ssa, paste0(outDir, 'ss_tested_age.csv'))
# ------------------------

