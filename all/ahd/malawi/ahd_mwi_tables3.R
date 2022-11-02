# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/1/22
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
# no patients who were not screened for tb were started on TPT
dt[ , table(tptstart)]
dt[ , table(tptcplt)]

dt[tbsympscrn==F, unique(tptstart)]
dt[is.na(tbsympscrn), unique(tptstart)]
dt[tbsympscrn_result==F, unique(tptstart)]
dt[tptstart==T, unique(tbsympscrn_result)]
# ------------------------

# ------------------------
# create a table of anyone started on TB preventive therapy
tp1 = dt[ ,.(value = sum(tptstart, na.rm=T)), by = .(sex, period)]
tp2 = dt[ ,.(value = sum(tptstart, na.rm=T)), by = .(period)]
tp2[ , sex:='Total']
tp = rbind(tp1, tp2)
tp[ , variable:='Started TB preventive therapy']

tp3 = dt[ ,.(value = sum(tptcplt, na.rm=T)), by = .(sex, period)]
tp4 = dt[ ,.(value = sum(tptcplt, na.rm=T)), by = .(period)]
tp4[ , sex:='Total']
tp5 = rbind(tp3, tp4)
tp5[ , variable:='Completed TB preventive therapy']
tp = rbind(tp, tp5)

# create a full data set and output
tp = dcast(tp, variable~period+sex)

# export the table
write.csv(tp, paste0(outDir, 'on_completed_tpt_sex.csv'))
# ------------------------

# ------------------------
# create a table showing % started on TPT of those screen TB-negative
tscreen1 = dt[tbsympscrn_result==F, .(value=length(unique(pid))), by = .(sex, period)]
tscreen2 = dt[tbsympscrn_result==F, .(value=length(unique(pid))), by = .(period)]
tscreen2[ , sex:='Total']
tscreen = rbind(tscreen1, tscreen2)
tscreen[ , variable:='Screened TB-negative']
tscreen = dcast(tscreen, variable~period+sex)

# bind the table to the tpt data and drop out completed
tscreen = rbind(tp, tscreen)
tscreen = tscreen[variable!='Completed TB preventive therapy']

# export the table
write.csv(tscreen, paste0(outDir, 'on_tpt_screened_neg_sex.csv'))

# ------------------------

# ------------------------
# started tpt and completed tpt by age, sex, cohort

# create a table of anyone started on TB preventive therapy
ta1 = dt[ ,.(value = sum(tptstart, na.rm=T)), by = .(age_cat, sex, period)]
ta2 = dt[ ,.(value = sum(tptstart, na.rm=T)), by = .(age_cat, period)]
ta2[ , sex:='Total']
ta = rbind(ta1, ta2)
ta[ , variable:='Started TPT']

# create a table of anyone who completed TB preventive therapy
tc1 = dt[ ,.(value = sum(tptcplt, na.rm=T)), by = .(age_cat, sex, period)]
tc2 = dt[ ,.(value = sum(tptcplt, na.rm=T)), by = .(age_cat, period)]
tc2[ , sex:='Total']
tc = rbind(tc1, tc2)
tc[ , variable:='Completed TPT']

# bind them together and reshape
tca = rbind(ta, tc)
tca = dcast(tca, age_cat~period+sex+variable)

# export the table
write.csv(tca, paste0(outDir, 'on_completed_tpt_age_sex.csv'))
# ------------------------

# ----------------------------------------------
# OUTCOME TABLES: TB TESTING
# ----------------------------------------------

# ------------------------
# create a table of sputum smear microscopy testing by sex
ss1 = dt[ ,.(value = sum(sstest, na.rm=T)), by = .(sex, period)]
ss2 = dt[ ,.(value = sum(sstest, na.rm=T)), by = .(period)]
ss2[ , sex:='Total']
ss = rbind(ss1, ss2)
ss[ , variable:='Tested']

ss3 = dt[sstest==F,.(value = length(unique(pid))), by = .(sex, period)]
ss4 = dt[sstest==F,.(value = length(unique(pid))), by = .(period)]
ss4[ , sex:='Total']
ss5 = rbind(ss3, ss4)
ss5[ , variable:='NotTested']
ss = rbind(ss, ss5)

ss6 = dt[is.na(sstest),.(value = length(unique(pid))), by = .(sex, period)]
ss7 = dt[is.na(sstest),.(value = length(unique(pid))), by = .(period)]
ss7[ , sex:='Total']
ss7 = rbind(ss6, ss7)
ss7[ , variable:='Missing']
ss = rbind(ss, ss7)

# create a full data set and output
ss = dcast(ss, sex~period+variable)

# rearrange the columns
ss = ss[,.(b_Tested, b_NotTested, b_Missing, e_Tested, e_NotTested, e_Missing), by = sex]

# export the table
write.csv(ss, paste0(outDir, 'ss_tested_sex.csv'))
# ------------------------

# ------------------------
# create a table of genexpert testing by sex
g1 = dt[ ,.(value = sum(gxtest, na.rm=T)), by = .(sex, period)]
g2 = dt[ ,.(value = sum(gxtest, na.rm=T)), by = .(period)]
g2[ , sex:='Total']
gg = rbind(g1, g2)
gg[ , variable:='Tested']

g3 = dt[gxtest==F,.(value = length(unique(pid))), by = .(sex, period)]
g4 = dt[gxtest==F,.(value = length(unique(pid))), by = .(period)]
g4[ , sex:='Total']
g5 = rbind(g3, g4)
g5[ , variable:='NotTested']
gg = rbind(gg, g5)

g6 = dt[is.na(gxtest),.(value = length(unique(pid))), by = .(sex, period)]
g7 = dt[is.na(gxtest),.(value = length(unique(pid))), by = .(period)]
g7[ , sex:='Total']
g7 = rbind(g6, g7)
g7[ , variable:='Missing']
gg = rbind(gg, g7)

# create a full data set and output
gg = dcast(gg, sex~period+variable)

# rearrange the columns
gg = gg[,.(b_Tested, b_NotTested, b_Missing, e_Tested, e_NotTested, e_Missing), by = sex]

# export the table
write.csv(gg, paste0(outDir, 'gx_tested_sex.csv'))
# ------------------------

# ------------------------
# create a table of genexpert testing by sex, age 

# there are almost no values for genexpert testing - only 14 
dt[, sum(gxtest, na.rm=T)]

gx1 = dt[ ,.(value = sum(gxtest, na.rm=T)), by = .(age_cat, sex, period)]
gx2 = dt[ ,.(value = sum(gxtest, na.rm=T)), by = .(age_cat, period)]
gx2[ , sex:='Total']
gx = rbind(gx1, gx2)
gx = dcast(gx, age_cat~period+sex)

# export the table
write.csv(gx, paste0(outDir, 'genexpert_tested_age_sex.csv'))
# ------------------------

# ------------------------
# create a table of tb lam testing by sex, age 

# there are almost no values for genexpert testing - only 14 
dt[, sum(lamtest, na.rm=T)]

lt1 = dt[ ,.(value = sum(lamtest, na.rm=T)), by = .(age_cat, sex, period)]
lt2 = dt[ ,.(value = sum(lamtest, na.rm=T)), by = .(age_cat, period)]
lt2[ , sex:='Total']
lt = rbind(lt1, lt2)
lt = dcast(lt, age_cat~period+sex)

# export the table
write.csv(lt, paste0(outDir, 'lam_tested_age_sex.csv'))
# ------------------------

# ------------------------
# create a table of tb lam tested and results
# just create a table for endline, baseline has only one test

lt3 = dt[period=='e',.(value = sum(lamresult, na.rm=T)), by = .(age_cat, sex)]
lt4 = dt[period=='e',.(value = sum(lamresult, na.rm=T)), by = .(age_cat)]
lt4[ , sex:='Total']
ltp = rbind(lt3, lt4)
ltp = dcast(ltp, age_cat~sex)

# merge in screened and organize columns
lts = lt[,.(age_cat, e_Female, e_Male, e_Total)]
lt_res = merge(ltp, lts, by = 'age_cat')
lt_res = lt_res[ ,.(age_cat, Female, e_Female, Male, e_Male, Total, e_Total)]

# export the table
write.csv(lt_res, paste0(outDir, 'lam_tested_result_age_sex.csv'))

# ------------------------

# ----------------------------------------------
# OUTCOME TABLES: TB TREATMENT
# ----------------------------------------------

# ------------------------
# check how many of the patients who started TB Tx had a positive TB test
dt[, table(tbtx_start, period)]

# of the 49 patients who started TB Tx, 37 have a documented positive test
dt[tbtx_start==T, .(gxresult, ssresult, lamresult)]
dt[tbtx_start==T & gxresult==T & lamresult==T] # no patients were + for both
dt[tbtx_start==T,.(sum(lamresult, na.rm=T)+sum(gxresult, na.rm=T))]

# did anyone have a positive result but was not on TB Tx?
# there are 48 positive results in the data set
dt[(gxresult==T | lamresult==T) & tbtx_start==F  ]

# summary sex table is calculated in excel using sex, age tables
# ------------------------

# ------------------------
# create a table of started tb treatment by sex, age 
tx1 = dt[ ,.(value = sum(tbtx_start, na.rm=T)), by = .(age_cat, sex, period)]
tx2 = dt[ ,.(value = sum(tbtx_start, na.rm=T)), by = .(age_cat, period)]
tx2[ , sex:='Total']
tx = rbind(tx1, tx2)
tx = dcast(tx, age_cat~period+sex)

# export the table
write.csv(tx, paste0(outDir, 'started_tb_tx_age_sex.csv'))
# ------------------------

# ------------------------
# create a table of completed tb treatment by sex, age 
tx3 = dt[ ,.(value = sum(tb_tx_cplt, na.rm=T)), by = .(age_cat, sex, period)]
tx4 = dt[ ,.(value = sum(tb_tx_cplt, na.rm=T)), by = .(age_cat, period)]
tx4[ , sex:='Total']
tx5 = rbind(tx3, tx4)
tx5 = dcast(tx5, age_cat~period+sex)

# export the table
write.csv(tx5, paste0(outDir, 'completed_tb_tx_age_sex.csv'))
# ------------------------

# ----------------------------------------------
# OUTCOME TABLES: ART & VIRAL LOAD
# ----------------------------------------------

# data quality checks - art and viral load variables
dt[is.na(everart)]
dt[!is.na(everart)]

dt[everart==T, unique(restarted_art)]
dt[is.na(everart) & !is.na(restarted_art)]
dt[is.na(everart) & is.na(restarted_art) & !is.na(art6m)]

dt[everart==F] # if ever on art is false, all subsequent variables are missing

# ------------------------
# started art by sex, period
art1 = dt[everart==T, .(pts = length(unique(pid))), by = .(sex, period)]
art2 = dt[everart==T, .(pts = length(unique(pid))), by = period]
art2[ , sex:='Total']
art = rbind(art1, art2)
art[ , variable:='aStartedART']

# restarted art by sex, period
rart1 = dt[restarted_art==T, .(pts = length(unique(pid))), by = .(sex, period)]
rart2 = dt[restarted_art==T, .(pts = length(unique(pid))), by = .(period)]
rart2[ , sex:='Total']
rart = rbind(rart1, rart2)
rart[ , variable:='bRestartedART']

# missing both 
miss1 = dt[is.na(everart) & is.na(restarted_art), .(pts = length(unique(pid))), by = .(sex, period)]
miss2 = dt[is.na(everart) & is.na(restarted_art), .(pts = length(unique(pid))), by = .(period)]
miss2[ , sex:='Total']
miss = rbind(miss1, miss2)
miss[ , variable:='cMissingART']

# bind them together and reshape
art = rbind(art, rart)
art = rbind(art, miss)
art = dcast(art, variable~period+sex, value.var = 'pts')

# export the table
write.csv(art, paste0(outDir, 'started_restarted_art_sex.csv'))
# ------------------------

# ------------------------
# on art at six months by sex, period
art61 = dt[, .(pts = length(unique(pid))), by = .(sex, period, art6m)]
art62 = dt[, .(pts = length(unique(pid))), by = .(period, art6m)]
art62[ , sex:='Total']
art6 = rbind(art61, art62)
art6 = dcast(art6, sex~period+art6m, value.var = 'pts')

# change the order
art6 = art6[ ,.(sex, b_TRUE, b_FALSE, b_NA, e_TRUE, e_FALSE, e_NA)]

# export the table
write.csv(art6, paste0(outDir, 'on_art_6m_sex.csv'))
# ------------------------

# ------------------------
# received a viral load test, did not receive, missing 
avl1 = dt[ ,.(value = sum(ahd_vl, na.rm=T), variable = 'aReceivedVL'), by = .(sex, period)]
avl2 = dt[ahd_vl==F,.(value = length(unique(pid)), variable = 'bNoVL'), by = .(sex, period)]
avl3 = dt[is.na(ahd_vl),.(value = length(unique(pid)), variable = 'cMissing'), by = .(sex, period)]
avl = rbind(avl1, avl2)
avl = rbind(avl, avl3)

# totals rows (not disaggregated by sex)
avl4 = dt[ ,.(value = sum(ahd_vl, na.rm=T), variable = 'aReceivedVL', sex = 'Total'), by = period]
avl5 = dt[ahd_vl==F,.(value = length(unique(pid)), variable = 'bNoVL', sex = 'Total'), by = period]
avl6 = dt[is.na(ahd_vl),.(value = length(unique(pid)), variable = 'cMissing', sex = 'Total'), by = period]
avl = rbind(avl, avl4)
avl = rbind(avl, avl5)
avl = rbind(avl, avl6)

# reshape 
avl = dcast(avl, variable~period+sex)

# export the table
write.csv(avl, paste0(outDir, 'received_no_vl_sex.csv'))
# ------------------------

# ------------------------
# received a viral load test by age, sex

vl1 = dt[ ,.(value = sum(ahd_vl, na.rm=T)), by = .(age_cat, sex, period)]
vl2 = dt[ ,.(value = sum(ahd_vl, na.rm=T)), by = .(age_cat, period)]
vl2[ , sex:='Total']
vl = rbind(vl1, vl2)
vl = dcast(vl, age_cat~period+sex)

# export the table
write.csv(vl, paste0(outDir, 'received_vl_age_sex.csv'))
# ------------------------

# ------------------------
# virally suppressed by age, sex

svl1 = dt[ ,.(value = sum(suppressed, na.rm=T)), by = .(age_cat, sex, period)]
svl2 = dt[ ,.(value = sum(suppressed, na.rm=T)), by = .(age_cat, period)]
svl2[ , sex:='Total']
svl = rbind(svl1, svl2)
svl = dcast(svl, age_cat~period+sex)

# export the table
write.csv(svl, paste0(outDir, 'vl_suppressed_age_sex.csv'))
# ------------------------

# ------------------------
# create a denominator table of documented vl results 
# 399 patients had a documented viral load

# create the table of documented vl results
sp1 = dt[!is.na(suppressed) ,.(value = length(unique(pid))), by = .(age_cat, sex, period)]
sp2 = dt[!is.na(suppressed) ,.(value = length(unique(pid)), sex = 'Total'), by = .(age_cat, period)]
sp = rbind(sp1, sp2)
sp = dcast(sp, age_cat~period+sex)
setnames(sp, c('age_cat', 'b_Femaled', 'b_Maled', 'b_Totald',
               'e_Femaled', 'e_Maled', 'e_Totald'))

# merge the documented vl results with vl suppression 
sp = merge(svl, sp, by = 'age_cat')

# calculate the percentages
sp[ , b_Female_perc:=round(100*(b_Female/b_Femaled), 1)]
sp[ , e_Female_perc:=round(100*(e_Female/e_Femaled), 1)]

sp[ , b_Male_perc:=round(100*(b_Male/b_Maled), 1)]
sp[ , e_Male_perc:=round(100*(e_Male/e_Maled), 1)]

sp[ , b_Total_perc:=round(100*(b_Total/b_Totald), 1)]
sp[ , e_Total_perc:=round(100*(e_Total/e_Totald), 1)]

# rearrange the variables in an interpretable order
sp = sp[,.(b_Female, b_Femaled, b_Female_perc, b_Male, b_Maled, b_Male_perc,
           b_Total, b_Totald, b_Total_perc,
           e_Female, e_Femaled, e_Female_perc, e_Male, e_Maled, e_Male_perc,
           e_Total, e_Totald, e_Total_perc)]

# export the table
write.csv(sp, paste0(outDir, 'vl_suppression_rate_age_sex.csv'))
# ------------------------

# ------------------------





# ------------------------

# ----------------------------------------------
# OUTCOME TABLES: MENINGITIS CASCADE
# ----------------------------------------------

c("screenedfor_crypto", "crag_dt", "crag_result_dt", "crag_result", 
  
"lumbar_referred", "lumbarreferred_dt", "lumbar_done", "lumbar_done_dt",           

"csf_cragperformed", "csf_cragperformed_dt", "csfcragresultsreturned_dt",     

"csf_result", "crypto_regimen", "crypto_regimen_start_dt",    
"complete_cryptoindcuti2weeks", "complete_cryptoindcuti2weeks_dt")


dt[ , table(screenedfor_crypto, period)]

dt[ , table(screenedfor_crypto, period)]


# ------------------------
# screened for crypto by age, sex

cry1 = dt[ ,.(value = sum(screenedfor_crypto, na.rm=T)), by = .(age_cat, sex, period)]
cry2 = dt[ ,.(value = sum(screenedfor_crypto, na.rm=T)), by = .(age_cat, period)]
cry2[ , sex:='Total']
cry = rbind(cry1, cry2)
cry = dcast(cry, age_cat~period+sex)

# export the table
write.csv(cry, paste0(outDir, 'crypto_screened_age_sex.csv'))
# ------------------------

# ------------------------
# crypto screening (crag) result by age, sex

cr1 = dt[ ,.(value = sum(crag_result, na.rm=T)), by = .(age_cat, sex, period)]
cr2 = dt[ ,.(value = sum(crag_result, na.rm=T)), by = .(age_cat, period)]
cr2[ , sex:='Total']
cr = rbind(cr1, cr2)
cr = dcast(cr, age_cat~period+sex)

# export the table
write.csv(cr, paste0(outDir, 'crag_result_age_sex.csv'))
# ------------------------

# ------------------------
# csf crag performed and csf result

# csf crag performed
csf1 = dt[ ,.(value = sum(csf_cragperformed, na.rm=T)), by = .(age_cat, sex)]
csf2 = dt[ ,.(value = sum(csf_cragperformed, na.rm=T), sex = 'Total'), by = .(age_cat)]
csf = rbind(csf1, csf2)
csf = dcast(csf, age_cat~sex)

# csf crag positive result - no missing data 
csf3 = dt[ ,.(value = sum(csf_result, na.rm=T)), by = .(age_cat, sex)]
csf4 = dt[ ,.(value = sum(csf_result, na.rm=T), sex = 'Total'), by = .(age_cat)]
csf5 = rbind(csf3, csf4)
csf5 = dcast(csf5, age_cat~sex)
setnames(csf5, c('age_cat', 'Female_r', 'Male_r', 'Total_r'))

# merge and arrange
csf = merge(csf, csf5, by = 'age_cat')
csf = csf[ ,.(age_cat, Female, Female_r, Male, Male_r, Total, Total_r)]

# export the table
write.csv(csf, paste0(outDir, 'csf_crag_results_age_sex.csv'))
# ------------------------

# ----------------------------------------------









