# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/25/2021
# Tanzania Baseline Cohort Data
# Create and export tables for analysis
# Sources the data prepped in ahd_tz_prep1.R
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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/ahd/tanzania/'
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
dt_ss = dt[ ,.(value = length(unique(pid))), by =.(sex, dhisname, period)]
dt_ss = dcast(dt_ss, dhisname~period+sex)
dt_ss[ , b_Total:=(b_Female+b_Male)]
dt_ss[ , e_Total:=(e_Female+e_Male)]
dt_ss = dt_ss[ ,.(Site = dhisname,b_Female, b_Male, b_Total, e_Female, e_Male, e_Total)]

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
# notes from gretchen - the know status variable is always false
# the tested and tested positive variables are always true
# auto filled so not useful 
# the date range of the hiv tests is useful

# one patient has a study eligibility date two years before testing positive
# exclude this date as it is likely wrong or indicates a confirmatory test
dt[pid=='07-01-0119-000593', dtpos:=NA]

# calculate the extent of missingness in the dates of positive hiv tests
# dt[!is.na(dtpos), length(unique(pid)),by = period]
# dt[ , range(dtpos, na.rm=T)]

# length of time between positive test and study eligibility date 
dt[ , time2ahd_days:=(ahd_dt-dtpos)]
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
# basic descriptive statistics for cd4
# no outcome recorded is listed as false
table(dt$cd4done_after_ahdelig)

#-------------------
# number receiving cd4 after ahd by sex, period
cd4_s1 = dt[cd4done_after_ahdelig==T, .(value = sum(cd4done_after_ahdelig, na.rm=T)), 
            by = .(sex, period)]
cd4_s2 = dt[cd4done_after_ahdelig==F, .(value = length(unique(pid))), by = .(sex, period)]
cd4_s1[ , type:='CD4 Tested']
cd4_s2[ , type:='No Outcome Recorded']
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
write.csv(cd4_m, paste0(outDir, 'cd4_result_sex.csv'))
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

#-------------------
#add a histogram 

hist(dt[period=='b']$cd4_after_ahdelig_result, xlab = 'CD4 cell count', main = NULL)
hist(dt[period=='e']$cd4_after_ahdelig_result, xlab = 'CD4 cell count', main = NULL)

# ----------------------------------------------
# OUTCOME TABLES: WHO STAGING
# ----------------------------------------------

# counts of missing data 
# values for stage completed are eithe 1 or missing
# dt[is.na(whostage1_done)] # 9 missing entries
# dt[, table(whostage1_done), by = .(period, sex)]
# dt[is.na(whostage1_done)][order(period)]

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
setnames(who_a, c('Age Category', '1', '2', '3', '4',
                  '1', '2', '3', '4'))

# export the table
write.csv(who_a, paste0(outDir, 'who_stage_age.csv'))
# ------------------------


# ----------------------------------------------
# OUTCOME TABLES: TB SCREENING
# ----------------------------------------------
# no missing data in tb symptom screening

# ------------------------
# screened and not screened for tb by period
tb_scrn = dt[, .(scrn = sum(tbsympscrn), type = 'Screened'), by = period]
tb_scrn2 = dt[tbsympscrn==0, .(scrn = length(unique(pid)), type = 'Not screened'), by = period]
tb_scrn = rbind(tb_scrn, tb_scrn2)
tb_scrn = dcast(tb_scrn, type~period, value.var = 'scrn')
setnames(tb_scrn, c('Screened for TB', 'Pre-intervention', 'Post-intervention'))

# export the table
write.csv(tb_scrn, paste0(outDir, 'screened_for_tb.csv'))
# ------------------------

# ------------------------
# screened for tb by sex, age, period
tb_scrn_as = dt[, .(scrn = sum(tbsympscrn)), by = .(sex, age_cat, period)]
tb_scrn_as = dcast(tb_scrn_as, age_cat~period+sex, value.var = 'scrn')

# export the table
write.csv(tb_scrn_as, paste0(outDir, 'screened_for_tb_sex_age.csv'))

# ------------------------

# ------------------------
# screened and not screened for tb by sex, age, period
setnames(tb_scrn_as,c('age_cat', 'b_Female_s', 'b_Male_s', 
           'e_Female_s', 'e_Male_s'))

# table of not screened for tb
tb_ns_as = dt[tbsympscrn==0, .(scrn = length(unique(pid))), by = .(sex, age_cat, period)]
tb_ns_as = dcast(tb_ns_as, age_cat~period+sex, value.var = 'scrn')
tb_all = merge(tb_scrn_as, tb_ns_as, by = 'age_cat', all = T)

# rearrange variables in order of table - by sex, then screening status
tb_all = tb_all[ ,.(age_cat, b_Female_s, b_Female, b_Male_s, b_Male,
            e_Female_s, e_Female, e_Male_s, e_Male)]

# export the table
write.csv(tb_all, paste0(outDir, 'screened_ns_for_tb_sex_age.csv'))

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

# ----------------------------------------------
# OUTCOME TABLES: VIRAL LOAD TESTING & SUPPRESSION
# ----------------------------------------------

# ------------------------
# received a viral load test within six months of starting ART
dt[, length(unique(pid)), by = .(period, hvl6m)] # type in - 6 values
# ------------------------

# ------------------------
# received a viral load test within six months of starting ART, age, sex
hvt = dt[, sum(hvl6m, na.rm=T), by = .(period, sex)]
hvt2 = dt[, sum(hvl6m, na.rm=T), by = .(period, age_cat)]
hvt2[ , sex:='Total']
hvt = rbind(hvt, hvt2)
hvt = dcast(hvt, age_cat~period+sex)

# export the table
write.csv(hvt, paste0(outDir, 'vl_tested_age_sex.csv'))
# ------------------------

# ------------------------
# viral suppression table 
vl = dt[hvl6m==T & !is.na(hvl6mresult),
   .(pid, hvl6m, hvl6mresult, sex, period)]

# create a viral suppression variable
vl[hvl6mresult <= 1000 , sup:=TRUE]
vl[is.na(sup), sup:=FALSE]

# # create the table
# vs = vl[, .(value = sum(sup)), by = .(period, sex)]
# vs[ ,var:='sup']
# vs = dcast(vs, sex~period+var, value.var = 'value')
# vt = hvt[ , .(sex, b_Total, e_Total)]
# vs = merge(vs, vt, by = 'age_cat')
# 
# # rename the variables and reorder 
# vs = vs[ ,.(age_cat, b_tested = b_Total, b_sup,
#        e_tested = e_Total, e_sup)]
# 
# # export the table
# write.csv(vs, paste0(outDir, 'vl_suppression_age_sex.csv'))
# ------------------------


vt = dt[!is.na(hvl6m),.(pid, period, site, sex, age_cat, hvl6m, hvl6mresult)]
vt[hvl6m==T & hvl6mresult <= 1000 , sup:=TRUE]
vt[hvl6m==T & 1001 <= hvl6mresult, sup:=FALSE]

vs_model = glm(formula = sup~period+sex+age_cat+site, family = "binomial", data = vt)
summary(vs_model)

vs_model2 = glm(formula = sup~period+sex+age_cat, family = "binomial", data = vt)
summary(vs_model2)

vl_model = glm(formula = hvl6m~period+sex+age_cat, family = "binomial", data = vt)
summary(vl_model)



# print the output
stargazer(vl_model, vs_model2, 
          title = 'Viral Load Testing & Viral Suppression', 
          align=T, type = 'text', no.space = TRUE, omit.stat = c("LL","ser","f"),
          dep.var.labels = c("Received a VL Test", 'Virally Suppressed'),
          covariate.labels = c("Endline Cohort",
                               "Female", '5-9',
                               '10-14', '15-19', '20-24', '25-29', '30-34',
                               '35-39', '40-44', '45-49', '50+'),
          out = paste0(outDir, 'viral_load_regressions_tz.txt'),
          single.row = TRUE)





















# run ttests

supt = vl[ ,.(pid, period, sup)]
t.test(supt[period=='b']$sup, supt[period=='e']$sup,
       var.eqal = FALSE)



hvl = dt[!is.na(hvl6m), .(pid, hvl6m, period)]

t.test(hvl[period=='b']$hvl6m, hvl[period=='e']$hvl6m,
       var.eqal = FALSE)









