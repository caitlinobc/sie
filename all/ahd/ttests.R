# t tests
# 
# exp = dt[ ,.(pid, whostage1_done, cd4done_after_ahdelig, tbsympscrn)]
# exp[is.na(cd4done_after_ahdelig), cd4done_after_ahdelig:=FALSE]
# exp[ , country:='TZ']
# exp[, period:=period]
# 
# saveRDS(exp, paste0(dir, period, '_ttest.RDS'))

dt1 = readRDS(paste0(dir, 'baseline_ttest.RDS'))
dt2 = readRDS(paste0(dir, 'endline_ttest.RDS'))

dt = rbind(dt1, dt2)
dt[is.na(whostage1_done), whostage1_done:=FALSE]

t.test(dt[period=='baseline']$whostage1_done, dt[period=='endline']$whostage1_done)
t.test(dt[period=='baseline']$cd4done_after_ahdelig, dt[period=='endline']$cd4done_after_ahdelig)
t.test(dt[period=='baseline']$tbsympscrn, dt[period=='endline']$tbsympscrn)


# test data
x = seq(1:20)
y = seq(40, 60)

#--------------------------
# MALAWI

dt = dt[ ,.(id, pre_post, who_staged, cd4_done)]
tbscrn = data.table(tbscrn = df$TB_screening.screenedforTB)

dt = cbind(dt, tbscrn = tbscrn$tbscrn)

dt[who_staged==1, who_staged:=TRUE]
dt[who_staged==2, who_staged:=FALSE]
dt[who_staged==3, who_staged:=NA]
dt[who_staged=='n/a', who_staged:=NA]
dt[ ,who_staged:=as.logical(who_staged)]

dt[cd4_done==1, cd4_done:=TRUE]
dt[cd4_done==2, cd4_done:=FALSE]
dt[cd4_done==3, cd4_done:=NA]
dt[ ,cd4_done:=as.logical(cd4_done)]


dt[tbscrn==1, tbscrn:=TRUE]
dt[tbscrn==2, tbscrn:=FALSE]
dt[tbscrn==3, tbscrn:=NA]
dt[ ,tbscrn:=as.logical(tbscrn)]

t.test(dt[pre_post=='Baseline']$who_staged, dt[pre_post=='Endline']$who_staged)
t.test(dt[pre_post=='Baseline']$cd4_done, dt[pre_post=='Endline']$cd4_done)
t.test(dt[pre_post=='Baseline']$tbscrn, dt[pre_post=='Endline']$tbscrn)






