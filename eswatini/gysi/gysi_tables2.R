# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/23/21
# Tables sources by gysi_prep1.R
# ----------------------------------------------

#-----------------------------
# Table 1 - total for key variables by sex, age

# shape wide for calculations
tab1 = dcast(df, age+sex+agyw~variable)
tab1[ , hts_yield:=round((hts_tst_pos/hts_tst)*100, 1)]
tab1[ , vls_ratio:=round((tx_pvls_n/tx_pvls_d)*100, 1)]

# change the order of variables
tab1 = tab1[, .(age, sex, prep_curr, prep_new, hts_tst,    
  hts_tst_pos, hts_yield, tx_curr, tx_new, 
  tx_pvls_n, tx_pvls_d,  vls_ratio)]

#-----------------------------
# reshape for variable, sex
tab1 = melt(tab1, id.vars = c("age", "sex"))

#-----------------------------
# create a totals row and merge
tab1_total = tab1[ ,.(age = 'Total', value = sum(value, na.rm = T)),
                   by = .(variable, sex)]
tab1_total = tab1_total[variable!='hts_yield' & variable!='vls_ratio']
tab1 = rbind(tab1, tab1_total)

#-----------------------------
# reshape the table into a key format

tab1[ , variable:=paste0(toupper(variable), " ")]
tab1 = dcast(tab1, age~variable+sex)

# rename age category
setnames(tab1, 'age', 'Age Category')

#-----------------------------
# export the file
write.xlsx(tab1, paste0(outDir, 'table1_all_indicators_age_sex.xlsx'))

#------------------------------------------------------
