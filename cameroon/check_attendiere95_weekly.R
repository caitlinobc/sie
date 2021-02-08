# 
# var_list = full_data[ ,.(variable = unique(variable)), by= file_name]
# 
# x2 = var_list[file_name=="littoral fy21_week02", .(indicator = unique(variable))]
# x3 = var_list[file_name=="littoral fy21_week03", .(indicator = unique(variable))]
# 
# x2 = x2[order(indicator)]
# x3 = x3[order(indicator)]
# x2[ , set:=2]
# x3[ ,set:=3]
# 
# write.csv(x2, paste0(OutDir, 'prepped/x2.csv'))
# write.csv(x3, paste0(OutDir, 'prepped/x3.csv'))
# 
# View(x3[!(indicator %in% x2$indicator)])

# --------------------
#create and export a list of variables to examine

# var_list = full_data[ ,unique(variable)]
# write.csv(var_list, paste0(OutDir, 'prepped/variables.csv'))
# 
# # read in the categories and merge
# cats = read.table(paste0(OutDir, 'prepped/variable_categories.csv'))
# 
# # merge in the variable categories to subset the data 
# 


print("Ok")