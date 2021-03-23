#--------------------------
# Run a series of checks on the weekly CDC Reporting
# 
# Caitlin O'Brien-Carelli
# 2/9/21
#--------------------------

#--------------------------
# check that every sheet has the same number of variables
# weeks 1 and 2 have 34;weeks 3 - 18 have 30
full_data[,length(unique(variable)), by = file_name]

#--------------------------
#print these lists to make sure they make sense

full_data[ ,unique(sex)]
full_data[ ,unique(region)]
full_data[ ,unique(district)]
full_data[ ,unique(facility)]

# check lengths of the categories
if (full_data[ ,length(unique(sex))]==3) {print ("Correct sex categories!")
} else {print("Something is wrong with the sex categories.")}

# if any of the data are missing a value these will print as incorrect
if (full_data[ ,length(unique(age))]==2) {print ("Correct age categories!")
} else {print("Something is wrong with the age categories.")}

if (full_data[ ,length(unique(region))]==2) {print ("Correct regions!")
} else {print("Something is wrong with the regions.")}

if (full_data[ ,length(unique(facility))]==76) {print ("Correct districts!")
} else {print("Something is wrong with the districts.")}

if (full_data[ ,length(unique(sex))]==3) {print ("Correct sex categories!")
} else {print("Something is wrong with the sex categories.")}

if (full_data[ ,length(unique(sex))]==3) {print ("Correct sex categories!")
} else {print("Something is wrong with the sex categories.")}





# indicator mapping
ind = full_data[ ,.(variable = unique(variable)), by = file_name]
ind = dcast(ind, variable~file_name, value.var = 'variable')

# export a list of indicators from each file
write.csv(ind, paste0(OutDir, 'att95_prepped/variable_map.csv'))










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