# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/23/21
# Visuals sources by gysi_prep1.R
# ----------------------------------------------





# --------------------
# Figures specifically for the write-up

pdf(paste0(dir, 'writeup_figures.pdf'), height = 9, width = 16)

# testing by sex, age over time - hts_tst
ggplot(dt_all[variable=='hts_tst'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#b2182b', '#fdae61')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Tested for HIV',
       fill ='Sex') +
  theme(text = element_text(size=24)) 

# tested HIV+ by sex, age over time - hts_tst_pos
ggplot(dt_all[variable=='hts_tst'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#d6604d', '#92c5de')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Tested HIV+',
       fill ='Sex') +
  theme(text = element_text(size=24)) 

# enrolled on ART by sex, age, over time
ggplot(dt_all[variable=='tx_curr'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#4575b4', '#a6dba0')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'On ART',
       fill ='Sex') +
  theme(text = element_text(size=24)) 

# newly enrolled on ART by sex, age over time
ggplot(dt_all[variable=='tx_new'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#35978f', '#c2a5cf')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Newly Enrolled on ART',
       fill ='Sex') +
  theme(text = element_text(size=24)) 


dev.off()
# --------------------


# --------------------------------------------------
# visualize the data

twelve_colors = c(brewer.pal(11, 'RdYlBu'), '#80cdc1')

# sum to the national level

# national - all variables
dt_nat_all = dt[ , .(value = sum(value, na.rm = T)),
                      by = .(age, sex, variable, fq)]

# national - sex disaggregation only
dt_nat_sex = dt[ , .(value = sum(value, na.rm = T)),
                 by = .(sex, variable, fq)]

# national - sex disaggregation only
dt_nat_age= dt[ , .(value = sum(value, na.rm = T)),
                by = .(age, variable, fq)]

# national - sex disaggregation only
dt_nat_all= dt[ , .(value = sum(value, na.rm = T)),
                by = .( variable, fq)]



# --------------------
# bar charts over time by age and sex
i = 1
natl_sex_plots = NULL

for (v in unique(dt_nat_sex$variable)) {
  
  var_name = toupper(v) 
  
  natl_sex_plots[[i]] = ggplot(dt_nat_sex[variable==v], 
                               aes(x=fq, y=value, fill = sex)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_text(position = position_dodge(width=0.9), aes(label=value), vjust=-0.5)+
    scale_fill_manual(values = c('#b2182b', '#fdae61', '#74add1')) +
    theme_bw()+
    labs(x = 'Fiscal Quarter', y = 'Number of Clients',
         title = paste0(var_name, ' by sex, Q1 FY20 - Q3 FY21'), 
         fill ='Sex', 
         caption = '*PrEP indicators were reported semi-annually in FY20')  
  
  i = i+1
}



pdf(paste0(dir, 'bar_plots_over_time_sex.pdf'), height = 9, width = 16 )

for (i in seq(1:length(natl_sex_plots))) {
  print(natl_sex_plots[[i]])
  i = i+1
}

for (i in seq(1:length(natl_all_plots))) {
  print(natl_all_plots[[i]])
  i = i+1
}


dev.off()

pdf(paste0(dir, 'trends_over_time.pdf'), height = 9, width = 16 )

for (i in seq(1:length(natl_all_plots))) {
  print(natl_all_plots[[i]])
  i = i+1
}


dev.off()


# --------------------
# bar plots over time by sex
i = 1
natl_sex_plots = NULL

for (v in unique(dt_nat_sex$variable)) {
  
  var_name = toupper(v) 
  
  natl_sex_plots[[i]] = ggplot(dt_nat_sex[variable==v], 
                               aes(x=fq, y=value, fill = sex)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_text(position = position_dodge(width=0.9), aes(label=value), vjust=-0.5)+
    scale_fill_manual(values = c('#b2182b', '#fdae61', '#74add1')) +
    theme_bw()+
    labs(x = 'Fiscal Quarter', y = 'Number of Clients',
         title = paste0(var_name, ' by sex, Q1 FY20 - Q3 FY21'), 
         fill ='Sex', 
         caption = '*PrEP indicators were reported semi-annually in FY20')  
  
  i = i+1
}

# --------------------
# line graphs over time by age and sex
i = 1
natl_all_plots = NULL

twelve_colors = c(brewer.pal(11, 'Spectral'), '#80cdc1')

for (v in unique(dt_nat_sex$variable)) {
  
  var_name = toupper(v) 
  
  natl_all_plots[[i]] = ggplot(dt_nat_all[variable==v], 
                               aes(x=fq, y=value, group = variable)) +
    geom_point()+
    geom_line()+
    scale_color_manual(values = twelve_colors) +
    theme_bw()+
    labs(x = 'Fiscal Quarter', y = 'Number of Clients',
         title = paste0(var_name, ', Q1 FY20 - Q3 FY21'), 
         color ='Age category', 
         caption = '*PrEP indicators were reported semi-annually in FY20')  
  
  i = i+1
}








