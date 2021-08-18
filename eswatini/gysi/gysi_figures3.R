

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
natl_sex_plots = NULL

twelve_colors = c(brewer.pal(11, 'Spectral'), '#80cdc1')

for (v in unique(dt_nat_sex$variable)) {
  
  var_name = toupper(v) 
  
  natl_all_plots[[i]] = ggplot(dt_nat_all[variable==v], 
                               aes(x=fq, y=value, color = age, group = age)) +
    facet_wrap(~sex)+
    geom_point()+
    geom_line()+
    scale_color_manual(values = twelve_colors) +
    theme_bw()+
    labs(x = 'Fiscal Quarter', y = 'Number of Clients',
         title = paste0(var_name, ' by sex and age, Q1 FY20 - Q3 FY21'), 
         color ='Age category', 
         caption = '*PrEP indicators were reported semi-annually in FY20')  
  
  i = i+1
}








