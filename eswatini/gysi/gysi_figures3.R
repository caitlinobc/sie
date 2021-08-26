# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/23/21
# Visuals sources by gysi_prep1.R
# ----------------------------------------------

# --------------------
# color palettes

twelve_colors = c(brewer.pal(11, 'RdYlBu'), '#80cdc1')

# --------------------
# Figures specifically for the write-up

# --------------------
 # pdf(paste0(outDir, 'writeup_figures.pdf'), height = 9, width = 16)

# --------------------
# PREP ENROLLMENT

# current prep enrollment - sex, age
ggplot(df[variable=='prep_curr'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 7)+
  scale_fill_manual(values = c('#73afb6', '#a6bddb')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Enrolled on PrEP',
       fill ='') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank()) 

# new prep enrollment - sex, age
ggplot(df[variable=='prep_new'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 7)+
  scale_fill_manual(values = c('#94D0CC', '#c2a5cf')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Newly Enrolled on PrEP',
       fill ='Sex') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank())

# hiv testing - sex, age
ggplot(df[variable=='hts_tst'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 5)+
  scale_fill_manual(values = c('#d6604d', '#92c5de')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Tested for HIV',
       fill ='Sex') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank()) 

# tested HIV+ - sex, age
ggplot(df[variable=='hts_tst_pos'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#b2182b', '#fdae61')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Tested HIV+',
       fill ='Sex') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank()) 

# enrolled on ART - sex, age
ggplot(df[variable=='tx_curr'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 7)+
  scale_fill_manual(values = c('#4575b4', '#a6dba0')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'On ART',
       fill ='Sex') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank()) 

# newly enrolled on ART - sex, age
ggplot(df[variable=='tx_new'], 
       aes(x=age, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 6)+
  scale_fill_manual(values = c('#94D0CC', '#c2a5cf')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Newly Enrolled on ART',
       fill ='') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank()) 

#-------------------
# create a vls table and shape long to plot
vl = tab1[ , c(1, 20:21)]
setnames(vl, c('age', 'Female', 'Male'))
vl = vl[!is.na(Female)]
vl = melt(vl, id.vars = 'age')

#-------------------

# VLS - sex, age 
ggplot(vl, aes(x=age, y=value, fill = variable)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#94D0CC', '#c2a5cf')) +
  theme_bw()+
  labs(x = 'Age Category', y = 'Newly Enrolled on ART',
       fill ='') +
  theme(text = element_text(size=20),
        legend.position = "top", 
        legend.title = element_blank()) 



dev.off()
# --------------------

# --------------------
# Youth-specific figures

# sum the table to children, adolescents, youth, and adults
yt = dt[ ,.(value = sum(value)), by = .(sex, caya, variable)]

# --------------------
pdf(paste0(outDir, 'youth_figures.pdf'), height = 9, width = 16)

# --------------------
# PREP ENROLLMENT

# current prep enrollment - sex, age
ggplot(yt[variable=='prep_curr'], 
       aes(x=caya, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 7)+
  scale_fill_manual(values = c('#73afb6', '#a6bddb')) +
  theme_bw()+
  labs(y = 'Enrolled on PrEP',
       fill ='') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank(),
        axis.title.x = element_blank()) 

# new prep enrollment - sex, age
ggplot(yt[variable=='prep_new'], 
       aes(x=caya, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 7)+
  scale_fill_manual(values = c('#f03b20', '#feb24c')) +
  theme_bw()+
  labs( y = 'Newly Enrolled on PrEP',
       fill ='Sex') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank(),
        axis.title.x = element_blank())

# hiv testing - sex, age
ggplot(yt[variable=='hts_tst'], 
       aes(x=caya, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 5)+
  scale_fill_manual(values = c('#d6604d', '#92c5de')) +
  theme_bw()+
  labs(y = 'Tested for HIV',
       fill ='Sex') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank(),
        axis.title.x = element_blank()) 

# tested HIV+ - sex, age
ggplot(yt[variable=='hts_tst_pos'], 
       aes(x=caya, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 7)+
  scale_fill_manual(values = c('#74a9cf', '#a6bddb')) +
  theme_bw()+
  labs(y = 'Tested HIV+',
       fill ='Sex') +
  theme(text = element_text(size=26),
        legend.position = "top", 
        legend.title = element_blank(),
        axis.title.x = element_blank()) 

# enrolled on ART - sex, age
ggplot(yt[variable=='tx_curr'], 
       aes(x=caya, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 7)+
  scale_fill_manual(values = c('#4575b4', '#a6dba0')) +
  theme_bw()+
  labs(y = 'On ART',
       fill ='Sex') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank(),
        axis.title.x = element_blank())

# newly enrolled on ART - sex, age
ggplot(yt[variable=='tx_new'], 
       aes(x=caya, y=value, fill = sex)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), 
            aes(label=value), vjust=-0.5, size = 7)+
  scale_fill_manual(values = c('#94D0CC', '#c2a5cf')) +
  theme_bw()+
  labs(y = 'Newly Enrolled on ART',
       fill ='') +
  theme(text = element_text(size=28),
        legend.position = "top", 
        legend.title = element_blank(),
        axis.title.x = element_blank()) 


dev.off()
# --------------------


















# --------------------------------------------------
# visualize the data



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








