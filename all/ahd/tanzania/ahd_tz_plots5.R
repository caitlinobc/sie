#-------------------------
# Diagnostic Plots
# Sourced from ahd_data_prep.R
#-------------------------



# --------------------
# convert eligibility classification to a factor

dt$ahd_elig = factor(dt$ahd_elig, levels = c(1:6), labels = c('Newly diagnosed',
                                                              'LTFU and returned', 'On ART vir failure', 'On ART AIDS ill',
                                                              'Under 5', 'CD4 < 200'))
#-------------------------
# color palettes 
tri_colors = c('#b2182b', '#fdae61', '#74add1')
bar_colors = c('#d73027', '#f46d43', '#fdae61', '#74add1', '#4575b4')

#-------------------------
# age classifications

# histogram of age
h1 = hist(dt$age)

# histogram of first cd4 count
h2 = hist(dt$cd4_after_ahdelig_result)

# histogram of who stage upon enrollment
h3 = hist(dt$whostage1st)

# jittered scatter of age by enrollment classification
g1 = ggplot(dt, aes(x = ahd_elig, y = age, color = ahd_elig))+
         geom_jitter()+
         theme_bw()

# box plot of age by enrollment classification
g2 = ggplot(dt, aes(x = ahd_elig, y = age))+
  geom_boxplot()+
  theme_bw()

# scatter of age by CD4 at enrollment
g3 = ggplot(dt, aes(x = cd4_after_ahdelig_result, y = age))+
  geom_point()+
  theme_bw()

# scatter of age by CD4 at enrollment with eligibility classification
g4 = ggplot(dt, aes(x = cd4_after_ahdelig_result, y = age, color = ahd_elig))+
  geom_point()+
  theme_bw()

#-------------------------

#-------------------------
# plot the logical variables by site just for counts

# create a data set solely composed of logicals
log_vars = names(dt)[!(names(dt) %in% log_byVars)]
dt_long = melt(dt, id.vars = idVars , measure.vars = log_vars)

# loop through the variables and plot them
vars = dt_long[,unique(variable)]

# createa new data set for the logicals reflecting response counts
i = 1
for (v in vars){
var_name = v

# create a data set ofcounts of responses for this variable by site
trues = dt_long[variable==v, .(value = sum(value, na.rm=T), category = 'Yes'), by=siteid]

falses = dt_long[variable==v & value==FALSE, .(value = length(unique(pid)), 
                            category = 'No'), by=siteid]

missing = dt_long[variable==v & is.na(value), .(value = length(unique(pid)), 
                                               category = 'Missing'), by=siteid]
dt_new = rbind(trues, falses, missing)
dt_new[ ,variable:=v]

# bind it to the other variables
if (i==1) dt_counts = dt_new
if (1 < i) dt_counts = rbind(dt_counts, dt_new)

i = i+1}

# factor the counts to plot bars in order
dt_counts$category = factor(dt_counts$category, levels = c('Yes', 'No', 'Missing'), 
                     labels = c('Yes', 'No', 'Missing'))

#-------------------------
# counts of ahd eligibility by site

ahd_elig_counts = dt[, .(value=length(unique(pid))), by =.(ahd_elig, siteid)]

ga= ggplot(ahd_elig_counts, aes(x=siteid, y=value, fill=ahd_elig)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = bar_colors, na.value = '#bababa')+
  theme_bw()+
  labs(x = 'Site ID', y = 'Count of responses',
       title = "AHD Study eligibility criterion, count of patients enrolled by site",
       subtitle = 'Tanzania Baseline Cohort',
       fill ='Reason for enrollment')


#-------------------------
# counts of who stage by site

who_counts = dt[, .(value=length(unique(pid))), by =.(whostage1st, siteid)]

gb = ggplot(who_counts, aes(x=siteid, y=value, fill=factor(whostage1st))) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = rev(brewer.pal(5, 'Greens')), na.value = '#bababa')+
  theme_bw()+
  labs(x = 'Site ID', y = 'Number of patients',
       title = "WHO Clinical Stage at enrollment by site",
       subtitle = 'Tanzania Baseline Cohort',
       fill ='WHO Clinical Stage')


#-------------------------

test_plots = NULL

i = 1
for (v in vars) {

plot_title = v

test_plots[[i]] = ggplot(dt_counts[variable==v], aes(x=siteid, y=value, fill=category)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(position = position_dodge(width=0.9), aes(label=value), vjust=-0.5)+
  scale_fill_manual(values = c('#b2182b', '#fdae61', '#74add1')) +
  theme_bw()+
  labs(x = 'Site ID', y = 'Count of responses',
        title = paste0('Variable name: ', plot_title), 
       fill ='Response')

i =i+1
}


#-------------------------

pdf(width=12, height = 9, paste0(dir, 'outputs/diagnostic_plots.pdf'))

ga
gb
h1 
h2
h3
g1
g2
g3
g4
test_plots

dev.off()




