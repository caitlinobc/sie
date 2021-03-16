# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# Source figures to evaluate data quality
# TB Services Tab
# 3/15/2021
# --------------------------------------------

# ------------------------
# color palettes 

blues= colorRampPalette(brewer.pal(9, 'Blues'))(15)
reds = colorRampPalette(rev(brewer.pal(9, 'YlOrRd')))(15)
sexes = c('#cb181d', '#4292c6', '#969696')
regions = c('#66c2a4', '#6a51a3')

# ------------------------
# count of facilities reporting

# count the number of facilities per week
fac_count = dt[ , .(facility = length(unique(facility))), 
                by = start_wk]
fac_reg = dt[ , .(facility = length(unique(facility))), 
          by = .(start_wk, week, region)]

# count of facilities reporting - line
f1 = ggplot(fac_count, aes(x = start_wk, y = facility))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title = "Number of facilities reporting CCD indicators by week",
       subtitle = "November 2020 - March 2021",
       x = 'Date*',
       y = "Number of health facilities",
       caption = "*Date indicates the start date of the week",
       color = "Region")

# count of facilities reporting by region - line
f2 = ggplot(fac_reg, aes(x = start_wk, y = facility, color = factor(region)))+
      geom_point()+
      geom_line()+
    scale_color_manual(values = regions)+
      scale_y_continuous(breaks = 1:10)+
      theme_bw()+
      labs(title = "Number of facilities reporting CCD indicators by week, region",
       subtitle = "November 2020 - March 2021",
       x = 'Date*',
       y = "Number of health facilities",
       caption = "*Date indicates the start date of the week",
       color = "Region")

# count of facilities reporting by region - stacked bar
f3 = ggplot(fac_reg, aes(x = start_wk, y = facility, fill = region,
                    label = facility))+
  geom_bar(position = 'stack', stat = 'identity')+
  geom_text(size = 4, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = regions)+
  theme_bw()+
  labs(title = "Number of facilities reporting CCD indicators by week, region",
       subtitle = "November 2020 - March 2021",
       x = 'Date*',
       y = "Number of health facilities",
       caption = "*Date indicates the start date of the week",
       fill = "Region")

# count of facilities reporting by region - stacked bar
f4 = ggplot(fac_reg[year(start_wk)==2021], aes(x = start_wk, y = facility, fill = region,
                    label = facility))+
  geom_bar(position = 'stack', stat = 'identity')+
  scale_fill_manual(values = c('#dadaeb', '#c7e9b4'))+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_bw()+
  labs(title = "Number of facilities reporting CCD indicators by week, region",
       subtitle = "January - March 2021",
       x = 'Date*',
       y = "Number of health facilities",
       caption = "*Date indicates the start date of the week",
       fill = "Region")

# ------------------------
# patients seen by location

# total seen by all locations
lc = dt[ ,.(ppl_seen = sum(ppl_seen, na.rm=T)), by = .(month, location)]

# total seen by all locations
lc_all = dt[ ,.(ppl_seen = sum(ppl_seen, na.rm=T)), by = .(location)]

# count of facilities reporting by region - stacked bar
l1 = ggplot(lc_all, aes(x = reorder(location, -ppl_seen), y = ppl_seen, 
  fill = factor(location), label = ppl_seen))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_text(vjust = -1)+
  scale_fill_manual(values = blues)+
  theme_bw()+
  labs(title = "Number of people seen at the CCD by location",
       subtitle = "November 2020 - March 2021",
       x = '',
       y = "Number of people seen",
       fill = "Location of CCD")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
  
# count of facilities reporting by region - stacked bar
l2 = ggplot(lc, aes(x = month, y = ppl_seen, fill = reorder(location, -ppl_seen), label = ppl_seen))+
    geom_bar(position = 'dodge', stat = 'identity')+
    scale_fill_manual(values = reds)+
    theme_bw()+
    labs(title = "Number of people seen at the CCD by location",
         subtitle = "December 2020 - March 2021",
         x = 'Date (monthly)',
         y = "Number of health facilities",
         fill = "Region")
  
# ------------------------
# missing data


# --------------------------------------------

# create total long files
dtl_tot = dt_long[ ,.(value = sum(value, na.rm=T)), by = c('variable','week',
      'start_wk', 'end_wk', 'month', 'qtr')]

dtl_sex = dt_long[ ,.(value = sum(value, na.rm=T)), 
      by = c('variable', 'sex', 'week', 'start_wk', 'end_wk',
      'month', 'qtr')]

dtl_age = dt_long[ ,.(value = sum(value, na.rm=T)),
      by = c('variable','age','week', 'start_wk', 'end_wk',
      'month', 'qtr')]

dtl_reg = dt_long[ ,.(value = sum(value, na.rm=T)), 
                   by = c('variable', 'region', 
                          'week', 'start_wk', 'end_wk',
                          'month', 'qtr')]

# ------------------------
# loop by sex
list_of_plots_sex = NULL
i = 1
for (v in unique(dt_long$variable)) {
  
  title = paste0(as.character(v), ' by sex')
  
list_of_plots_sex[[i]] = ggplot(dtl_sex[variable==v], aes(x = start_wk, 
    y = value, color = sex))+
    geom_point()+
    geom_line()+
    scale_color_manual(values = sexes)+
    theme_bw()+
    labs(title = title,
         subtitle = "November 2020 - March 2021",
         x = 'Date*',
         y = "Count",
         caption = "*Date indicates the start date of the week",
         color = "Sex")

i = i+1
}
# ------------------------

# # ------------------------
# # loop by age
# list_of_plots_age = NULL
# i = 1
# for (v in unique(dt_long$variable)) {
#   
#   title = paste0(as.character(v), ' by sex')
#   
#   list_of_plots_age[[i]] = ggplot(dtl_age[variable==v], 
#     aes(x = start_wk, 
#     y = value, color = age))+
#     geom_point()+
#     geom_line()+
#     theme_bw()+
#     labs(title = title,
#          subtitle = "November 2020 - March 2021",
#          x = 'Date*',
#          y = "Count",
#          caption = "*Date indicates the start date of the week",
#          color = "Sex")
#   
#   i = i+1
# }
# 

# ------------------------

# --------------------------------------------
pdf(pdf_out, width = 12, height = 9)

f1
f2
f3
f4
l1
l2

for(f in seq(1:length(list_of_plots_sex))) {
  print(list_of_plots_sex[[f]])}
# 
# for(f in seq(1:length(list_of_plots_age))) {
#   print(list_of_plots_age[[f]])}

dev.off()


