# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 6/11/21
# Eswatini Key Indicators Visualization 
# Global Program Review
# ----------------------------------------------

# ------------------------
# load packages

rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(RColorBrewer)
library(lubridate)
library(plyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)
library(eeptools)
# ------------------------

# ------------------------
# files and directories

# set the working directory to the data set location
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/eswatini/gpr/'
setwd(dir)

# set the output directory for prepped data and visualizations
OutDir = dir

# -------------------------------------
# load the study data and format for use

# load the patient study eligibility and service provision data 
dt = data.table(read_excel(paste0(dir, 'Eswatini Key Indicators Quarterly DATIM.xlsx')))

# -------------------------------------
# PREP THE DATA 

# reset the variable names
setnames(dt, c('variable', 'value', 'fq'))

# add frequency of reporting
dt[ , count:= .N, by = variable]
dt[count==6 | count==4, report:='Quarterly']
dt[count==2 | count==3, report:='Semi-annual']
dt[count==1, report:='Annual']
dt[ ,count:=NULL]

# keep values reporting quarterly
dt = dt[report=='Quarterly']

# ------------------------
# drop indicators Tony is not interested in:
 drops = c("HTS_INDEX (Newly Tested)", "HTS_INDEX (Positives)", "HTS_INDEX (New Positives)",
           "PMTCT_STAT (Known status)", "PMTCT_STAT (Positives)", "PMTCT_STAT (New Positives)", 
           "TB_STAT (Documented HIV status)", "PMTCT_HEI_POS", "TX_CURR_MMD", 
           "TX_PVLS_BF (Supressed)", "TX_PVLS_BF_D (Documented VL)")

# drop all the unnecessary variables
dt = dt[!(variable %in% drops)]

# ------------------------
# final set to keep
keeps = c("VMMC_CIRC", "PREP_NEW", "HTS_TST", "HTS_TST_POS",
          "PMTCT_STAT_D (ANC clients)", "PMTCT_EID", "TB_STAT_D (All TB cases)",
          "TB_ART", "PMTCT_ART", "TX_NEW", "TX_CURR", 
          "VL Coverage Proxy", 'Viral Suppression Rate')

# keep only the final set
dt = dt[(variable %in% keeps)]

# round the values 
dt[ ,value:=round(value, 0)]

# ------------------------
# add 95-95-95 classes to the variables
dt[variable %in% c("VMMC_CIRC", "PREP_NEW"), class:='Prevention']
dt[variable %in% c("HTS_TST", "HTS_TST_POS", "PMTCT_STAT_D (ANC clients)", 
                   "PMTCT_EID", "TB_STAT_D (All TB cases)"), class:='First 95']
dt[variable %in% c("TB_ART", "PMTCT_ART", "TX_NEW", "TX_CURR"), class:='Second 95']
dt[variable %in% c("VL Coverage Proxy",  'Viral Suppression Rate'), class:='Third 95']

# factor the order of the classes
dt$class = factor(dt$class, c('Prevention', 'First 95', 'Second 95', 'Third 95'),
                  c('Prevention', 'First 95', 'Second 95', 'Third 95'))

# ------------------------
# set up variable categories

dt[grepl('HTS', variable), type:='Testing']
dt[grepl('PREP', variable), type:='PrEP']
dt[grepl('TX', variable), type:='Treatment']
dt[grepl('VL', variable) | grepl('Viral', variable), type:='Viral Load']
dt[grepl('PMTCT', variable), type:='PMTCT']
dt[grepl('TB', variable), type:='TB']
dt[grepl('VMMC', variable), type:='VMMC']

# factor the variable type
dt$type = factor(dt$type, c('Testing', 'VMMC', 'PrEP', 
          'PMTCT', 'Treatment', 'TB', 'Viral Load'), 
          c('HIV Testing', 'VMMC', 'PrEP', 
            'PMTCT', 'Treatment', 'TB', 'Viral Load'))

# ------------------------
# factor the variables for the original graphs (to sort by type, not 95)
dt[ , variable_alt:=variable]

dt$variable_alt = factor(dt$variable_alt, c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "HTS_SELF", "VMMC_CIRC",
  "PREP_CURR", "PREP_NEW", "PMTCT_STAT_D (ANC clients)", "PMTCT_ART", "PMTCT_EID",
  "TX_CURR",   "TX_NEW", "TX_ML", "TX_RTT",
  "TB_STAT_D (All TB cases)",  "TB_ART",
  "TX_PVLS_D (Documented VL)", "TX_PVLS (Suppressed VL)",
  "VL Coverage Proxy",  'Viral Suppression Rate'),
  c("HTS_TST", "HTS_TST_POS", "HTS_INDEX", "HTS_SELF", "VMMC_CIRC",
    "PREP_CURR", "PREP_NEW", "PMTCT_STAT_D (ANC clients)", "PMTCT_ART", "PMTCT_EID",
    "TX_CURR",   "TX_NEW", "TX_ML", "TX_RTT",
    "TB_STAT_D (All TB cases)",  "TB_ART",
    "TX_PVLS_D (Documented VL)", "TX_PVLS (Suppressed VL)", 
    "VL Coverage Proxy (%)",  'Viral Suppression Rate (%)'))

# ------------------------
# set up the 95-95-95 order or variables

dt$variable = factor(dt$variable, 
                     c("VMMC_CIRC", "PREP_NEW", "HTS_TST", "HTS_TST_POS",
                       "PMTCT_STAT_D (ANC clients)", "PMTCT_EID", "TB_STAT_D (All TB cases)",
                       "TB_ART", "PMTCT_ART", "TX_NEW", "TX_CURR", 
                       "VL Coverage Proxy",  'Viral Suppression Rate'), 
                     c("VMMC_CIRC", "PREP_NEW", "HTS_TST", "HTS_TST_POS",
                       "PMTCT_STAT_D (ANC clients)", "PMTCT_EID", "TB_STAT_D (All TB cases)",
                       "TB_ART", "PMTCT_ART", "TX_NEW", "TX_CURR", 
                       "VL Coverage Proxy (%)",  'Viral Suppression Rate (%)'))

# ------------------------
# sort by type to mimic the cascade of care - 95-95-95
dt = dt[order(class, variable)]

# -------------------------------------

# ------------------------
# export data set for tableau

dt[ , value:=round(value, 0)]
dt[variable == "VL Coverage Proxy (%)", exclude:=TRUE]
dt[variable != "VL Coverage Proxy (%)", exclude:=FALSE]
write.csv(dt, paste0(OutDir, 'key_indicators_tableau.csv'))

# ------------------------

# -------------------------------------
# VISUALIZE

# ------------------------
# color palettes

type_colors = c('#d73027', '#fdae61', '#045a8d', '#6a51a3', '#7fcdbb', 
                '#74a9cf', '#4d4d4d')

colors95 = c('#fdae61', '#045a8d', '#6a51a3', '#7fcdbb', 
             '#74a9cf', '#4d4d4d')

blue_cols = c('#253494', '#2c7fb8', '#74a9cf', '#41b6c4')

# ------------------------
# Facet all variables

# ------------------------
# 95-95-95 graphs 

pdf(width = 20, height = 10, paste0(OutDir, 'key_indicator_graphs_all.pdf'))

# all figures as requested - 5 columns
ggplot(dt, aes(x = fq, y = value, group=variable, color = class))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = colors95)+
  facet_wrap(class~variable, scales = 'free_y', ncol=5)+
  theme_minimal()+
  xlab('Fiscal Quarter')+
  ylab('')+
  theme(legend.title = element_blank())

# all figures as requested - 5 columns
ggplot(dt, aes(x = fq, y = value, group=variable, color = class))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = blue_cols)+
  facet_wrap(class~variable, scales = 'free', ncol=5)+
  theme_minimal()+
  xlab('Fiscal Quarter')+
  ylab('')+
  theme(legend.title = element_blank())

# all figures as requested - 5 columns
ggplot(dt, aes(x = fq, y = value, group=variable, color = class))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = blue_cols)+
  facet_wrap(class~variable, scales = 'free', ncol=3)+
  theme_minimal()+
  xlab('Fiscal Quarter')+
  ylab('')+
  theme(legend.title = element_blank())


# all figures as requested
ggplot(dt, aes(x = fq, y = value, group=variable, color = class))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = colors95)+
  facet_wrap(class~variable, scales = 'free_y')+
  theme_minimal()+
  xlab('Fiscal Quarter')+
  ylab('')+
  theme(legend.title = element_blank())


dev.off()




# all vars on same graph color by type
p1 = ggplot(dt[variable!= "VL Coverage Proxy (%)"], 
         aes(x = fq, y = value, group=variable, color = type))+
         geom_point()+
         geom_line()+
  scale_color_manual(values = type_colors)+
         facet_wrap(type~variable, scales = 'free_y', ncol=3)+
          theme_minimal()+
          xlab('Fiscal Quarter')+
          ylab('Clients')+
           theme(legend.title = element_blank())
         

# all vars on same graph color by type
p2 = ggplot(dt[variable!= "VL Coverage Proxy (%)"],
  aes(x = fq, y = value, group=variable))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = type_colors)+
  facet_wrap(type~variable, scales = 'free_y', ncol=3)+
  theme_minimal()+
  xlab('Fiscal Quarter')+
  ylab('Clients')+
  theme(legend.title = element_blank())

# ------------------------
# VL coverage 

p3 = ggplot(dt[type=='Viral Load'],
  aes(x = fq, y = value, color=variable, group = variable))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = type_colors)+
  facet_wrap(~variable, scales = 'free_y')+
  theme_minimal()+
  labs(title = 'Viral load testing coverage and viral suppression',
       x = 'Fiscal Quarter', y = '', color = '')

# ------------------------
# loop by type of variable - same scale
type_plots = NULL
i = 1
for (t in unique(dt$type)) {
  plot_title = t
  plot_title = as.character(plot_title)
  
  type_plots[[i]] = ggplot(dt[type==t & variable!= "VL Coverage Proxy (%)"],
    aes(x = fq, y = value, group=variable))+
    geom_point()+
    geom_line()+
    scale_color_manual(values = type_colors)+
    facet_wrap(~variable)+
    theme_minimal()+
    labs(x = 'Fiscal Quarter', y = '', title = plot_title)
  i =i+1 #reset the index for the next group
}


# ------------------------
# loop by type of variable - distinct scales

type_plots_scales = NULL
i = 1
for (t in unique(dt$type)) {
  plot_title = t
  plot_title = as.character(plot_title)
  
  type_plots_scales[[i]] = ggplot(dt[type==t & variable!= "VL Coverage Proxy (%)"],
    aes(x = fq, y = value, group=variable))+
    geom_point()+
    geom_line()+
    scale_color_manual(values = type_colors)+
    facet_wrap(~variable, scales = 'free_y')+
    theme_minimal()+
    labs(x = 'Fiscal Quarter', y = '', title = plot_title)
  i =i+1 #reset the index for the next group
}


# --------------------------------------
# export visuals of the key indicators

# ------------------------
# key indicator time trends
pdf(width = 14, height = 10, paste0(OutDir, 'key_indicator_graphs.pdf'))

p1
p3
p2
type_plots
type_plots_scales

dev.off()

# ------------------------
