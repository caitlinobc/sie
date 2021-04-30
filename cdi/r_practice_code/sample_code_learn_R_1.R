# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/29/21
# Sample code for the Dream Team
# --------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clears the workspace

# --------------------
# install important R packages on your computer
# you only need to run this section one time 
# once the packages are installed, you can add # in front of the installation code

install.packages('Rtools') # this package helps other packages
install.packages('readxl')
install.packages('data.table')
install.packages('ggplot2')
install.packages('RColorBrewer')

# --------------------
# load the packages you need to run this code file

library(readxl) # loads excel files
library(data.table) # manipulates data
library(ggplot2) # creates beautiful maps and graphs
library(RColorBrewer) # allows for nice changes in color palettes
# --------------------

# --------------------------------------------
# set directories and import the data

# CHANGE THIS TO THE LOCATION OF THE DATA ON YOUR COMPUTER
# set the main directory where the data are stored
# use forward slashes / for file names
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/CDI/data/r_training_data/'

# --------------------
# load the data 
dt = read.csv(paste0(dir, 'datim_sample_data.csv'))

# convert the data to a 'data table'
# data tables are a special format that makes data cleaning easier
dt = data.table(dt)

# --------------------------------------------
# LET'S EXPLORE AT OUR BEAUTIFUL DATA!

# --------------------
# view the data set in a separate tab
View(dt)

# view the top 5 lines of the data 
head(dt, n = 5) # change n = to see a different number of lines

# how many rows are there in the data set?
nrow(dt)

# what are the names of the variables included in this data set?
names(dt)

# view the 'structure' of the data
# which data types are included? numerics, strings, logicals (booleans), dates?
str(dt)

# hmmm... I only see characters and integers (numerics) 
# let's check the type of a specific variable
class(dt$quarter)

# --------------------
# printing out unique values of specific variables

# these data are at the regional level
# this one is hard - how many specific sites are in the data set?
length(unique(dt$region))

# print me a list of every unique site!!!!
unique(dt$region)

# now it's your turn:
# add code that prints the unique values of the variable

dt$quarter = factor(dt$quarter)

# --------------------

# --------------------------------------------
# PERFORM BASIC CALCULATIONS FOR A REPORT

# --------------------
# first we need to do some quick formatting - convert the quarter to a 'factor'
# a 'factor' just means specifyng the levels/order of an ordinal variable
# for example "children first, then adults"; "Q1 comes before Q2"

dt$quarter = factor(dt$quarter) # set it so Q1 is before Q2 in a graph
# --------------------

# --------------------
# what was the total number of hiv tests performed in these data?
dt[variable=='HTS_TST', sum(value)]

# what was the total number of hiv tests performed in these data by quarter?
dt[variable=='HTS_TST', sum(value), by = quarter]

# what was the total number of positive tests in these data by quarter?
dt[variable=='HTS_POS', sum(value), by = quarter]

# what was the total number of positive tests for women in these data by quarter?
dt[variable=='HTS_POS' & sex=='Female', sum(value), by = quarter]

# now you try - how many men were tested for HIV in Q1?
# write code here:



# you should get 28,513 men

# --------------------

# --------------------------------------------

# --------------------------------------------
# VISUALIZE THE DATA 

# --------------------
# sum up the data to the level of the plot
# in this case we are visualizing at the national level by sex and quarter
# data are regional, so we need to 'sum up' to the national level
plot_dt = dt[ ,.(value = sum(value)), by = .(variable, quarter, sex)]

# plot the data as a time trend
ggplot(plot_dt[variable=='HTS_TST'], 
       aes(x = quarter, y = value, color = sex, group = sex))+
  geom_point()+ # adds dots to the graph
  geom_line()+ # adds a line graph
  theme_bw() # makes the graph pretty

# --------------------
# make an ugly 'spaghetti plot' at the regional level

ggplot(dt[variable=='HTS_TST'], 
       aes(x = quarter, y = value, color = region, group = region))+
  geom_point()+ 
  geom_line()+ 
  facet_wrap(~sex)+ # splits the graphs up side-by-side by sex
  theme_bw()+
  labs(title = 'Number of HIV tests performed (HTS_TST) by region')

# --------------------
# BONUS: make a stacked bar graph - this is advanced R, but fun as a bonus:

ggplot(plot_dt, aes(x = quarter, y = value, fill = sex))+
  geom_bar(position = "dodge", stat = "identity")+
  facet_wrap(~variable, scales = 'free_y')+
  theme_bw()+
  labs(x = 'Fiscal Quarter', y = 'Clients', fill = 'Sex')

# --------------------------------------------

# --------------------
# END OF THE CODE

print("Wow! You're now an R coder!!!! Congratulations!!!!")
print("Feel free to e-mail me with any questions!")

# --------------------







