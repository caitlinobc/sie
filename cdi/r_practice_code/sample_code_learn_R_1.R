# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# 9/15/21
# Sample code for the Dream Team
# --------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clears the workspace

# --------------------
# install important R packages on your computer
# you only need to run this section one time 
# once the packages are installed, you can add # in front of the installation code
# then they won't run again

install.packages('Rtools') # this package helps other packages
install.packages('readxl')
install.packages('data.table')
install.packages('ggplot2')
install.packages('RColorBrewer')

# --------------------
# load the packages you need to run this code file
# each time you use R, you need to load the packages you need

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
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/cdi/data/r_training_data/'

# --------------------
# load the data 
# the paste0 function pastes the location of the file (dir) to the name of the file
# you could write the whole thing but... why?
dt = read.csv(paste0(dir, 'datim_sample_data.csv'))

# convert the data to a 'data table'
# data tables are a special file format that makes data cleaning easier
dt = data.table(dt)

# --------------------------------------------
# LET'S EXPLORE AT OUR BEAUTIFUL DATA!

# --------------------
# view the data set in a separate tab
View(dt)

# view the top 5 lines of the data 
# HTS_TST refers to HIV tests, and HTS_TST_POS is the number who tested HIV+
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
# what type of variable is 'quarter' (refers to the fiscal quarter)
class(dt$quarter)

# --------------------
# printing out unique values of specific variables

# these data are at the regional level
# this one is hard - how many specific sites are in the data set?
length(unique(dt$region))

# print me a list of every unique region!!!!
unique(dt$region)

# now it's your turn:
# add code that prints the unique values of the variable


# --------------------

# --------------------------------------------
# PERFORM BASIC CALCULATIONS FOR A REPORT

# --------------------
# first we need to do some quick formatting - convert the quarter to a 'factor'
# a 'factor' just means specifyng the levels/order of an ordinal variable
# for example "children first, then adults"; "Q1 comes before Q2"

dt$quarter = factor(dt$quarter) # set it so Q1 is before Q2 in a graph
# --------------------

# CALCULATE STUFF
# --------------------
# step by step - specify rows, columns, and any "sorts"
dt[variable=='HTS_TST'] # select the rows that show HIV tests
dt[variable=='HTS_TST', sum(value)] # for the rows with hiv tests, sum up the values
dt[variable=='HTS_TST', sum(value), by = sex ] # for the rows with hiv tests, sum up the values by sex

# literally you just did everything you need to do in R ever

# --------------------
# ok let's calculate some stuff 

# what was the total number of hiv tests performed in these data?
dt[variable=='HTS_TST', sum(value)]

# what was the total number of hiv tests performed in these data by quarter?
dt[variable=='HTS_TST', sum(value), by = quarter]

# what was the total number of positive tests in these data by quarter?
dt[variable=='HTS_POS', sum(value), by = quarter]

# what was the total number of positive tests for women in these data by quarter?
dt[variable=='HTS_POS' & sex=='Female', sum(value), by = quarter]

# now you try - what was the total number of hiv tests performed on men?

# you should get 28,513 men - if you don't you did wrong

# what was the total number of hiv tests performed on men in Q2?

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

# NICE! An upward trend

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

ggplot(plot_dt, aes(x = quarter, y = value, fill = factor(sex)))+
  geom_bar(position = "dodge", stat = "identity")+
  facet_wrap(~variable, scales = 'free_y')+
  geom_text(aes(label = value), vjust = -0.2, position = position_dodge(0.9))+
  scale_fill_manual(values = c('#f03b20', '#feb24c'))+
  theme_bw()+
  labs(x = 'Fiscal Quarter', y = 'Clients', fill = 'Sex', 
       title = 'HIV tests and clients who tested HIV+ by sex, FY21')

# --------------------------------------------

# --------------------
# END OF THE CODE

print("Wow! You're now an R coder!!!! Congratulations!!!!")
print("Feel free to e-mail me with any questions!")

# --------------------







