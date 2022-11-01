# --------------------------------------------
# Caitlin O'Brien-Carelli
#
# June 29, 2022
# Sample code to test that R works
# --------------------------------------------

# --------------------
# Set up R
# --------------------

# --------------------
# clears the workspace - use this line of code every time
rm(list=ls()) 
# --------------------

# --------------------
# install R packages
# you only need to run the installation code once
# once the packages are installed, you just need to load them when you code 
install.packages('Rtools')
install.packages('data.table')
install.packages('stringr')
# --------------------

# --------------------
# load the packages you need to run this code file
library(data.table) # used to manipulate data 
# --------------------

# --------------------
# run a few pieces of code to ensure R works
# to run the code line, highlight it and click "Run" above

# perform basic calculations
2*4
10 - 9
8/4

# create a vector and plot it
x = c(2, 4, 6, 8, 10, 12)
hist(x)
# --------------------
