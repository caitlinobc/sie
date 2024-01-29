# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/19/2023
# IHME Managing Research Scientist Test
# Import COVID-19 data, visualize, and analyze
# Includes descriptive statistics and U.S. maps
# ----------------------------------------------

# ------------------------
# load packages

rm(list=ls()) # clear the workspace
library(readxl)
library(lubridate)
library(plyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)
library(data.table)
# ------------------------

# ----------------------------------------------
# DIRECTORIES AND SHAPE FILES
# ----------------------------------------------
# ------------------------
# set working directories 
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/ihme/'

# mapping directory
mapDir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/maps/shape_files/'
# ------------------------

# ----------------------------------------------
# IMPORT & CLEAN THE DATA 
# ----------------------------------------------

# ------------------------
# import the data and rename the columns for coding ease
dt = data.table(read_excel(paste0(dir, 'covid_data_cases_deaths_hosp.xlsx')))
setnames(dt, c('loc_id', 'state', 'country', 'date', 
               'cases', 'deaths', 'pop', 'hosps', 'state_id'))
dt[ , state_id:=NULL] # this value is always 1 - drop
# ------------------------

# ------------------------
# check that location ID 102 (missing state) most likely represents national data           
dt[loc_id==102]
test = dt[ ,.(state = unique(state)), by = pop]
test[!is.na(state), sum(pop)] # similar total population (slightle less - missing WA)  

# save national data and drop out for now
natl = dt[loc_id==102]
dt = dt[loc_id!=102]

# reformat date as a date type variable
dt[ , date:=as.Date(date)]

# add a variable summarizing the number of days from day 1
first = as.Date("2020-01-26")
dt[ , days:=as.numeric(date - first)]
# ------------------------

# ------------------------
# summarize missingness and data errors - checks
dt[ , length(unique(state))] # includes 49 states (no WA) and dc
dt[ , unique(state), by = loc_id] 
dt[is.na(state)]
dt[ ,range(date)]
# ------------------------

# ----------------------------------------------
# SUMMARIZE THE ASSOCIATION
# ----------------------------------------------

# ------------------------
# sum to the national level to examine national data over time
tot = dt[ ,.(cases = sum(cases, na.rm=T), deaths = sum(deaths, na.rm = T),
             hosps = sum(hosps, na.rm=T)), by = .(days, date)][order(days)]

# create a long version 
tot_long = melt(tot, id.vars = c('days', 'date'))

# factor the variable 
tot_long$variable = factor(tot_long$variable, levels = c('cases', 'deaths', 'hosps'),
          labels = c('Cumulative Cases', 'Cumulative Deaths',
                     'Cumulative Hospitalizations' ))

# ------------------------

# ------------------------c('cases', 'deaths', 'hosps'),
# run a nice plot showing the variables
options(scipen=999)
ggplot(tot_long, aes(x=date, y=value, color=variable))+
  geom_line()+
  xlab("Date")+
  ylab("Count")+
  labs(color=NULL)+
  theme_bw()+ 
  theme(text=element_text(size=18))


# ------------------------

# ------------------------
# examine the correlations between the variables
cor.test(tot$cases, tot$deaths, method = "pearson")
cor.test(tot$cases, tot$hosps, method = "pearson")
cor.test(tot$hosps, tot$deaths, method = "pearson")

# check for differences from a standard normal distribution
# all three fail the test (and are non-linear)
shapiro.test(tot$deaths)
shapiro.test(tot$cases)
shapiro.test(tot$hosps)

# visualize the correlation between cases and hospitalizations
ggscatter(tot, x = "cases", y = "hosps", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Relationship between confirmed cases and hospitalizations",
          xlab = "Number of Confirmed Cases", ylab = "Number of Hospitalizations")+
  theme(text=element_text(size=18))

# visualize the correlation between cases and deaths 
ggscatter(tot, x = "cases", y = "deaths", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Relationship between confirmed cases and deaths",
          xlab = "Number of Confirmed Cases", ylab = "Number of Deaths")+
          theme(text=element_text(size=18))
# ------------------------


# ----------------------------------------------
# FIT A CURVE
# ----------------------------------------------

# ------------------------
# calculate the daily deaths (not cumulative)
tot[ , prev_days_deaths:=data.table::shift(deaths, 1L, type="lag")]
tot[is.na(prev_days_deaths), prev_days_deaths:=0]
tot[ ,todays_deaths:=(deaths - prev_days_deaths)]

tot[ , prev_days_cases:=data.table::shift(cases, 1L, type="lag")]
tot[is.na(prev_days_cases), prev_days_cases:=0]
tot[ ,todays_cases:=(cases - prev_days_cases)]
# ------------------------

# ------------------------
# fit a curve of daily deaths by day 

# create a data set with just deaths and days
death_data = data.table(x=tot$days, y = tot$deaths)

# fit polynomial regression models up to degree 5 
linear_model1 <- lm(y~x, data=death_data) 
linear_model2 <- lm(y~poly(x,2,raw=TRUE), data=death_data) 
linear_model3 <- lm(y~poly(x,3,raw=TRUE), data=death_data) 
linear_model4 <- lm(y~poly(x,4,raw=TRUE), data=death_data) 
linear_model5 <- lm(y~poly(x,5,raw=TRUE), data=death_data) 

# create a basic scatterplot  
plot(death_data$x, death_data$y, ylab = 'Cumulative Deaths',
     xlab = 'Number of Days', cex.lab=1.5, cex.axis=1.5) 

# define x-axis values using the maximum value in the data set
p = max(death_data$y)
x_axis <- c(0, seq(1, p, length=p))

# add curve of each model to plot 
lines(x_axis, predict(linear_model1, data.frame(x=x_axis)), col='green') 
lines(x_axis, predict(linear_model2, data.frame(x=x_axis)), col='red') 
lines(x_axis, predict(linear_model3, data.frame(x=x_axis)), col='purple') 
lines(x_axis, predict(linear_model4, data.frame(x=x_axis)), col='blue') 
lines(x_axis, predict(linear_model5, data.frame(x=x_axis)), col='orange')
# ------------------------

# ------------------------
# choose the best model

summary(linear_model1)$adj.r.squared 
summary(linear_model2)$adj.r.squared 
summary(linear_model3)$adj.r.squared 
summary(linear_model4)$adj.r.squared 
summary(linear_model5)$adj.r.squared 
# ------------------------

# ------------------------
# visualizing the best model 

# using loess, which is way easier
ggplot(tot, aes(x=date, y=deaths, color = 'red'))+
  geom_line()+
  geom_smooth(method='loess', se=TRUE)

# using the fifth order polynomial
ggplot(tot, aes(x=date, y=deaths))+
    geom_line()+
    stat_smooth(method="lm", se=TRUE,
          formula=y ~ poly(x, 5, raw=TRUE),colour="red")+
          xlab("Date")+
           ylab("Cumulative Deaths")+
          theme(text=element_text(size=18))+
  theme_bw()

# ----------------------------------------------
# PREDICT USING THE CURVE
# ----------------------------------------------
# use the linear model to predict the values
summary(linear_model5)

# predict for the next 14 days 
predict_data = data.frame(x = c(seq(194, 207)))
predictions = data.table(predict(linear_model5, newdata = predict_data, 
                                 interval = 'confidence'))

# add the predictions to the overall data set
predictions = cbind(predictions, predict_data)
setnames(predictions, c('deaths', 'lwr', 'upr', 'days'))

# bind them to the other data 
setnames(death_data, c('days', 'deaths'))
death_data[ , lwr:=NA]
death_data[ , upr:=NA]

# bind in the predictions
predictions = rbind(predictions, death_data)

# plot it all, including the predicted values and confidence intervals
ggplot(predictions, aes(x=days, y=deaths))+
  geom_line()+
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill = '#fdd0a2'), alpha=0.4)+
  xlab("Number of Days")+
  ylab("Cumulative Deaths")+
  theme_bw()+ 
  guides(fill="none")+
  theme(text=element_text(size=18))

# ------------------------

