# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/12/2023
# Examining the Data Lake elements
# searching for PrEP indicators
# ----------------------------------------------

# ------------------------
# load packages

rm(list=ls()) # clear the workspace
library(readxl)
library(data.table)
library(lubridate)
library(plyr)
library(tidyr)
library(zoo)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(openxlsx)
# ------------------------

# ----------------------------------------------
# DIRECTORIES AND IMPORT
# ----------------------------------------------
# ------------------------
# set working directories 
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/all/data_lake_index/2023/'

# output directory
outDir = paste0(dir, 'prepped/')
# ------------------------

# ------------------------
# import the file
dt = data.table(read.xlsx(paste0(dir, 'raw/data_elements_7_2023.xlsx')))

# rename the first line for ease of searching
setnames(dt, c('var'))

# drop the last row "Grand Total"
dt = head(dt, -1)

# ------------------------

# ----------------------------------------------
# FORMATTING THE DATA
# ----------------------------------------------

# use the maximum numbers of characters to find specific variable values
dt[ , max:=nchar(var)]
dt[grepl("Total", var) & max==5] # no other totals in the data set

# add a column for orgUnitID
dt[max==11, element_id:=var]

# repeat the element id and drop the extra lines
dt = dt %>% fill(element_id, .direction = "up")
dt = dt[var!=element_id]

# add the countries by elements
dt[grepl('Cameroon', var) & max==8, country:='Cameroon']
dt[grepl('CDI', var) & max==3, country:='CDI']
dt[grepl('DRC', var) & max==3, country:='DRC']
dt[grepl('Kenya', var) & max==5, country:='Kenya']
dt[grepl('Lesotho', var) & max==7, country:='Lesotho']

dt[grepl('Malawi', var) & max==6, country:='Malawi']
dt[grepl('Mozambique', var) & max==10, country:='Mozambique']
dt[grepl('Tanzania', var) & max==8, country:='Tanzania']

# fill in the value with the preceding value
dt = dt %>% fill(country, .direction = "down")

# if the country is the same as the variable, drop it
dt = dt[var!=country]

# format and drop max variable
dt = dt[,.(country, element = var, element_id)]

# ----------------------------------------------
# CATALOGUE THE DATA
# ----------------------------------------------
# use to view variables by type
View(dt[grepl("ahd", tolower(element)) | grepl("advanced", tolower(element))])

# search for prep related variables
dt[grepl("prep", tolower(element)), prep:=TRUE]
dt[is.na(prep), prep:=FALSE]

# search for pmtct variables
dt[grepl("pmtct", tolower(element)) | grepl("ptme", tolower(element)), pmtct:=TRUE]
dt[is.na(pmtct), pmtct:=FALSE]

# search for tb variables
dt[grepl("ahd", tolower(element)), tb:=TRUE]
dt[is.na(tb), tb:=FALSE]

# search for tb variables
dt[grepl("tb", tolower(element)), tb:=TRUE]
dt[is.na(tb), tb:=FALSE]


# ----------------------------------------------
# EXPORT THE FILE
# ----------------------------------------------

# ---------------------------
# set the names for the table of contents export
dt1 = copy(dt)
setnames(dt1, c('Country', 'Data Element', 'DHIS2 Element ID', 'PrEP',
                'PMTCT', 'TB'))

# create a header style that looks pretty
hs = createStyle(fontColour = "#000000", fgFill = "#deebf7",
                 halign = "center", valign = "center", textDecoration = "Bold",
                 border = "TopBottomLeftRight")

# export the data to the prepped folder
write.xlsx(dt1, paste0(outDir, 'table_of_contents.xlsx'),
           sheetName = "Data Lake Elements", headerStyle = hs)

# ---------------------------

