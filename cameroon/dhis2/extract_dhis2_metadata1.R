# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/19/2022
# Cameroon API Model - DHIS2 aggregate data
# Download all of the meta data using an API call
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls()) # clear the workspace
library(readxl)
library(openxlsx)
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(RJSONIO)
# --------------------

# --------------------
# Files and directories

# set the working directory to the location of the raw data
dir = 'C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/data/cameroon/dhis2/'
setwd(dir)

# set the output directory
outDir = paste0(dir, 'raw/')

# --------------------
# load the function that fixes diacritical marks
source('C:/Users/ccarelli/OneDrive - E Glaser Ped AIDS Fdtn/Documents/GitHub/sie/all/fix_diacritics_function.R')

# --------------------
# list of API links

# table of contents - all API links
'https://cmrdhis.eastus.cloudapp.azure.com/pldcare/api/resources'

# specific org unit example 
'https://cmrdhis.eastus.cloudapp.azure.com/pldcare/api/organisationUnits/ME6mMdBOq6r'

# for an example of all fields
'https://cmrdhis.eastus.cloudapp.azure.com/pldcare/api/dataSets.json?fields=:all'

# --------------------
# links for downloading data - includes all essential attributes

# full list of org units - does not include ancestors
org_url = "https://Caitlin:User001*@cmrdhis.eastus.cloudapp.azure.com/pldcare/api/organisationUnits.json?includeDescendants=true&paging=false&fields=id,name,parent,level,geometry"

sets_url = 'https://Caitlin:User001*@cmrdhis.eastus.cloudapp.azure.com/pldcare/api/dataSets.json?includeDescendants=true&paging=false&fields=:all'

elements_url = 'https://Caitlin:User001*@cmrdhis.eastus.cloudapp.azure.com/pldcare/api/dataElements.json?includeDescendants=true&paging=false&fields=:all'

cat_combo_url = 'https://Caitlin:User001*@cmrdhis.eastus.cloudapp.azure.com/pldcare/api/categoryCombos?includeDescendants=true&paging=false&fields=:all'

cat_options_url = 'https://Caitlin:User001*@cmrdhis.eastus.cloudapp.azure.com/pldcare/api/categoryOptions?includeDescendants=true&paging=false'

# -----------------------------------------
# DOWNLOAD THE FILES
# -----------------------------------------

# -------------------------
# download the list of organisational units
download.file(org_url, 
              destfile = paste0(dir, 'raw/organisationUnits.json'))

# download the list of data sets
# download.file(sets_url, 
#               destfile = paste0(dir, 'raw/data_sets.json'))
# 
# # data elements/program data elements
# download.file(elements_url, 
#               destfile = paste0(dir, 'raw/data_elements.json'))
# 
# # categories and their associated options 
# download.file(cat_combo_url, 
#               destfile = paste0(dir, 'raw/categories.json'))
# download.file(cat_options_url, 
#               destfile = paste0(dir, 'raw/category_options.json'))

# indicator group sets/indicators 

# attributes

# -------------------------

# -----------------------------------------
# DE-LIST THE FILES TO READY FOR PREP
# -----------------------------------------

# --------------------------
# CREATE A SET OF ORG UNITS

# de-soup the file from the json 
orgUnitList = jsonlite::fromJSON(paste0(dir, 'raw/organisationUnits.json'))

# unlist the list/parse the file
org_units = data.table(cbind(orgUnitID = orgUnitList$organisationUnits$id,
                             orgUnit = orgUnitList$organisationUnits$name,
                             level = orgUnitList$organisationUnits$level,
                             parentID = orgUnitList$organisationUnits$parent$id))

# four org units are duplicated with no associated name
org_units = org_units[!(orgUnit=='.' | orgUnit=='-')]

# save the data table (subset of the full json extraction)
saveRDS(org_units, paste0(dir, 'raw/organisationUnits.rds'))
# --------------------------

# --------------------------
# CREATE A SET OF DATA SETS

# de-soup the file from the json 
DataSetList = jsonlite::fromJSON(paste0(dir, 'raw/data_sets.json'))

# unlist the list/parse the file
# can add the href if you want the set location and associated information
data_sets = data.table(cbind(name = DataSetList$dataSets$name,
                             shortName = DataSetList$dataSets$shortName,
                             id = DataSetList$dataSets$id,
                             lastUpdated = DataSetList$dataSets$lastUpdated,
                             displayFormName = DataSetList$dataSets$displayFormName,
                             displayName = DataSetList$dataSets$displayName ))

# drop the CM before the short names
data_sets = data_sets[ ,shortName:=gsub('CM.', '', shortName)]

# save the data table (subset of the full json extraction)
saveRDS(data_sets, paste0(dir, 'raw/data_sets.rds'))
# --------------------------

# --------------------------
# CREATE A SET OF DATA ELEMENTS 

# de-soup the file from the json 
DataElementList = jsonlite::fromJSON(paste0(dir, 'raw/data_elements.json'))

# unlist the list/parse the file
data_elements = data.table(cbind(name = DataElementList$dataElements$name, 
                                 shortName = DataElementList$dataElements$shortName,
                                 displayName = DataElementList$dataElements$displayName, 
                                 displayShortName = DataElementList$dataElements$displayShortName, 
                                 element_id = DataElementList$dataElements$id,
                                 lastUpdated = DataElementList$dataElements$lastUpdated))


# save the data table (subset of the full json extraction)
saveRDS(data_elements, paste0(dir, 'raw/data_elements.rds'))
write.csv(data_elements, paste0(dir, 'raw/data_elements.csv'))
# --------------------------

# --------------------------
# CREATE A LINK BETWEEN ELEMENTS AND SETS


# create a list of the data sets associated with each element
data_elements_sets = data.table(cbind(name = DataSetList$dataSets$name,
                                      set_id = DataSetList$dataSets$id,
                                      element_id = unlist(DataSetList$dataSets$dataSetElements)))


# all of the data set ids are in the element set ids 
data_sets$id %in% data_elements_sets$set_id

data_elements$element_id %in% data_elements_sets$element_id

data_elements_sets$element_id %in% data_elements$element_id


# merge the data 




# --------------------------
# CREATE A SET OF CATEGORIES AND THEIR OPTIONAL RESPONSES

# de-soup the file from the json 
CategoryList = jsonlite::fromJSON(paste0(dir, 'raw/categories.json'))

# create a data table showing the sets of category option combos
cats = data.table(cbind(code = CategoryList$categoryCombos$code,
                        displayName = CategoryList$categoryCombos$displayName,
                        set_id = CategoryList$categoryCombos$id,
                        name = CategoryList$categoryCombos$name,
                        dimensionType = str_to_title(CategoryList$categoryCombos$dataDimensionType)))

# --------------
# loop through and get all of the categories

i = 1
for (c in seq(1:length(CategoryList$categoryCombos$categories))) {
  
  cat = data.table(CategoryList$categoryCombos$categories[[c]])
  cat[ , option:=c]
  if(i==1) full_data = cat
  if(1 < i) full_data = rbind(full_data, cat)
  
  i = i+1
}
# --------------

# --------------
# merge the categories in with their option sets

# number the lines of the data table in order
cats[ , option:=1:nrow(cats)]

# merge on the option 
cats = merge(full_data, cats, by = 'option')
# --------------

# --------------
# merge in the names of the categories based on their id 

# de-soup the file from the json 
CategoryOptionsList = jsonlite::fromJSON(paste0(dir, 'raw/category_options.json'))

# create a table of the options
opts = data.table(cbind(id = CategoryOptionsList$categoryOptions$id,
                        name = CategoryOptionsList$categoryOptions$displayName))


# -------------------------
# save the data table (subset of the full json extraction)
saveRDS(cats, paste0(dir, 'raw/categories.rds'))
write.csv(cats, paste0(dir, 'raw/categories.csv'))
# -------------------------

# --------------------------

