library(ggplot2)
library(dplyr)
library(lubridate)
library(neonUtilities)

# set working directory
setwd("~/Documents/data-for-proj")

# stack the files in the plant phenology data
stackByTable(filepath = "/Users/kbradlee/Documents/data-for-proj/NEON_obs-phenology-plant.zip")

# read in the phe_perindividual data
ind <- read.csv("/Users/kbradlee/Documents/data-for-proj/NEON_obs-phenology-plant/stackedFiles/phe_perindividual.csv", 
                stringsAsFactors = FALSE)

# read in the phe_statusintensity data
status <- read.csv("/Users/kbradlee/Documents/data-for-proj/NEON_obs-phenology-plant/stackedFiles/phe_statusintensity.csv", 
                stringsAsFactors = FALSE)

# remove the uid columns for both the ind and the status data
ind <- select(ind, -uid)
status <- select(status, -uid)

# remove duplicate rows in the ind data and the status data
ind_noD <- distinct(ind)
status_noD <- distinct(status)

# rename variables in the status object to have "Stat" at the end of the variable name before joining the datasets
status_noD <- rename(status_noD, editedDateStat = editedDate, measuredByStat = measuredBy, recordedByStat = recordedBy,
                   samplingProtocolVersionStat = samplingProtocolVersion, remarksStat = remarks, dataQFStat = dataQF)

# convert the date columns from character class to date class for ind and status
ind_noD$editedDate <- as.Date(ind_noD$editedDate)
str(ind_noD$editedDate)
status_noD$date <- as.Date(status_noD$date)
str(status_noD$date)


############### https://www.neonscience.org/osis-pheno-temp-series on retain only the max of the date