library(ggplot2)
library(dplyr)
library(lubridate)
library(neonUtilities)

# set working directory
setwd("~/Documents/data-for-proj")

# stack the files in the plant phenology data
# stackByTable(filepath = "/Users/kbradlee/Documents/data-for-proj/NEON_obs-phenology-plant.zip")

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
                   samplingProtocolVersionStat = samplingProtocolVersion, remarksStat = remarks, dataQFStat = dataQF,
                   dateStat = date)

# convert the date columns from character class to date class for ind and status
ind_noD$editedDate <- as.Date(ind_noD$editedDate)
str(ind_noD$editedDate)
status_noD$dateStat <- as.Date(status_noD$dateStat)
str(status_noD$date)


# retain only the latest editedDate for each individualID on ind and get rid of duplicate dates
ind_lastnoD <- ind_noD %>%
  group_by(individualID) %>%
  filter(editedDate == max(editedDate)) %>%
  group_by(editedDate, individualID) %>%
  filter(row_number() == 1)

###########
# join the two dataframes together into one table
# this will be the table that is used to then narrow sites and species and phenophases
phe_ind <- left_join(status_noD, ind_lastnoD)


# removing columns with mostly NAs that aren't needed
phe_ind <- select(phe_ind, -dayOfYear, -samplingProtocolVersionStat, -remarksStat, -dataQFStat)
phe_ind <- select(phe_ind, -geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -elevationUncertainty, -sampleLatitude, 
                  -sampleLongitude, -sampleCoordinateUncertainty, -sampleElevation, -sampleElevationUncertainty, 
                  -identificationQualifier, -vstTag, -dataQF)
# removing other columns that don't seem to be relevant
phe_ind <- select(phe_ind, -measuredByStat, -recordedByStat, -sampleGeodeticDatum, -samplingProtocolVersion,
                  -measuredBy, -identifiedBy, -recordedBy)

### phe_ind is now ready to be used to pull info about specific sites
#############