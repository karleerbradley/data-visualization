library(ggplot2)
library(dplyr)
library(lubridate)
library(neonUtilities)
library(devtools)
library(geoNEON)


#---------------------------------------------------------------------------------------

# PLANT PHENOLOGY DATA

# set working directory
setwd("~/Documents/data-for-proj")



# setting file paths for where the data is 
if (file.exists(
  'C:/Users/kbradlee')){
  myPathToGit <- "C:/Users/kbradlee/Desktop/data-visualization"
  myPathToData <- "/Users/kbradlee/Documents/data-for-proj"
}

# pull plant phenology data via api instead of on local directory
dpid <- as.character('DP1.10055.001')
zipsByProduct(dpID=dpid, site="all", package="basic")
stackByTable(paste0(getwd(), "/filesToStack10055"), folder=T)


ind <- read.csv(paste(myPathToData, "filesToStack10055/stackedFiles/phe_perindividual.csv", sep = "/"),  header = T)
status <- read.csv(paste(myPathToData, "filesToStack10055/stackedFiles/phe_statusintensity.csv", sep = "/"), header = T)


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
                   samplingProtocolVersionStat = samplingProtocolVersion, remarksStat = remarks, dataQFStat = dataQF)

# rename variables in ind so that it doesn't conflict later on when joining the dataframes
ind_noD <- rename(ind_noD, addDate = date)

# convert the date columns from character class to date class for ind and status
ind_noD$editedDate <- as.Date(ind_noD$editedDate)
status_noD$date <- as.Date(status_noD$date)



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



# adding other date columns for better date analysis
phe_ind$dayOfYear <- yday(phe_ind$date)
phe_ind$year <- substr(phe_ind$date, 1, 4)
phe_ind$monthDay <- format(phe_ind$date, format = "%m-%d")




# using geoNEON package to find locations
# adding latitude and longitude from geoNEON to phe_ind, and elevation
spatialOnly <- def.extr.geo.os(phe_ind, 'namedLocation', locOnly = T)
phe_ind$latitude <- spatialOnly$api.decimalLatitude[match(phe_ind$namedLocation, spatialOnly$data.locationName)]
phe_ind$longitude <- spatialOnly$api.decimalLongitude[match(phe_ind$namedLocation, spatialOnly$data.locationName)]
phe_ind$elevation <- spatialOnly$api.elevation[match(phe_ind$namedLocation, spatialOnly$data.locationName)]




# saving the phe_ind object in R
save(phe_ind, file = "phe_ind.RData")

### phe_ind is now ready to be used to pull info about specific sites
#############





#-----------------------------------------------------------------------------------------------------------------

# TOWER TEMPERATURE DATA


library(scales)
library(tidyr)

# stack the files in the single aspirated air temperature data for D01
# stackByTable(filepath = "/Users/kbradlee/Documents/data-for-proj/NEON_temp-air-single-D01.zip")

# read in the 30 minute temperature data 
temp30_D01 <- read.csv("NEON_temp-air-single-D01/stackedFiles/SAAT_30min.csv", stringsAsFactors = FALSE)

# check for NAs
sum(is.na(temp30_D01$tempSingleMean))
# remove the NAs from the dataframe
temp30_D01 <- temp30_D01 %>%
  drop_na(tempSingleMean)
# check to see if it worked
sum(is.na(temp30_D01$tempSingleMean))

# check format of the dates
str(temp30_D01$startDateTime)

# convert to date-time class 
temp30_D01$startDateTime <- as.POSIXct(temp30_D01$startDateTime,
      format = "%Y-%m-%dT%H:%M:%SZ", tz = "GMT")
# checking to see if it's in the right format
str(temp30_D01$startDateTime)

# convert to local time zone
# convert to local tz in new column
# D01 in Eastern Time Zone
temp30_D01$dtLocal <- format(temp30_D01$startDateTime, tz = "America/New_York", usetz = TRUE)
# check it
head(select(temp30_D01, startDateTime, dtLocal))

# saving the temperature data for D01 to be used 
save(temp30_D01, file = "temp30_D01.RData")
