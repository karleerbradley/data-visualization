library(ggplot2)
library(dplyr)
library(lubridate)
library(neonUtilities)
library(devtools)
library(geoNEON)
library(gridExtra)
library(tidyverse)
library(gtools)
library(httr)
library(jsonlite)
library(RColorBrewer)

# setting file paths for where the data will be and where my repo is 
#if (file.exists(
#'C:/Users/kbradlee')){
myPathToGit <- "C:/Users/kbradlee/Desktop/data-visualization"
myPathToData <- "/Users/kbradlee/Desktop/data-visualization"
#}


# pulling the air temp data via api
dpid <- as.character('DP1.00002.001')  ##single aspirated air temperature
sites <- c("HARV", "SERC", "UNDE", "UKFS", "ORNL", "CLBJ", "ABBY", "TOOL", "BONA", "BART")


#zipsByProduct(dpID=dpid, site=sites, package="basic", avg = "30")
#stackByTable(paste0(myPathToData, "/filesToStack00002"), folder=T)



# load the temp data
SAAT <- read.csv(paste(myPathToData, "filesToStack00002/stackedFiles/SAAT_30min.csv", sep="/"), 
                 stringsAsFactors = F, header = T)
df <- SAAT

# pulling the air temp data via api
dpid <- as.character('DP1.00002.001')  ##single aspirated air temperature
sites <- c("HARV", "SERC", "UNDE", "UKFS", "ORNL", "CLBJ", "ABBY", "TOOL", "BONA", "BART")


#zipsByProduct(dpID=dpid, site=sites, package="basic", avg = "30")
#stackByTable(paste0(myPathToData, "/filesToStack00002"), folder=T)



# load the temp data
SAAT <- read.csv(paste(myPathToData, "filesToStack00002/stackedFiles/SAAT_30min.csv", sep="/"), 
                 stringsAsFactors = F, header = T)
df_2 <- filter(SAAT, !is.na(tempSingleMinimum), !is.na(tempSingleMaximum))

# convert df min temps and max temps from c to f
df_2$minTempF <- c_to_f(df_2$tempSingleMinimum)
df_2$maxTempF <- c_to_f(df_2$tempSingleMaximum)

#pull date value from dateTime
# pulls the first 10 indices from endDateTime (yyyy-mm-dd) and creates a new column with just the date
df_2$date <- substr(df_2$endDateTime, 1, 10)


#create new dataframe with daily values
# groups by date, and for each date, it takes the max mean and makes that the max, the min mean and makes that the min, and the
# mean mean and makes that the mean
day_temp_2 <- df_2%>%
  group_by(date, siteID)%>%
  filter(year(date)=="2017") %>% # added the year here so that AGDDs starts from beginning of year
  arrange(date) %>% # had to add this so that the dates were in order
  select(siteID, date, verticalPosition, minTempF, maxTempF) %>%
  mutate(dayMax=max(maxTempF), dayMin=min(minTempF)) %>% 
  select(siteID, date, dayMax, dayMin)%>%
  distinct() # only keeps one row for each date with the max, min, and mean


# calculating the mean for max and mins of each day
day_temp_2$dayMean <- (day_temp_2$dayMax + day_temp_2$dayMin)/2


#caluculate daily GDD
# base temp is 50F
# if mean2-50<0, then the number of GDDs is 0
# if mean2-50>0, then it rounds the value to the nearest whole number, and that's the number of GDDs for that date
day_temp_2$GDD <- ifelse(day_temp_2$dayMean-50 < 0, 0, round(day_temp_2$dayMean-50, 0))

#calculate Accumlated GDD
# uses the sumr.2 function to add the GDDs together and puts it in AGDD
day_temp_2 <- day_temp_2 %>%
  group_by(siteID) %>%
  mutate(AGDD = sumr.2(x=GDD))

day_temp_2 <- ungroup(day_temp_2)

# adding useful columns to dataframe for date analysis
day_temp_2$dayOfYear <- yday(day_temp_2$date)
day_temp_2$year <- substr(day_temp_2$date, 1, 4)
day_temp_2$monthDay <- format(day_temp_2$date, format = "%m-%d")


