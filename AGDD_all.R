library(tidyverse)
library(gtools)
library(httr)
library(jsonlite)
library(neonUtilities)
library(lubridate)


# setting file paths for where the data is 
if (file.exists(
  'C:/Users/kbradlee')){
  myPathToGit <- "C:/Users/kbradlee/Desktop/data-visualization"
  myPathToData <- "/Users/kbradlee/Documents/data-for-proj"
  }
  


# pulling the air temp data via api
dpid <- as.character('DP1.00002.001')  ##single aspirated air temperature

#zipsByProduct(dpID=dpid, site="HARV", package="basic", avg = "30")

#stackByTable(paste0(myPathToData, "/filesToStack00002"), folder=T) # temp data





# load the temp data
SAAT <- read.csv(paste(myPathToData, "filesToStack00002/stackedFiles/SAAT_30min.csv", sep="/"), 
                 stringsAsFactors = F, header = T)
df <- SAAT




# format data
# define function to convert temp c to f 
c_to_f <- function(x)  (x * 1.8 + 32)


# remove NAs
df <- filter(df, !is.na(tempSingleMean))

# convert df mean temps from c to f
df$meanTempF <- c_to_f(df$tempSingleMean)


#pull date value from dateTime
# pulls the first 10 indices from endDateTime (yyyy-mm-dd) and creates a new column with just the date
df$date <- substr(df$endDateTime, 1, 10)

#create new dataframe with daily values
# groups by date, and for each date, it takes the max mean and makes that the max, the min mean and makes that the min, and the
# mean mean and makes that the mean
day_temp <- df%>%
  group_by(date)%>%
  filter(year(date)=="2017") %>% # added the year here so that AGDDs starts from beginning of year
  arrange(date) %>% # had to add this so that the dates were in order
  select(siteID, date, verticalPosition, meanTempF)%>%
  mutate(dayMax=max(meanTempF), dayMin=min(meanTempF), dayMean=mean(meanTempF))%>% 
  select(siteID, date, dayMax, dayMin, dayMean)%>%
  distinct() # only keeps one row for each date with the max, min, and mean


##alt mean, consistent with GDD calculations 
### would be interesting to see how different the accumulation curves looks for true mean vs. simplified mean
# takes the min mean and max mean and uses them to find mean2 by averaging them
day_temp$mean2 <- (day_temp$dayMin + day_temp$dayMax)/2


#caluculate daily GDD
# base temp is 50F
# if mean2-50<0, then the number of GDDs is 0
# if mean2-50>0, then it rounds the value to the nearest whole number, and that's the number of GDDs for that date
day_temp$GDD <- ifelse(day_temp$mean2-50 < 0, 0, round(day_temp$mean2-50, 0))


#function to add daily GDD values
sumr.2 <- function(x) {
  sapply(1:length(x), function(i) sum(x[1:i]))
}



#calculate Accumlated GDD
# uses the sumr.2 function to add the GDDs together and puts it in AGDD
day_temp$AGDD <- sumr.2(x=day_temp$GDD)

day_temp <- ungroup(day_temp)

# adding useful columns to dataframe for date analysis
day_temp$dayOfYear <- yday(day_temp$date)
day_temp$year <- substr(day_temp$date, 1, 4)
day_temp$monthDay <- format(day_temp$date, format = "%m-%d")


# plot AGGD data
# define year of interest
#year <- 2017

# subset data to specified year
# don't need this part anymore because filtered out date above
# data <- filter(day_temp, date >= paste(year, '01-01', sep='-') & date <= paste(year, '12-31', sep='-'))

#plot it
# calculates AGDDs from the beginning of data collection and not just for the specified year
# now plots from beginning of year
AGDD_2017 <- ggplot(data=day_temp, aes(x=dayOfYear, y=AGDD, group = 1)) +
  geom_path() + xlab("Day of Year") + ggtitle("Aggregated Growing Degree Days in 2017")
AGDD_2017
# curve is weird at the end because the dates aren't all in order
# now dates are in order after adding arrange(date) above





# zooming in on the graph so easier to compare with DBL_P_2017
AGDD_2017 <- ggplot(data=day_temp, aes(x=dayOfYear, y=AGDD, group = 1)) +
  geom_path() + xlab("Day of Year") + ggtitle("Aggregated Growing Degree Days in 2017") + xlim(c(100, 350))
AGDD_2017
# putting the graphs together to compare in HARV.R
