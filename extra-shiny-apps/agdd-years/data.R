library(lubridate)
library(dplyr)


# cleaning data for the agdd-years app

years_data <- df_2%>%
  group_by(date, siteID)%>%
  #filter(year(date)=="2017") %>% # added the year here so that AGDDs starts from beginning of year
  arrange(date) %>% # had to add this so that the dates were in order
  select(siteID, date, verticalPosition, minTempF, maxTempF) %>%
  mutate(dayMax=max(maxTempF), dayMin=min(minTempF)) %>% 
  select(siteID, date, dayMax, dayMin)%>%
  distinct() # only keeps one row for each date with the max, min, and mean


# calculating the mean for max and mins of each day
years_data$dayMean <- (years_data$dayMax + years_data$dayMin)/2


#caluculate daily GDD
# base temp is 50F
# if mean2-50<0, then the number of GDDs is 0
# if mean2-50>0, then it rounds the value to the nearest whole number, and that's the number of GDDs for that date
years_data$GDD <- ifelse(years_data$dayMean-50 < 0, 0, round(years_data$dayMean-50, 0))

# adding useful columns to dataframe for date analysis
years_data$dayOfYear <- yday(years_data$date)
years_data$year <- substr(years_data$date, 1, 4)
years_data$monthDay <- format.Date(years_data$date, format="%B %d")


# ***** this was the original code, but when a new year began, it still added AGDDs instead of starting
# ***** back at 0 for the beginning of a new year
#calculate Accumlated GDD
# uses the sumr.2 function to add the GDDs together and puts it in AGDD
#years_data <- years_data %>%
    #group_by(siteID) %>%
   # mutate(AGDD = sumr.2(x=GDD))
#years_data <- ungroup(years_data)
# **************



# new code that starts AGDD over at 0 at the beginning of a new year
require(data.table)
years_data <- data.table(years_data)
years_data[, AGDD := cumsum(GDD), by=list(year, siteID)]


# saving the data as a new file so that it can be read in the app.R script
write.csv(years_data, file = "years_data.csv", row.names = FALSE)


