library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
# set working directory
setwd("~/Documents/data-for-proj")

# load the phe_ind data
harv <- load("~/Documents/data-for-proj/phe_ind.RData")


# select site of interest
siteOfInterest <- "HARV"
harv <- filter(phe_ind, siteID %in% siteOfInterest)



# select growth form of interest
growthFormOfInterest <- "Deciduous broadleaf"

# select species of interest
# selecting the DBL species
speciesOfInterest <- c("QURU", "ACRU")


# make a subset to just the two species we want
harv <- filter(harv, taxonID %in% speciesOfInterest)


# check which phenophases are available to look at
unique(harv$phenophaseName)
# select phenophase of interest
# want to look at the leaves phenophase
phenophaseOfInterest <- "Leaves"

# subset of just the phenophase of interest
harv <- filter(harv, phenophaseName %in% phenophaseOfInterest)
unique(harv$phenophaseName)


# look at the total individuals in leaves status by day
sampSize <- count(harv, date)
inStat <- harv %>%
  group_by(date, taxonID) %>%
  count(phenophaseStatus) 
inStat <- full_join(sampSize, inStat, by = "date")

ungroup(inStat)


# only look at the yes's
inStat_T <- filter(inStat, phenophaseStatus %in% "yes")


# plot of the number of individuals in the leaves phenophase
phenoPlot <- ggplot(inStat_T, aes(date, n.y)) +
  geom_bar(stat = "identity", na.rm = TRUE) +
  ggtitle("Total Individuals in Leaf") +
  xlab("Date") + ylab("Number of Individuals") +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20)) +
  theme(text = element_text(size = 18))
phenoPlot



# plot the percentage of individuals in the leaves phenophase
# convert to percentage
inStat_T$percent <- ((inStat_T$n.y)/inStat_T$n.x)*100
# plot
phenoPlot_P <- ggplot(inStat_T, aes(date, percent)) +
  geom_bar(stat = "identity", na.rm = TRUE) +
  ggtitle("Percentage in the Leaves Phenophase") +
  xlab("Date") + ylab("% of Individuals") +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20)) +
  theme(text = element_text(size = 18))
phenoPlot_P



# plot percentage in just 2017
startTime <- as.Date("2017-01-01")
endTime <- as.Date("2017-12-31")
start.end <- c(startTime, endTime)
# plot
leaves17 <- ggplot(inStat_T, aes(date, percent)) +
  geom_bar(stat = "identity", na.rm = TRUE) +
  ggtitle("Total Individuals in Leaf in 2017") +
  xlab("Date") + ylab("% of Individuals") +
  (scale_x_date(limits = start.end, date_breaks = "1 month", date_labels = "%b")) +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20)) +
  theme(text = element_text(size = 18))
leaves17


# plot percentage in just 2016
startTime16 <- as.Date("2016-01-01")
endTime16 <- as.Date("2016-12-31")
start16.end16 <- c(startTime16, endTime16)
#plot
leaves16 <- ggplot(inStat_T, aes(date, percent)) +
  geom_bar(stat = "identity", na.rm = TRUE) +
  ggtitle("Total Individuals in Leaf in 2016") +
  xlab("Date") + ylab("% of Individuals") +
  (scale_x_date(limits = start16.end16, date_breaks = "1 month", date_labels = "%b")) +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20)) +
  theme(text = element_text(size = 18))
leaves16



# plot % leaves in 2016 and 2017 together
grid.arrange(leaves16, leaves17)


# adding the dayOfYear column to inStat_T so it can be used in graphs
inStat_T$dayOfYear <- yday(inStat_T$date)


# making another dataframe that only has info from 2017
inStat_T_17 <- inStat_T %>%
  filter(year(date)=="2017")




# bar graph that shows how much of 100% is ACRU and how much is QURU for each day of the year
# can see that for some days, both of them aren't in leaves -> ACRU looks like it changes faster than QURU
DBL_P_2017 <- ggplot(data = inStat_T_17, aes(x=dayOfYear, y=percent, group = 1, color = taxonID)) +
  geom_col() + xlab("Day of Year") + ylab("% of DBL") +
  ggtitle("Percent of Deciduous Broadleaf Species in Leaf in 2017") +
  labs(color = "Species") + theme(legend.position = "top") + xlim(c(0,365))
DBL_P_2017




# putting the % in Leaf and AGDD together
grid.arrange(DBL_P_2017, AGDD_2017)


# zooming on on graph so that it's easier to compare with AGDD_2017
DBL_P_2017 <- ggplot(data = inStat_T_17, aes(x=dayOfYear, y=percent, group = 1, color = taxonID)) +
  geom_col() + xlab("Day of Year") + ylab("% of DBL") +
  ggtitle("Percent of Deciduous Broadleaf Species in Leaf in 2017") +
  labs(color = "Species") + theme(legend.position = "top") + xlim(c(100,350))
DBL_P_2017




# putting the % in Leaf and AGDD together after zoomed in
grid.arrange(DBL_P_2017, AGDD_2017)

# putting the % in Leaf and AGDD_2 together after zoomed in
grid.arrange(DBL_P_2017, AGDD_2017_2)


  
