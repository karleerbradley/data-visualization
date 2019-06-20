library(dplyr)
library(ggplot2)
library(lubridate)

# set working directory
setwd("~/Documents/data-for-proj")

# load the phe_ind data
harv <- load("~/Documents/data-for-proj/phe_ind.RData")


# select site of interest
siteOfInterest <- "HARV"
harv <- filter(phe_ind, siteID %in% siteOfInterest)

unique(harv$taxonID)
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
  group_by(date) %>%
  count(phenophaseStatus)
inStat <- full_join(sampSize, inStat, by = "date")
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




