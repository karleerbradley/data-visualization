# practice for both app

library(ggplot2)
library(gridExtra)
library(dplyr)
library(lubridate)
library(scales)
library(gtable)
library(grid)
library(geoNEON)

# load the data, both use the same data
DBL <- read.csv("area.csv", stringsAsFactors = FALSE)
temp_data <- read.csv("years.csv", stringsAsFactors = FALSE)


pheno <- DBL %>%
  filter(siteID %in% "CLBJ") %>%
  filter(year %in% c("2017", "2018"))

# look at the total individuals in phenophase status by day
phenoSamp <- pheno %>%
  group_by(siteID) %>%
  count(date)
phenoStat <- pheno %>%
  group_by(date, siteID, taxonID, phenophaseName, commonName) %>%
  count(phenophaseStatus) 
phenoStat <- full_join(phenoSamp, phenoStat, by = c("date", "siteID"))
ungroup(phenoStat)

# only look at the yes's
phenoStat_T <- filter(phenoStat, phenophaseStatus %in% "yes")

# plot the percentage of individuals in the leaves phenophase
# convert to percentage
phenoStat_T$percent <- ((phenoStat_T$n.y)/phenoStat_T$n.x)*100

phenoStat_T$dayOfYear <- yday(phenoStat_T$date)
phenoStat_T$year <- substr(phenoStat_T$date, 1, 4)
phenoStat_T$phenophaseName <- factor(phenoStat_T$phenophaseName, levels = c("Leaves", "Falling leaves", "Colored leaves",  "Increasing leaf size","Breaking leaf buds",  "Open flowers"))


phenoP <- ggplot(phenoStat_T, aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName)) +
  geom_density(alpha=0.3,stat = "identity", position = position_dodge(width = .1)) +
  #geom_density(alpha=0.3,stat = "identity", position = "stack") +  
  theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
  xlab("Day Of Year") + ylab("% of Individuals") + xlim(0,366) +
  ggtitle("Phenophase Density for Selected Site") +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase")
phenoP


site_select <- temp_data %>%
  filter(siteID %in% "CLBJ") %>%
  filter(year %in% c("2017", "2018"))

# plots the data with the different years as the different lines
yearsP <- ggplot(data=site_select, aes(x=dayOfYear, y=AGDD, color = as.factor(year))) +
  geom_path() + xlab("Day of Year") + ggtitle("Accumulated Growing Degree Days Across Years") +
  scale_color_brewer(palette = "Dark2") + ylab("AGGDs")  + xlim(c(0,366))+ theme_bw() +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5), 
        axis.title = element_text(lineheight = .5, size = 15),
        legend.title = element_text(lineheight = .5, size = 12)) + labs(color="Year") 
yearsP

grid.arrange(phenoP, yearsP)



siteO <- c("HARV") 
yr <- c("2017", "2018")
sort(yr)


dpid <- as.character('DP1.10055.001')

phenoLoad <- loadByProduct(dpID=dpid, site= siteO, package="basic", check.size = FALSE, 
              startdate = c(paste(yr[1],"-01", sep = "")), enddate = c(paste(tail(yr, n=1), "-12", sep = "")))
              
ind <- phenoLoad$phe_perindividual
status <- phenoLoad$phe_statusintensity

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
phe_ind$year <- substr(phe_ind$date, 1, 4)

# using geoNEON package to find locations
# adding latitude and longitude from geoNEON to phe_ind, and elevation
spatialOnly <- def.extr.geo.os(phe_ind, 'namedLocation', locOnly = T)
phe_ind$latitude <- spatialOnly$api.decimalLatitude[match(phe_ind$namedLocation, spatialOnly$data.locationName)]
phe_ind$longitude <- spatialOnly$api.decimalLongitude[match(phe_ind$namedLocation, spatialOnly$data.locationName)]
phe_ind$elevation <- spatialOnly$api.elevation[match(phe_ind$namedLocation, spatialOnly$data.locationName)]


pheno <- phe_ind %>%
  filter(growthForm=="Deciduous broadleaf")
pheno <- pheno %>%
  filter(siteID %in% site) %>%
  filter(year %in% yr)

pheno <- mutate(pheno, commonName = ifelse(taxonID %in% "ACRU", "Red Maple", 
                                       ifelse(taxonID %in% "QURU", "Northern Red Oak", 
                                              ifelse(taxonID %in% "ACPE", "Snakebark Maple", 
                                                     ifelse(taxonID %in% "FAGR", "American Beech",
                                                            ifelse(taxonID %in% "LITU", "Tulip Tree", 
                                                                   ifelse(taxonID %in% "LIST2", "Sweetgum",
                                                                          ifelse(taxonID %in% "LIBE3", "Northern Spicebush",
                                                                                 ifelse(taxonID %in% "POTR5", "Quaking Aspen",
                                                                                        ifelse(taxonID %in% "ACSA3", "Sugar Maple",
                                                                                               ifelse(taxonID %in% "COCO6", "Beaked Hazelnut",
                                                                                                      ifelse(taxonID %in% "SYOR", "Coralberry",
                                                                                                             ifelse(taxonID %in% "CAOV2", "Shagbark Hickory",
                                                                                                                    ifelse(taxonID %in% "CEOC", "Common Hackberry",
                                                                                                                           ifelse(taxonID %in% "QUMO4", "Chestnut Oak",
                                                                                                                                  ifelse(taxonID %in% "COFL2", "Flowering Dogwood",
                                                                                                                                         ifelse(taxonID %in% "QUMA3", "Blackjack Oak",
                                                                                                                                                ifelse(taxonID %in% "COCOC", "California Hazelnut",
                                                                                                                                                       ifelse(taxonID %in% "BEGL", "Resin Birch", "other")))))))))))))))))))

# look at the total individuals in phenophase status by day
phenoSamp <- pheno %>%
  group_by(siteID) %>%
  count(date)
phenoStat <- pheno %>%
  group_by(date, siteID, taxonID, phenophaseName, commonName) %>%
  count(phenophaseStatus) 
phenoStat <- full_join(phenoSamp, phenoStat, by = c("date", "siteID"))
ungroup(phenoStat)

# only look at the yes's
phenoStat_T <- filter(phenoStat, phenophaseStatus %in% "yes")

# plot the percentage of individuals in the leaves phenophase
# convert to percentage
phenoStat_T$percent <- ((phenoStat_T$n.y)/phenoStat_T$n.x)*100

phenoStat_T$dayOfYear <- yday(phenoStat_T$date)
phenoStat_T$year <- substr(phenoStat_T$date, 1, 4)
phenoStat_T$phenophaseName <- factor(phenoStat_T$phenophaseName, levels = c("Leaves", "Falling leaves", "Colored leaves",  "Increasing leaf size","Breaking leaf buds",  "Open flowers"))


dpid2 <- as.character('DP1.00002.001') 
loadByProduct(dpID=dpid2, site=siteO, package="basic", check.size = FALSE, 
              startdate = c(paste(yr[1],"-01", sep = "")), enddate = c(paste(tail(yr, n=1), "-12", sep = "")), avg = "30")


site_select <- temp_data %>%
  filter(siteID %in% site) %>%
  filter(year %in% yr)







phenoP <- ggplot(phenoStat_T, aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName)) +
  geom_density(alpha=0.3,stat = "identity", position = position_dodge(width = .1)) +
  #geom_density(alpha=0.3,stat = "identity", position = "stack") +  
  theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
  xlab("Day Of Year") + ylab("% of Individuals") + xlim(0,366) +
  ggtitle("Phenophase Density for Selected Site") +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase")
phenoP


