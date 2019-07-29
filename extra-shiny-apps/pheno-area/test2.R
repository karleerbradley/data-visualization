library(tidyverse)
library(dplyr, quietly=T)
library(lubridate)
library(neonUtilities)
library(downloader)
library(ggplot2)

siteO <- "HARV"
yr <- c("2015", "2016", "2017")
sort(yr)

# loading data from API
dpid <- as.character('DP1.10055.001')
# observeEvent(input$goplot,{
phenoLoad <- loadByProduct(dpID=dpid, site=siteO, package="basic", check.size = FALSE, 
                           startdate = c(paste(yr[1],"-01", sep = "")), enddate = c(paste(tail(yr, n=1), "-12", sep = "")))

# cleaning the data:
ind <- phenoLoad$phe_perindividual
status <- phenoLoad$phe_statusintensity

#Format dates
status$date <- as.Date(status$date, "%Y-%m-%d")
status$editedDate <- as.Date(status$editedDate, "%Y-%m-%d")

### dayOfYear may be populated, if null, assign
status$dayOfYear <- yday(status$date)

status$year <- substr(status$date, 1, 4)
status$monthDay <- format(status$date, format="%m-%d")

#remove duplicate records
status <- select(status, -uid)

status <- distinct(status)

## the webui created duplicates for every record edit submitted, this cleans those duplicates 
si_last <- status %>%
  group_by(individualID, date, phenophaseName) %>%
  filter(editedDate==max(editedDate))

### join perindividual and statusintensity tables

## subset to just the fields of interest
ind <- select(ind, individualID, growthForm, taxonID, editedDate)
## de-dupe
ind <- distinct(ind)

ind_last <- ind %>%
  group_by(individualID) %>%
  filter(editedDate==max(as.character(editedDate)))

ind_last <- select(ind_last, -editedDate)

si_last <- left_join(si_last, ind_last)

### use this to subset to any set of variables you may want to plot 
# comment/uncomment the fields you'd like to filter on
df <- filter(si_last, 
             #siteID=='DEJU'
             year %in% yr
             & phenophaseStatus=='yes' 
             #& taxonID=='BEGL/BENA'
             & growthForm=='Deciduous broadleaf'
             #& phenophaseName%in%c('Breaking leaf buds', 'Initial growth', 'Open flowers', 'Colored leaves') 
             #& phenophaseIntensity !=""
)

# adding common names
df <- mutate(df, commonName = ifelse(taxonID %in% "ACRU", "Red Maple", 
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

# counting total individuals by day for each commonName
total <- df %>%
  group_by(commonName) %>%
  count(date)
# renaming counted column to "total"
names(total)[which(names(total) == "n")] <- "total"
# joining total dateframe to df
df <- left_join(df, total, by = c("date", "commonName"))

# counting total individuals by day for each phenophase for each commonName
phenos <- df %>%
  group_by(commonName, phenophaseName) %>%
  count(date)
names(phenos)[which(names(phenos)=="n")] <- "number"
df <- left_join(df, phenos, by = c("date", "commonName", "phenophaseName"))

# calculating percentage by dividing count of each phenophase by total count
df$percent <- ((df$number)/df$total)*100




df$phenophaseName <- factor(df$phenophaseName, levels = c("Leaves", "Colored leaves", "Falling leaves",   "Young leaves", "Young needles", "Increasing leaf size","Breaking leaf buds", "Breaking needle buds", "Open flowers", "Open pollen cones"))

ggplot(df, aes(x = dayOfYear, y=percent, fill = phenophaseName, color = phenophaseName #,group = 1,
                       # text = paste("</br>Date:",monthDay, "</br>Percent:",percent, "</br>Phenophase:", phenophaseName)
                       )) +
  #geom_density(alpha=0.3, position = "stack",inherit.aes = FALSE) +
  geom_density(alpha=0.3,stat = "identity", position = position_dodge(width = .5)) +
  #geom_density() +
  theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
  xlab("Day Of Year") + ylab("Percentage of Individuals\nin Each Phenophase") + xlim(0,366) +
  ggtitle("Plant Phenophase Density for Selected Site") +
  theme(plot.title = element_text(lineheight = 1, face = "bold", size = 20, hjust = 0.5),
        axis.title = element_text(lineheight = .8, size = 15),
        legend.title = element_text(lineheight = .8, size = 20),
        legend.text = element_text(size = 15), 
        axis.text = element_text(size = 10),
        strip.text.x = element_text(size = 10) , strip.text.y = element_text(size = 10)) +
  scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase")
