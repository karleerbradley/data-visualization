## Author: Katie Jones 

library(tidyverse)
library(dplyr, quietly=T)

library(lubridate)

##pull all available data for a site from the NEON API

library(neonUtilities)
library(downloader)

# # Pull data as zip files from portal, save to working directory
dpid <- as.character('DP1.10055.001')
zipsByProduct(dpID='DP1.10055.001', site="DEJU", package="basic") # Set to your site
y### add savepath = "C:/...." to save to a different location

### merge all site/month files from zips to one file per table
stackByTable(dpid, filepath=paste0(getwd(), "/filesToStack10055"), 
             savepath = paste0(getwd(), "/filesToStack10055"), folder=T)  

## load tag, taxon, location table
perind <- read.csv(paste0(getwd(), '/filesToStack10055/stackedFiles/phe_perindividual.csv'),
                            stringsAsFactors = F, header = T)

## load observations
statusIntensity <- read.csv(paste0(getwd(), '/filesToStack10055/stackedFiles/phe_statusintensity.csv'),
                                      stringsAsFactors = F, header = T)

#Format dates
statusIntensity$date <- as.Date(statusIntensity$date, "%Y-%m-%d")
statusIntensity$editedDate <- as.Date(statusIntensity$editedDate, "%Y-%m-%d")

### dayOfYear may be populated, if null, assign
statusIntensity$dayOfYear <- yday(statusIntensity$date)

statusIntensity$year <- substr(statusIntensity$date, 1, 4)
statusIntensity$monthDay <- format(statusIntensity$date, format="%m-%d")

#remove duplicate records
statusIntensity <- select(statusIntensity, -uid)

statusIntensity <- distinct(statusIntensity)

## the webui created duplicates for every record edit submitted, this cleans those duplicates 
si_last <- statusIntensity %>%
  group_by(individualID, date, phenophaseName) %>%
  filter(editedDate==max(editedDate))

### remove first year of data from each site ### Typically partial season
## optional ##
# si_last <- si_last %>%
#   group_by(siteID) %>%
#   filter(year!=min(year))


### join perindividual and statusintensity tables

## subset to just the fields of interest
perind <- select(perind, individualID, growthForm, taxonID, editedDate)
## de-dupe
perind <- distinct(perind)

ind_last <- perind %>%
  group_by(individualID) %>%
  filter(editedDate==max(editedDate))

ind_last <- select(ind_last, -editedDate)

si_last <- left_join(si_last, ind_last)


### Plot Data ##

## details about ggplot2 options here: 
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

### use this to subset to any set of variables you may want to plot 
# comment/uncomment the fields you'd like to filter on
df <- filter(si_last, 
             #siteID=='DEJU'
              year=='2017'
             & phenophaseStatus=='yes' 
             & taxonID=='BEGL/BENA'
             #& growthForm=='Deciduous broadleaf'
             #& phenophaseName%in%c('Breaking leaf buds', 'Initial growth', 'Open flowers', 'Colored leaves') 
             #& phenophaseIntensity !=""
)


# reorganize levels to optimize plot - by placing 'leaves' first in order, this send this to the back of the plot. 
# otherwise, may layer in front of and obscure other phenophases.

df$phenophaseName <- factor(df$phenophaseName, levels = c("Leaves", "Colored leaves", "Falling leaves",   "Young leaves", "Young needles", "Increasing leaf size","Breaking leaf buds", "Breaking needle buds", "Open flowers", "Open pollen cones"))

df$phenophaseIntensity <- factor(df$phenophaseIntensity, levels = c("< 5%", "5-24%", "25-49%", "50-74%", "75-94%", ">= 95%"))

### density plot  - layered curves
ggplot(df, aes(x=dayOfYear, ..count.., fill=phenophaseName, color=phenophaseName)) +
  #ggplot(df, aes(x=dayOfYear, ..count.., fill=phenophaseIntensity, color=phenophaseIntensity)) +  
  #geom_density(position="stack")+  # stacks data vertically
  #geom_density(alpha=0.8)+  # sensitivity of the curves
  #geom_density(alpha=0.3, position = position_dodge(width = .05))+
  ggtitle("DEJU2017")+
  geom_density()+  
  #facet_wrap(~taxonID, scale="free_y")+ # places taxonID in separate windows
  xlim(min(df$dayOfYear)-15, max(df$dayOfYear)+15) # x-axis scales by date

## histogram
ggplot(df, aes(x=dayOfYear, fill=phenophaseStatus)) +
  ggtitle("DEJU")+
  geom_histogram(binwidth = 2) +
  facet_wrap(~year, scale="free_y") 
#ylim(0,500)+
#xlim(50, 350)


## Have fun  ##