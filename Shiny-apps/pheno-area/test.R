library(dplyr)
library(ggplot2)
library(lubridate)
library(shiny)


# reading in the data needed for the app
DBL <- read.csv("DBL-area.csv", stringsAsFactors = FALSE)

pheno <- DBL %>%
  filter(siteID %in% "HARV") %>%
  filter(year %in% c("2016","2017", "2018"))

# look at the total individuals in phenophase status by day
phenoSamp <- pheno %>%
  group_by(siteID) %>%
  count(date)
phenoStat <- pheno %>%
  group_by(date, siteID, taxonID, phenophaseName) %>%
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


ggplot(phenoStat_T, aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName)) +
  geom_density(alpha=0.3,stat = "identity", position = "stack")+  theme_bw() +
  facet_grid(cols = vars(taxonID),rows = vars(year)) 

