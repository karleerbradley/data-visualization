library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


# want to make a density map?? for the whole US using each site's lat and long and 
# plot the % of individuals in the phenophase in each site for each month
# phenophase: leaves
# year: 2017
# need to filter out the domains we want
# need to filter out one site from each domain 
# need to filter out the species that are DBL (growthForm)






sites <- c("HARV", "SERC", "UNDE", "UKFS", "ORNL", "CLBJ", "ABBY", "TOOL", "BONA")

all_dbl <- filter(phe_ind, siteID %in% sites)
all_dbl <- filter(all_dbl, growthForm == "Deciduous broadleaf")

sampSize_all <- count(all_dbl, date)
inStat_all <- all_dbl %>%
  group_by(date) %>%
  count(phenophaseStatus)
inStat_all <- full_join(sampSize_all, inStat_all, by = "date")
# only look at the yes's
inStat_T_all <- filter(inStat_all, phenophaseStatus %in% "yes")




all_2017 <- phe_ind %>%
  filter(!is.na(decimalLatitude), 
         !is.na(decimalLongitude)) %>%
  filter(year(date)=="2017") %>%
  filter(phenophaseName == "Leaves") %>%
  filter(domainID %in% domains) %>%
  filter(siteID %in% sites) %>%
  filter(phenophaseStatus == "yes") %>%
  filter(growthForm == "Deciduous broadleaf")

all_2017$percent <- ((all_2017$n.y)/all_2017$n.x)*100

  ggplot(inStat_T, aes(date, percent)) +
  qmplot(decimalLongitude, decimalLatitude, data = ., extent = "panel", alpha = I(.5), color = siteID) +
  stat_density2d(aes(fill = percent, alpha = I(.2)), geom = "polygon") + scale_fill_gradient(low = "black", high = "red") 
