
library(dplyr)
library(ggplot2)
library(lubridate)


DBL <- phe_ind %>%
  filter(growthForm=="Deciduous broadleaf")


write.csv(DBL, file = "DBL.csv", row.names = FALSE)

DBL <- DBL %>%
  filter(phenophaseStatus == "yes")

dblYear <- DBL %>%
  filter(year == "2017")
dblSite <- dblYear %>%
  filter(siteID == "HA")
unique(dblSite$phenophaseName)
