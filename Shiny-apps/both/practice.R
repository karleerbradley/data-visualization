# practice for both app

library(ggplot2)
library(gridExtra)
library(dplyr)
library(lubridate)
library(scales)
library(gtable)
library(grid)

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






# using way of graphing from NEON website
grid.newpage()
phenoP2 <- ggplot(phenoStat_T, aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName)) +
  geom_density(alpha=0.3,stat = "identity", position = position_dodge(width = .1)) +
  #geom_density(alpha=0.3,stat = "identity", position = "stack") +  
  theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
  xlab("") + ylab("% of Individuals") + xlim(0,366) +
  ggtitle("Phenophase Density for Selected Site") +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase") +
  theme(legend.justification=c(0,1),
        legend.position=c(0,1),
        plot.title=element_text(size=25,vjust=1),
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20))

yearsP2 <- ggplot(data=site_select, aes(x=dayOfYear, y=AGDD, color = as.factor(year))) +
  geom_path() + xlab("") + ggtitle("Accumulated Growing Degree Days Across Years") +
  scale_color_brewer(palette = "Dark2") + ylab("AGGDs")  + xlim(c(0,366))+ #theme_bw() +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5), 
        axis.title = element_text(lineheight = .5, size = 15),
        legend.title = element_text(lineheight = .5, size = 12)) + labs(color="Year") +
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_text(size=20,color="red"),
        axis.title.y=element_text(size=20))

g1 <- ggplot_gtable(ggplot_build(phenoP2))
g2 <- ggplot_gtable(ggplot_build(yearsP2))
pp <- c(subset(g1$layout, name == "panel", se=t:r))
# error starting here
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]],
                     pp$t,pp$l,pp$b,pp$l)
ia <- which(g2$layout$name=="axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(as$widths)
as$grobs <- rev(ax$grobs)
as$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

grid.draw(g)






# using another way of graphing
# how to do facet?
# source: https://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
par(mar=c(3.5, 3.5, 3.5, 3.5) + 0.1)
# ggplot(phenoStat_T, aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName)) +
#   geom_density(alpha=0.3,stat = "identity", position = position_dodge(width = .1)) +
#   #geom_density(alpha=0.3,stat = "identity", position = "stack") +
#   theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
#   xlab("Day Of Year") + ylab("% of Individuals") + xlim(0,366) +
#   ggtitle("Phenophase Density for Selected Site") +
#   theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5)) +
#   theme(text = element_text(size = 15)) +
#   scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
#   theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase")
plot(phenoStat_T$dayOfYear, phenoStat_T$percent, pch=16, xlab = "", ylab = "",
     scale = "free_y", axes = FALSE, type="b", col=phenoStat_T$phenophaseName, main = "Phenophases and AGDDs")
axis(2, ylim=c(0,50), col = "black", las=1)
mtext("% of Individuals", side = 2, line = 2.5)
box()
par(new=TRUE)
plot(site_select$dayOfYear, site_select$AGDD, pch=15,
     xlab = "", ylab = "", ylim = c(0,6000),
     axes = FALSE, type = "b", col="black")
mtext("AGDDs", side = 4, col = "red", line = 4)
axis(4, ylim=c(0,6000), col = "red", col.axis="red", las=1)
axis(1, pretty(range(phenoStat_T$dayOfYear), 10))
mtext("Days of the Year", side = 1, col = "black", line = 2.5)
legend("topleft", legend = c("Phenophases", "AGDDs"),
       text.col = c("black", "red"), pch = c(16,15), col = c("black", "red"))







# another way of combining the graphs
# error: x and y must have the same number of columns, line 161
# source: https://github.com/tidyverse/ggplot2/wiki/Align-two-plots-on-a-page
g1 <- ggplotGrob(phenoP2)
g1 <- gtable_add_cols(g1, unit(0, "mm"))
g2 <- ggplotGrob(yearsP2)
g <- rbind(g1, g2, size="first")
g$widths <- unit.pmax(g1$widths, g2$widths)








#another way of trying
# source: https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
ggplot() +
  geom_density(mapping=aes(x=phenoStat_T$dayOfYear, y=phenoStat_T$percent),
                 alpha=0.3,stat = "identity", position = position_dodge(width = .1), color=phenoStat_T$phenophaseName,
                 fill = phenoStat_T$phenophaseName) +
  geom_path(mapping = aes(x=site_select$dayOfYear, y=site_select$AGDD), color=as.factor(site_select$year)) +
  xlim(0,366)+
  #facet_grid(cols = vars(phenoStat_T$commonName),rows = vars(phenoStat_T$year), scale = "free_y") +
  scale_y_continuous(name = "% of Individuals", sec.axis = sec_axis(~./240, name = "AGDDs", labels = c(0,6000)))+
  theme(
    axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "blue"))






# source: https://rpubs.com/MarkusLoew/226759


# phenoStat_T <- phenoStat_T %>%
#   filter(commonName == "Red Maple")

p<- ggplot(data=phenoStat_T, mapping=aes(x = dayOfYear, y = percent, fill = phenophaseName, color = phenophaseName)) +
  geom_density(data=phenoStat_T,alpha=0.3,stat = "identity", position = position_dodge(width = .1)) +
  #geom_density(alpha=0.3,stat = "identity", position = "stack") +  
  theme_bw() + facet_grid(cols = vars(commonName),rows = vars(year), scale = "free_y") +
  #xlab("Day Of Year") + ylab("% of Individuals") + xlim(0,366) +
  #ggtitle("Phenophase Density for Selected Site") +
  #theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20, hjust = 0.5)) +
  #theme(text = element_text(size = 15)) +
  scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") + labs(fill = "Phenophase", color = "Phenophase")
p <- p+geom_path(data=site_select,aes(x=dayOfYear, y=AGDD/200), inherit.aes = FALSE, size = 1.5) 
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*200, name = "AGDDs"))
p <- p + labs(x = "Day of Year", y = "Percentage of Individuals in Each Phenophase")
p  


