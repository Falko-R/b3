# Restore SpatialDataFrame object
BT <- readRDS(file = "1-prep/BT.rds")

DISTR <- BT@data

dis <- rep(DISTR$dist2BN,times=DISTR$WZ3.52.1_2013)
year <- rep(2013,times=length(dis))
DIS2013 = cbind.data.frame(dis,year)

dis <- rep(DISTR$dist2BN,times=DISTR$WZ3.52.1_2012)
year <- rep(2012,times=length(dis))
DIS2012 = cbind.data.frame(dis,year)

dis <- rep(DISTR$dist2BN,times=DISTR$WZ3.52.1_2011)
year <- rep(2011,times=length(dis))
DIS2011 = cbind.data.frame(dis,year)

dis <- rep(DISTR$dist2BN,times=DISTR$WZ3.52.1_2010)
year <- rep(2010,times=length(dis))
DIS2010 = cbind.data.frame(dis,year)

dis <- rep(DISTR$dist2BN,times=DISTR$WZ3.52.1_2009)
year <- rep(2009,times=length(dis))
DIS2009 = cbind.data.frame(dis,year)

dis <- rep(DISTR$dist2BN,times=DISTR$WZ3.52.1_2008)
year <- rep(2008,times=length(dis))
DIS2008 = cbind.data.frame(dis,year)

dis <- rep(DISTR$dist2BN,times=DISTR$WZ3.52.1_2007)
year <- rep(2007,times=length(dis))
DIS2007 = cbind.data.frame(dis,year)

dis <- rep(DISTR$dist2BN,times=DISTR$WZ3.52.1_2006)
year <- rep(2006,times=length(dis))
DIS2006 = cbind.data.frame(dis,year)


DIS <- rbind(DIS2006,DIS2007,DIS2008,DIS2009,DIS2010,DIS2011,DIS2012,DIS2013)
ggplot(DIS, aes(x = factor(year), y = dis, fill = factor(year))) + geom_violin()+geom_boxplot(width = 0.2,fill="white") + labs(fill = "Jahr") +ggtitle("H?ufigkeit der Distanzen aller 52.1-Unternehmen von Berlin in Brandenburg") + scale_fill_brewer() 

DIS1 <- DIS[which(DIS$year %in% c(2006,2007,2008,2009)),]
DIS2 <- DIS[which(DIS$year %in% c(2010,2011,2012,2013)),]
ggplot(DIS1, aes(x = dis, fill = factor(year))) + geom_density(alpha = 0.6) #+ scale_fill_brewer() 
ggplot(DIS2, aes(x = dis, fill = factor(year))) + geom_density(alpha = 0.6) #+ scale_fill_brewer()

#*****************************************************************************

library(gridExtra)
library(dplyr)

DISSummary <- DIS %>%
  group_by(year) %>%
  summarize(dis_mean = mean(dis),
            dis_se = sqrt(var(dis)/length(dis)))


plotPoints <- ggplot(DIS, aes(x = factor(year), y = dis, color = factor(year))) + 
  geom_point(aes(y = dis, color = factor(year)), 
             position = position_jitter(width = 0.25, height = 0.0),
             alpha = 0.6) + 
  geom_point(aes(y = dis_mean), color = "black", size = 2, data = DISSummary) + 
  geom_errorbar(aes(y = dis_mean, ymin = dis_mean-dis_se, ymax = dis_mean+dis_se), 
                color = "black", width = 0.2, data = DISSummary) + 
  ylim(15500, 126000) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "darkgrey")) +
  ggtitle("Alle Beobachtungen") +
  scale_fill_brewer() +
  scale_colour_brewer() #scale_colour_manual(values=c("white", "black","blue", "cyan4"))

plotViolins <- ggplot(DIS, aes(x = factor(year), y = dis, fill = factor(year))) + 
  geom_violin(aes(y = dis, fill = factor(year))) + 
  geom_boxplot(width = 0.2,fill="white") + 
  geom_point(aes(y = dis_mean), color = "black", size = 2, data = DISSummary) + 
  geom_errorbar(aes(y = dis_mean, ymin = dis_mean-dis_se, ymax = dis_mean+dis_se), 
                color = "black", width = 0.2, data = DISSummary) + 
  ylim(15500, 126000) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "darkgrey")) + 
  ggtitle("Violin plot") +
  scale_fill_brewer()
#+ theme_classic()
#+ theme(plot.background = element_rect(fill = 'green', colour = 'red'))

grid.arrange(plotPoints, plotViolins, ncol = 2)

#*********************************************************************

# +   theme(
#     axis.line = element_blank(),              
#     axis.text.x = element_blank(), 
#     axis.text.y = element_blank(), 
#     axis.ticks = element_blank(), 
#     axis.title.x = element_blank(), 
#     axis.title.y = element_blank(), 
#     panel.background = element_blank(), 
#     panel.border = element_blank(), 
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(), 
#     strip.background = element_blank())


# hist(DIS2013,50)
# hist(DIS2010,50)
# hist(DIS2006,50)

