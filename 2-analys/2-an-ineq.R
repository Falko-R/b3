# Restore SpatialDataFrame object
BT <- readRDS(file = "1-prep/BT.rds")
DISTR <- BT@data

# DISTR <- readRDS("Daten1/BER_BB_data.rds")
# BER_BB <- readRDS("Daten1/BER_BB_SpatialPolygonDF.rds")
#write.csv(DISTR, "DISTR.csv", row.names=FALSE)

# which(DISTR[,17:24]=="0")
DF <- DISTR
# DF_Lagerei_06bis13 <- DF[which(DF[,17]!=0 & DF[,18]!=0 & DF[,19]!=0 & DF[,20]!=0 & DF[,21]!=0 & DF[,22]!=0 & DF[,23]!=0 & DF[,24]!=0),]
# 
# indizes_nie_Lagerei <- which(DF[,17]==0 & DF[,18]==0 & DF[,19]==0 & DF[,20]==0 & DF[,21]==0 & DF[,22]==0 & DF[,23]==0 & DF[,24]==0)
# full.list <- seq(from = 1, to = 418)
# 
# indizes <- setdiff(full.list,indizes_nie_Lagerei)
# 
# DF_Lagerei_mind1Jahr <- DF[indizes,]


#oder:
DF5 <- DF[rowSums(sapply(DF[,c(17,18,19,20,21,22,23,24)],`!=`,e2=0))!=0,]

REG1 <- DISTR[which(DISTR$REG==1),]
REG2 <- DISTR[which(DISTR$REG==2),]
REG3 <- DISTR[which(DISTR$REG==3),]
REG23 <- DISTR[which(DISTR$REG!=1),]

require(ineq)
ineq(GemBB2@data$WZ3.52.1,type="Gini")   #0.899204
ineq(GemBB2@data$Summe.Betriebe,type="Gini")   #0.6978698
plot(Lc(GemBB2@data$WZ3.52.1),col="darkred",lwd=2)
ineq(DISTR$WZ3.52.1_2013,type="Atkinson")   # 0.899204
ineq(DISTR$WZ3.52.1_2012,type="Atkinson")   # 0.8964646
ineq(DISTR$WZ3.52.1_2011,type="Atkinson")   # 0.8935407
ineq(DISTR$WZ3.52.1_2010,type="Atkinson")   # 0.8820783
ineq(DISTR$WZ3.52.1_2009,type="Atkinson")   # 0.8989179
ineq(DISTR$WZ3.52.1_2008,type="Atkinson")   # 0.8941327
ineq(DISTR$WZ3.52.1_2007,type="Atkinson")   # 0.9040292
ineq(DISTR$WZ3.52.1_2006,type="Atkinson")   # 0.8919139
plot(Lc(DISTR$WZ3.52.1_2006),col="darkred",lwd=2)

ineq(DISTR$AN2013,type="Atkinson")   
ineq(DISTR$AN2012,type="Atkinson")   
ineq(DISTR$AN2011,type="Atkinson")  
ineq(DISTR$AN2010,type="Atkinson")   
ineq(DISTR$AN2009,type="Atkinson")            
ineq(DISTR$AN2008,type="Atkinson")   
ineq(DISTR$AN2007,type="Atkinson")   
ineq(DISTR$AN2006,type="Atkinson")   

ineq(DISTR$WZ3.ALL_2013,type="Atkinson")   
ineq(DISTR$WZ3.ALL_2012,type="Atkinson")   
ineq(DISTR$WZ3.ALL_2011,type="Atkinson")  
ineq(DISTR$WZ3.ALL_2010,type="Atkinson")   
ineq(DISTR$WZ3.ALL_2009,type="Atkinson")   
ineq(DISTR$WZ3.ALL_2008,type="Atkinson")   
ineq(DISTR$WZ3.ALL_2007,type="Atkinson")   
ineq(DISTR$WZ3.ALL_2006,type="Atkinson")   

ineq(DISTR$ALL.AN2013,type="Atkinson")   
ineq(DISTR$ALL.AN2012,type="Atkinson")   
ineq(DISTR$ALL.AN2011,type="Atkinson")  
ineq(DISTR$ALL.AN2010,type="Atkinson")   
ineq(DISTR$ALL.AN2009,type="Atkinson")   
ineq(DISTR$ALL.AN2008,type="Atkinson")   
ineq(DISTR$ALL.AN2007,type="Atkinson")   
ineq(DISTR$ALL.AN2006,type="Atkinson") 

plot(Lc(DISTR$AN2006),col="darkred",lwd=2,main="2006")
par(new=TRUE)
#oder lines()
plot(Lc(DISTR$ALL.AN2006),col="darkblue",lwd=2,main="")




ineq(DF5$WZ3.52.1_2013,type="Gini")   # 0.5564974
ineq(DF5$WZ3.52.1_2012,type="Gini")   # 0.5444444
ineq(DF5$WZ3.52.1_2011,type="Gini")   # 0.5315789
ineq(DF5$WZ3.52.1_2010,type="Gini")   # 0.4811446
ineq(DF5$WZ3.52.1_2009,type="Gini")   # 0.5552386
ineq(DF5$WZ3.52.1_2008,type="Gini")   # 0.5341839
ineq(DF5$WZ3.52.1_2007,type="Gini")   # 0.5777285
ineq(DF5$WZ3.52.1_2006,type="Gini")   # 0.5244211

plot(Lc(DF5$WZ3.52.1_2013),col="darkred",lwd=2,main="2013")
plot(Lc(DF5$WZ3.52.1_2012),col="darkred",lwd=2,main="2012")
plot(Lc(DF5$WZ3.52.1_2011),col="darkred",lwd=2,main="2011")
plot(Lc(DF5$WZ3.52.1_2010),col="darkred",lwd=2,main="2010")
plot(Lc(DF5$WZ3.52.1_2009),col="darkred",lwd=2,main="2009")
plot(Lc(DF5$WZ3.52.1_2008),col="darkred",lwd=2,main="2008")
plot(Lc(DF5$WZ3.52.1_2007),col="darkred",lwd=2,main="2007")
plot(Lc(DF5$WZ3.52.1_2006),col="darkred",lwd=2,main="2006")

ineq(DF5$WZ3.52.1_2013,type="RS")   # 0.3929204
ineq(DF5$WZ3.52.1_2012,type="RS")   # 0.3766082
ineq(DF5$WZ3.52.1_2011,type="RS")   # 0.3595142
ineq(DF5$WZ3.52.1_2010,type="RS")   # 0.3242718
ineq(DF5$WZ3.52.1_2009,type="RS")   # 0.3876045
ineq(DF5$WZ3.52.1_2008,type="RS")   # 0.3559809
ineq(DF5$WZ3.52.1_2007,type="RS")   # 0.4
ineq(DF5$WZ3.52.1_2006,type="RS")   # 0.3505263


gini_ineq <- sapply(17:24, function(x) ineq((DF5[,x]),type='Gini') )
rs_ineq <- sapply(24:17, function(x) ineq((DF5[,x]),type='RS') )
atk_ineq <- sapply(24:17, function(x) ineq((DF5[,x]),type='Atkinson') )
theil_ineq <- sapply(24:17, function(x) ineq((DF5[,x]),type='Theil') )
entr_ineq <- sapply(24:17, function(x) ineq((DF5[,x]),type='entropy') )

UNGL <- data.frame(YEAR=c("2006","2007","2008","2009","2010","2011","2012","2013"),GINI=rev(gini_ineq),RS=rs_ineq,ATKINSON=atk_ineq,THEIL=theil_ineq,ENTROPY=entr_ineq)


plot((as.numeric(as.character(UNGL$YEAR))+0.5),UNGL$GINI,type="s", col = "darkred",ylim=c(0,1),xlab="year",ylab="")
par(new=T)
plot((as.numeric(as.character(UNGL$YEAR))+0.5),UNGL$RS,type="s", col = "darkgreen",ylim=c(0,1),xlab="year",ylab="")
par(new=T)
plot((as.numeric(as.character(UNGL$YEAR))+0.5),UNGL$ATKINSON,type="s", col = "royalblue",ylim=c(0,1),xlab="year",ylab="")
par(new=T)
plot((as.numeric(as.character(UNGL$YEAR))+0.5),UNGL$THEIL,type="s", col = "gold",ylim=c(0,1),xlab="year",ylab="")
par(new=T)
plot((as.numeric(as.character(UNGL$YEAR))+0.5),UNGL$ENTROPY,type="s", col = "black",ylim=c(0,1),xlab="year",ylab="")


#attach(UNGL); plot(YEAR, RS, GINI); detach(UNGL)  #col=c("red","blue","green")[YEAR]
library(reshape2)
long <- reshape(UNGL, varying = NULL, timevar = "time", idvar = "id", direction="wide", sep = "")




# ggplot(UNGL, aes(x = YEAR, y=GINI)) + geom_density(alpha = 0.6) #+ scale_fill_brewer() 
# 
# plotPoints <- ggplot(UNGL, aes(x = factor(year), y = dis, color = factor(year))) + 
#   geom_point(aes(y = dis, color = factor(year)), 
#              position = position_jitter(width = 0.25, height = 0.0),
#              alpha = 0.6) + 
#   geom_point(aes(y = dis_mean), color = "black", size = 2, data = DISSummary) + 
#   geom_errorbar(aes(y = dis_mean, ymin = dis_mean-dis_se, ymax = dis_mean+dis_se), 
#                 color = "black", width = 0.2, data = DISSummary) + 
#   ylim(15500, 126000) + 
#   theme(legend.position = "none",
#         panel.background = element_rect(fill = "darkgrey")) +
#   ggtitle("Alle Beobachtungen") +
#   scale_fill_brewer() +
#   scale_colour_brewer() #scale_colour_manual(values=c("white", "black","blue", "cyan4"))
# 



ineq(DISTR$WZ3.52.1_2013,type="RS")
ineq(GemBB2@data$Summe.Betriebe,type="RS")

ineq(DISTR$WZ3.52.1_2013,type="Atkinson")
ineq(GemBB2@data$Summe.Betriebe,type="Atkinson")

ineq(DISTR$WZ3.52.1_2013,type="Theil")
ineq(GemBB2@data$Summe.Betriebe,type="Theil")

ineq(DISTR$WZ3.52.1_2013,type="entropy")
ineq(GemBB2@data$Summe.Betriebe,type="entropy")


