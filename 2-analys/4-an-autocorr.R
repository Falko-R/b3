# Restore SpatialDataFrame object
BT <- readRDS(file = "1-prep/BT.rds")
DISTR <- BT@data
#BER_BB <- readRDS("Daten1/BER_BB_SpatialPolygonDF.rds")
#DISTR <- readRDS("Daten1/BER_BB_data.rds")

BT1 <- BT[BT$reg==1, ]
BT2 <- BT[BT$reg==2, ]
BT21 <- BT2[BT2$GEN!="Potsdam", ]
BT3 <- BT[BT$reg==3, ]
#ber <- BT@data$AGS[as.numeric(BT@data$AGS)<12000000]
BER <- BT[BT$SN_L %in% c("11"), ]
BB <- BT[BT$SN_L %in% c("12"), ]
'%!in%' <- function(x,y)!('%in%'(x,y))
BB2 <- BB[BB$GEN %!in% c("Potsdam","Cottbus","Brandenburg an der Havel","Frankfurt (Oder)"), ]
#"Oranienburg"
#BB$GEN[BB$empl_2013==21110]
#BT$GEN[BT$emp_bev==max(BT$emp_bev)]
BT2 <- BT[BT$GEN %!in% c("Schenkenberg"), ]


# Spatial object BT hat viele Attribute, 
# relevant: empl_2013, empl_2013_52_1, Fläche.km2, Bev.dichte, Bevölkerung.2014, Betriebe.pro.km2
hist(BT3$Bevölkerung.2014,breaks=50)
boxplot(BT3$Bevölkerung.2014, las = 2)
# Besser abgeleitete Attribute und rates statt Bev und empl
BT$emp_bev <- BT$empl_2013/BT$Bevölkerung.2014
boxplot(BT$emp_bev)
hist(BT$emp_bev,breaks=50)


# Mit Hilfe des Pakets tmap plotten wir die Verteilung per Quantil-Klassifikationsschema
library(tmap)
#tm_shape(BT) + tm_polygons(style="quantile", col = "Betriebe.pro.km2") +
#  tm_legend(outside = TRUE, text.size = .8) +tmap_options(max.categories = 4)

tm_shape(BT) + tm_polygons(style="quantile", col = "Fläche.km2") +
  tm_legend(outside = TRUE, text.size = .8)

tm_shape(BT21) + tm_polygons(style="quantile", col = "Bevölkerung.2014") +
  tm_legend(outside = TRUE, text.size = .8)

tm_shape(BB2) + tm_polygons(style="quantile", col = "empl_2013") +
  tm_legend(outside = TRUE, text.size = .8)

tm_shape(BT2) + tm_polygons(style="quantile", col = "emp_bev") +
  tm_legend(outside = TRUE, text.size = .8)


par(mai=c(0,0,0,0))
plot(BT21, col=2:7)
xy <- coordinates(BT21)
points(xy, cex=2, pch=20, col='white')
text(BT21, 'id', cex=1.5)


# Definieren Nachbarschaften der Polygone
# Wir nutzen die Queen-Definition für Nachbarschaft: 
# Angrenzende Polygone welche mind. 1 Knoten teilen (queen=TRUE) stehen in räuml. Beziehung
# ALternativ: Polygone welche mind. 1 Kante teilen (queen=FALSE) 

library(spdep)
nb <- poly2nb(BT21, queen=FALSE)
#w <- poly2nb(BB, row.names=BB$AGS, queen=TRUE)
# class(w)
# summary(w)
# str(w)

# Für jedes Polygon sind in nb alle Nachbarn gespeichert

# für das erste Polygon des Spatial Object mit Namen:
# BT$GEN[1]
# Nachbarn des ersten Polygons (gibt IDs aus):
# nb[[1]]
# Namen der Nachbarn:
# BT$GEN[nb[[1]]]

# Plotten die links zwischen den Polygonen
xy <- coordinates(BT21)
plot(BT21, col='gray', border='blue', lwd=1)
plot(nb, xy, col='red', lwd=1, add=TRUE)

# Jedem Nachbarschaftspolygon wird ein Gewicht zugewiesen
# style="W" weist jedem Nachbarn identische Gewichte zu (1/Anzahl Nachbarn)
# Problem: Randgebiete haben tendenziell weniger Nachbarn und ihre lag-Werte basieren auf weniger Polygonen. 
# Sie sind somit anfälliger für Über oder Unterschätzung der tatschlichen Autokorrelation
# style="B" ist eine robustere Alternative
# zero.policy=TRUE erlaubt die Listung von Gemeinden ohne Nachbarn, welche jedoch die Anzahl n in der I und c Statistik 
# aufblähen würden (betrifft uns hier aber nicht)

lw <- nb2listw(nb, style="B", zero.policy=TRUE)
#alternativ
#lw <- nb2listw(nb, style='W',zero.policy=TRUE)

# z.B. die Gewichte der Nachbarn des ersten Polygons (Berlin-Mitte)
# Wird etwa das lokale Durchschnittseinkommen berechnet, so fließt das Einkommen eines jeden Nachbarn zu 20% ein
lw$weights[1]

# manuelle Berechnung MORAN I
n <- length(BB)
y <- BB$Bevölkerung.2014
ybar <- mean(y)

dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj

# Y Matrix der multiplizierten Paare
pm <- matrix(yiyj, ncol=n)
# Gewichtung mit wij
pmw <- pm * wm
#Kreuzprodukt berechnen (Zähler des 2.Faktors in Formel für I)
spmw <- sum(pmw)
#Teile durch Summe der Gewichte
smw <- sum(wm)
sw  <- spmw / smw
# berechne inverse Varianz von y
vr <- n / sum(dy^2)
# Ermittle MOrans I
MI <- vr * sw
MI
# Erwartungswert von I
EI <- -1/(n-1)
EI


# automatische Berechnung MORAN I
moran(BT21$Bevölkerung.2014, lw, n=length(lw$neighbours), S0=Szero(lw))

# einfache MORAN I Berechnung
moran.test(BT21$Bevölkerung.2014,lw, randomisation=TRUE)
# test under randomisation: randomisation=TRUE
# test under normality : randomisation=FALSE

# Monte Carlo Test

MC<- moran.mc(BT21$Bevölkerung.2014, lw, nsim=999)
#What is the maximum value we can use for nsim??

# View results (including p-value)
MC

# Plot the distribution (note that this is a density plot instead of a histogram)
plot(MC, main="", las=1)

##############################################################################################
# Moran Scatterplot: ALTERNATIVE BERECHNUNG

n <- length(BB)
y <- BB$Bevölkerung.2014

# Transformieren w in Nachbarschaftsmatrix, welche die Intensität der räumlichen Beziehungen widerspiegelt
wm <- nb2mat(nb, style='B')

# Nachbarwerte abfragen
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
# Zeilen mit Nullwerten entfernen
ms <- ms[ms[,3] > 0, ]
# durchschnittl Nachbarwert berechnen
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'spatially lagged y')
head(ams)

# Scatterplot erstellen
plot(ams)
reg <- lm(ams[,2] ~ ams[,1])
abline(reg, lwd=2)
abline(h=mean(ams[,2]), lt=2)
abline(v=ybar, lt=2)

coefficients(reg)[2]

rwm <- mat2listw(wm, style='W')
# Checking if rows add up to 1
# mat <- listw2mat(rwm)
# apply(mat, 1, sum)[1:15]

moran.plot(y, rwm)


# Berechnung spatially lagged values = durchschnittliche Nachbareinkommen für jedes Polygon 
# Inc_lag <- lag.listw(lw, BB$Bevölkerung.2014)
# irgendwie kein richtiges Mittel?
Inc_lag <- lag.listw(rwm, BB$Bevölkerung.2014)

# Wir können lagged values gegen Ursprungswerte plotten und ein lineares Regressionsmodell anpassen

# Regressionsmodell
M <- lm(Inc_lag ~ BB$Bevölkerung.2014)

plot(Inc_lag ~ BB$Bevölkerung.2014, pch=20, asp=1, las=1)

# Anstieg der Regressionsgeraden
coef(M)[2]


# Zur Abschätzung der Signifikanz randomisieren wir die empl_2013-Werte über alle Gemeinden und implementieren keinerlei 
# räumliche Autokorrelationsstruktur 
# Zu jeder Permutation wird ein Regressionsmodell angepasst und der Regressionsanstieg als Wert einer Moran I Statistik erfasst 
# Die Verteilung aller Werte gibt uns die erwartete Verteilung unter Annahme der Nullhypothese aus, dass empl_2013 Werte zufällig 
# über die Gemeinden verteilt sind.
# Letztlich wird der tatsächliche Moran I-Wert mit der simulierten Verteilung verglichen

n <- 1599L   # Define the number of simulations
I_r <- vector(length=n)  # Create an empty vector

for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(BB$Bevölkerung.2014, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(rwm, x)
  # Compute the regression slope and store its value
  M_r    <- lm(x.lag ~ x)
  I_r[i] <- coef(M_r)[2]
}

# Plot the histogram of simulated Moran's I values
# then add our observed Moran's I value to the plot
hist(I_r, main=NULL, xlab="Moran's I", las=1,breaks = 50)
abline(v=coef(M)[2], col="red")


# Die Simulation zeigt, dass der beobachtete Moran I-Wert nicht zur Erwartung bei nicht vorhandener Autokorrelation passt
# Von dieser Simulation lässt sich ein pseudo p-value ableiten
# Zuerst wird die Anzahl an Simulationsläufen ermittelt, welche einen Moran-I Wert ergab, welcher über dem tatsächlich beobachte
N_greater <- sum(coef(M)[2] > I_r)

# pseudo p-Wert
# To compute the p-value, find the end of the distribution closest to the observed Moran’s I value, 
# then divide that count by the total count. Note that this is a so-called one-sided P-value.
p <- min(N_greater + 1, n + 1 - N_greater) / (n + 1)

# Der p-value gibt an, dass eine kleine Chance (0.000625%) besteht, dass die Aussage 
# "empl-Werte sind nicht autokorreliert auf Gemeindelevel" falsch ist


#################################################################################################################

# Vorbereitung
## Einlesen der shape- und Exceldaten
## global morans I (based on centroids)

require("ape")
#library(ape)

# Nachbarschaftsmatrix W mit inversen Distanzgewichten
point.dists <- as.matrix(dist(cbind(BB$c_long,BB$c_lat)))
point.dists.inv  <- 1/point.dists
diag(point.dists.inv) <- 0

pdi <- mat2listw(point.dists.inv, style='B')
# Checking if rows add up to 1
# mat <- listw2mat(pdi)
# apply(mat, 1, sum)[1:15]


moran(BB$Bevölkerung.2014, pdi, n=length(pdi$neighbours), S0=Szero(pdi))
moran.test(BB$Bevölkerung.2014,pdi, randomisation=TRUE)
# Monte Carlo Test
MC<- moran.mc(BB$Bevölkerung.2014, pdi, nsim=2999)

# Plot the distribution (note that this is a density plot instead of a histogram)
plot(MC, main="", las=1)


# Transformieren w in Nachbarschaftsmatrix, welche die Intensität der räumlichen Beziehungen widerspiegelt
# wm <- nb2mat(nb, style='B')

Moran.I(BB$Bevölkerung.2014,point.dists.inv)

Moran.I(BB$Bevölkerung.2014,wm)

#p.value=0 --> Nullhypothesis of clustering cannot be rejected (AT ALL)


##########################################################################################################

## local morans I (based on centroids)
library(spdep)

BT_nbq <- poly2nb(BT)  #queen's neighborhood
BT_nbq_w <- nb2listw(BT_nbq)
locm <- localmoran(BT$empl_2013, BT_nbq_w)  #calculate the local moran's I
summary(locm)

# manually make a moran plot - standardize variables
BT$s_empl_2013 <- scale(BT$empl_2013)  #save to a new column

# create a lagged variable
BT$lag_empl_2013 <- lag.listw(BT_nbq_w, BT$s_empl_2013)

#summary(BT$lag_empl_2013)

plot(x = BT$s_empl_2013, y = BT$lag_empl_2013, main = " Moran Scatterplot empl2013")
abline(h = 0, v = 0)
abline(lm(BT$lag_empl_2013 ~ BT$s_empl_2013), lty = 3, lwd = 4, col = "red")

# check out the outliers click on one or two and then hit escape (or click finish)
#identify(BT$s_empl_2013, soco$lab_sPPOV, soco$CNTY_ST, cex = 0.8)




# identify the moran plot quadrant for each observation
BT$quad_sig <- NA
BT@data[(BT$s_empl_2013 >= 0 & BT$lag_empl_2013 >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 1
BT@data[(BT$s_empl_2013 <= 0 & BT$lag_empl_2013 <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 2
BT@data[(BT$s_empl_2013 >= 0 & BT$lag_empl_2013 <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 3
BT@data[(BT$s_empl_2013 >= 0 & BT$lag_empl_2013 <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 4
BT@data[(BT$s_empl_2013 <= 0 & BT$lag_empl_2013 >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)

# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")

# see ?findInterval - This is necessary for making a map
np <- findInterval(BT$quad_sig, breaks)

# Assign colors to each map class
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(BT, col = colors[np])  #colors[np] manually sets the color for each county
mtext("Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")







