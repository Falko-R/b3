# Dieses Script liefert viele mögliche Funktionen und Paketbeiträge zur Untersuchung der Autokorrelation
# Es liegt als reine Auflistung für schnelle, unkomplizierte Anwendung vor  
# für eine spezialisierte Betrachtung ausgewählter mit Anwendung auf relevante Daten sowie
# zusätzliche Beschreibungen, Details und bessere Lesbarkeit siehe
# für globale Maße und Tests: "4-an-autocorr-global.Rmd"
# lokale Maße und Tests: "4-an-autocorr-local.Rmd"

# Restore SpatialDataFrame object
#? DISTR <- BT@data
BT <- readRDS(file = "1-prep/BT.rds")
# neues File
#BT <- readRDS(file = "1-prep/BT2.rds")

'%!in%' <- function(x,y)!('%in%'(x,y))

BER <- BT[BT$reg==1, ]
BT2 <- BT[BT$reg==2, ]
BT21 <- BT2[BT2$GEN!="Potsdam", ]
#BT21 <- BT[BT$reg %in% c(1,2), ]

BB <- BT[BT$reg %in% c(2,3), ]
BB2 <- BB[BB$GEN %!in% c("Potsdam","Cottbus","Brandenburg an der Havel","Frankfurt (Oder)"), ]

#Aussortieren
#BB$GEN[BB$empl_2013==21110]
#"Oranienburg"
#BT$GEN[BT$emp_bev==max(BT$emp_bev)]
BT2 <- BT[BT$GEN %!in% c("Schenkenberg"), ]

# Spatial object BT hat viele Attribute, 
# relevant: empl_2013, empl_2013_52_1, Fläche.km2, Bev.dichte, Bevölkerung.2014, Betriebe.pro.km2
hist(BT3$Bevölkerung.2014,breaks=50)
boxplot(BT3$Bevölkerung.2014, las = 2)

hist(BT2$emp_bev,breaks=50)
boxplot(BT2$emp_bev, las = 2)
# Besser abgeleitete Attribute und rates statt Bev und empl
boxplot(BT$Fläche.km2)
hist(BT$Fläche.km2,breaks=50)

# Mit Hilfe des Pakets tmap plotten wir die Verteilung per Quantil-Klassifikationsschema
require(tmap)
require(tmaptools)
#tm_shape(BT) + tm_polygons(style="quantile", col = "Betriebe.pro.km2") +
#  tm_legend(outside = TRUE, text.size = .8) +tmap_options(max.categories = 4)

# Kompass-Details
# tm_shape(BT2) + tm_borders() + tm_scale_bar(position = c("left","bottom")) + 
#   tm_compass(type = "8star" ,size = 3, show.labels = 2,cardinal.directions = c("N", "O", "S", "W"),
#              text.color = "red", color.dark = "blue", color.light = "cyan", position = c("right","top"))

tm_shape(BT2) + tm_borders() + tm_scale_bar(position = c("left","bottom")) + 
  tm_compass(size = 1.5,position = c("right","top")) 
#+ tm_bubbles(size = 0.2, col="red",shape = 21)


tm_shape(BT) + tm_polygons(style="quantile", col = "Fläche.km2") +
  tm_legend(outside = TRUE, text.size = .8)

tm_shape(BT21) + tm_polygons(style="quantile", col = "Bevölkerung.2014") +
  tm_legend(outside = TRUE, text.size = .8)

tm_shape(BB2) + tm_polygons(style="quantile", col = "empl_2013") +
  tm_legend(outside = TRUE, text.size = .8)

tm_shape(BT2) + tm_polygons(style="quantile", col = "emp_bev") +
  tm_legend(outside = FALSE, text.size = .8) 


par(mai=c(0,0,0,0))
plot(BT21, col=2:7)
xy <- coordinates(BT21)
points(xy, cex=2, pch=20, col='white')
text(BT21, 'id', cex=1.5)

# -------------------------- Start der Analyse -----------------------------------------------
BT$emp_bev <- BT$empl_2013/BT$Bevölkerung.2014
SPDF <- BT[BT$reg %in% c(1,2), ]
 
# Definieren Nachbarschaften der Polygone
# Wir nutzen die Queen-Definition für Nachbarschaft: 
# Angrenzende Polygone welche mind. 1 Knoten teilen (queen=TRUE) stehen in räuml. Beziehung
# ALternativ: Polygone welche mind. 1 Kante teilen (d.h. mind. 2 Knoten) (queen=FALSE) 

# -------- Nachbarschaften -------------################################
library(spdep)
#snap=sqrt(.Machine$double.eps*1000) erlaubt Lücken, wie sie durch das Einfügen Berliner Bezirke entstehen
# first order queen contiguity
nb_qc <- poly2nb(SPDF, row.names=SPDF$id,queen=TRUE,snap=50)
# first order rook contiguity
nb_rc <- poly2nb(SPDF, row.names=SPDF$id,queen=FALSE,snap=50)
# second order queen contiguity
nb_sqc <- nblag(nb_qc,2)
# second order rook contiguity
nb_src <- nblag(nb_rc,2)


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
# second order infos abrufen
#nb_sqc[[2]]

# Distance-based neighbours
coords <- coordinates(SPDF) # get centroid coordinates
# 1. Euclidean distance between centroids: dnearneigh
nb_d <- dnearneigh(coords, 0, 13000,row.names = SPDF$id)  #upper and lower distance bounds in m
# 2. number of k nearest neighbors: knn2nb
nb_5 <- knn2nb(knearneigh(coords,k=5),row.names = SPDF$id)



# Plotten die links zwischen den Polygonen
# coords <- coordinates(BT2) # get centroid coordinates
plot(SPDF, col='gray', border='blue', lwd=1)
plot(nb_d, coords, col='red', lwd=1, add=TRUE)


# Jedem Nachbarschaftspolygon wird ein Gewicht zugewiesen
# style="W" weist jedem Nachbarn identische Gewichte zu (1/Anzahl Nachbarn)
# Reihenstandardisierung bewertet Beobachtungen mit wenigen Nachbarn stärker
# Problem: Randgebiete haben tendenziell weniger Nachbarn und ihre lag-Werte basieren auf weniger Polygonen. 
# Sie sind somit anfälliger für Über- oder Unterschätzung der tatschlichen Autokorrelation
# style="B" ist eine robustere Alternative
# Styles "B", "C" und "U" gewichten Beobachtungen mit vielen Nachbarn stärker
# Style "S" ....?
# Die Testfunktionen akzeptieren im Standardmodus keine Raumeinheiten ohne jegliche Nachbarschaftsbeziehung (Inseln)
# Bei der ursprünglichen Entwicklung der Statistiken wurde davon ausgegangen, dass alle Einheiten zu mind. 1 Nachbarn in Beziehung stehen.
# zero.policy=TRUE erlaubt die Listung von Gemeinden ohne Nachbarn, welche jedoch zur Erhöhung der I und c Statistik beitragen, während
# durch das "aufgeblähte" n der Erwartungswert und die Varianz reduziert werden.
# adjust.n = TRUE (by default) sorgt für Korrektur der Statistik, indem die Anzahl Lokationen ohne Nachbarn von n abgezogen wird
# [Bivand 2013,pp.278-279]

lw <- nb2listw(nb_d, style="W", zero.policy=TRUE)

# z.B. die Gewichte der Nachbarn des ersten Polygons (Berlin-Mitte)
# Wird etwa das lokale Durchschnittseinkommen berechnet, so fließt das Einkommen eines jeden Nachbarn zu 20% ein
# lw$weights[1]


MC<- moran.mc(SPDF$emp_bev, lw, nsim=1999)
MC
plot(MC, main="", las=1)

#----------- manuelle Berechnung MORAN I ------------------------###########

wmat <- nb2mat(nb_d, style="B")
colnames(wmat) <- rownames(wmat)

n <- length(SPDF)
y <- SPDF$Bevölkerung.2014
ybar <- mean(y)

dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj

# Y Matrix der multiplizierten Paare
pm <- matrix(yiyj, ncol=n)
# Gewichtung mit wij
pmw <- pm * wmat
#Kreuzprodukt berechnen (Zähler des 2.Faktors in Formel für I)
spmw <- sum(pmw)
#Teile durch Summe der Gewichte
smw <- sum(wmat)
sw  <- spmw / smw
# berechne inverse Varianz von y
vr <- n / sum(dy^2)
# Ermittle MOrans I
MI <- vr * sw
MI
# Erwartungswert von I
EI <- -1/(n-1)
EI

# direkt MORAN I Berechnung (spdep-package)

moran(SPDF$emp_bev, listw = lw, n=length(lw$neighbours), S0=Szero(lw))

moran.test(SPDF$emp_bev,listw = lw, randomisation=TRUE)
# test under randomisation: randomisation=TRUE
# test under normality : randomisation=FALSE

# I-Wert selbst ist nicht interpretierbar.
# Ergebnis: Räumliche Musterung ist der emp_bev ist signifikant. Nachbargebiete neigen tendenziell zu ähnlichen Werten.
# Standardmäßig wird die Randomisierungsannahme verwendet.

# Sattelpunkt Approximation statt analytischer Normalverteilungsannahme
lm.morantest.sad(lm(emp_bev ~ 1,SPDF),listw = lw)

# Exakter Test 
lm.morantest.exact(lm(emp_bev ~ 1,SPDF),listw = lw)

#--- allgemeine globale Tests ----------------------------------
# (global) Geary's c Test
geary.test()
# global Getis-Ord G Test
globalG.test()
# spatial general cross product Mantel Test
# includes Moran's I, Geary's C and Sokal variant of Geary's c as alternative forms
sp.mantel.mc()

# join count tests for categorical variables (variable represented as factor)
joincount.test()  #same-colour joins
joincount.multi() # same-colour and diff-colour joins

# ---------------  Monte Carlo Test ----------------------------------------
# Monte Carlo Test/permutation bootstrap test 
# statistic is computed nsim times 
set.seed(1234)
MC<- moran.mc(SPDF$emp_bev, listw = lw, nsim=999)
#Benennung als MC oder bperm?
#What is the maximum value we can use for nsim??

# View results (including p-value)
MC

# Plot the distribution (note that this is a density plot instead of a histogram)
plot(MC, main="", las=1)

##############################################################################################
# Moran Scatterplot: ALTERNATIVE BERECHNUNG

n <- length(SPDF)
y <- SPDF$Bevölkerung.2014

# Transformieren w in Nachbarschaftsmatrix, welche die Intensität der räumlichen Beziehungen widerspiegelt
wmat <- nb2mat(nb_d, style="B")
colnames(wmat) <- rownames(wmat)

# Nachbarwerte abfragen
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wmat * y))
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

rwm <- mat2listw(wmat, style='W')
# Checking if rows add up to 1
# mat <- listw2mat(rwm)
# apply(mat, 1, sum)[1:15]

moran.plot(y, rwm)


# Berechnung spatially lagged values = durchschnittliche Nachbareinkommen für jedes Polygon 
# Inc_lag <- lag.listw(lw, BB$Bevölkerung.2014)
# irgendwie kein richtiges Mittel?
Inc_lag <- lag.listw(rwm, SPDF$Bevölkerung.2014)

# Wir können lagged values gegen Ursprungswerte plotten und ein lineares Regressionsmodell anpassen

# Regressionsmodell
M <- lm(Inc_lag ~ SPDF$Bevölkerung.2014)

plot(Inc_lag ~ SPDF$Bevölkerung.2014, pch=20, asp=1, las=1)

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
  x <- sample(SPDF$Bevölkerung.2014, replace=FALSE)
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

# ----------------------------------------- global morans I (based on centroids) ###################

require("ape")
#library(ape)

# Nachbarschaftsmatrix W mit inversen Distanzgewichten
point.dists <- as.matrix(dist(cbind(SPDF$c_long,SPDF$c_lat)))
point.dists.inv  <- 1/point.dists
diag(point.dists.inv) <- 0

pdi <- mat2listw(point.dists.inv, style='B')
# Checking if rows add up to 1
# mat <- listw2mat(pdi)
# apply(mat, 1, sum)[1:15]


moran(SPDF$Bevölkerung.2014, pdi, n=length(pdi$neighbours), S0=Szero(pdi))
moran.test(SPDF$Bevölkerung.2014,pdi, randomisation=TRUE)


# Monte Carlo Test
MC<- moran.mc(SPDF$Bevölkerung.2014, pdi, nsim=2999)

# Plot the distribution (note that this is a density plot instead of a histogram)
plot(MC, main="", las=1)


# Transformieren w in Nachbarschaftsmatrix, welche die Intensität der räumlichen Beziehungen widerspiegelt
# wmat <- nb2mat(nb_d, style="B")
# colnames(wmat) <- rownames(wmat)

Moran.I(SPDF$Bevölkerung.2014,point.dists.inv)

Moran.I(SPDF$Bevölkerung.2014,wmat)

#p.value=0 --> Nullhypothesis of clustering cannot be rejected (AT ALL)

# -------------- higher order contiguities #############

cor4 <- sp.correlogram(neighbours = nb_d, var = SPDF$emp_bev, order = 4, method = "I", style = "C")
plot(cor4)

# -------------- Distance bands --------------- #############
# Idea: spatial autocorrelation as a function of distance bands
# define neighbors based on distances to polygon centers
coord <- coordinates(SPDF)
# object coo stores all 430 pairs of coordinate values.
# define the search radius to include all neighboring polygon centers within 50 km (or 50,000 meters)
s_dist  <-  dnearneigh(coord, 0, 15000)  
# dnearneigh function takes 3 parameters: coordinate values coo, radius for inner and outer radius of annulus band
# here: all polygon centers up to 50km are considered neighbors
# now: identify all neighboring polygons for each polygon in the dataset
lw <- nb2listw(s_dist, style="W",zero.policy=T) 
# run MC Simulation
MI  <-  moran.mc(SPDF$Bevölkerung.2014, lw, nsim=599,zero.policy=T) 
plot(MI, main="", las=1) 
#Display p-value and other summary statistics.
MI
#Vergleich
moran.test(SPDF$Bevölkerung.2014, lw, zero.policy=T)

# package by Patrick Giradoux, correlog function is wrapper for dnearneigh() and moran.test() and automatically
# selects distance bands of ca. 10km, spanning the whole study area
library(pgirmess)
corD <- correlog(coordinates(SPDF),SPDF$Bevölkerung.2014,method = "Moran" )
plot(corD)
# in this case the first two bands of 0-10 and 10-20km have significant values

# -------------- local morans I (based on areal units) #######################
library(spdep)
# SPDF_nbq <- poly2nb(SPDF)  #queen's neighborhood
# SPDF_nbq_w <- nb2listw(SPDF_nbq) # style ="W"
# lw <- nb2listw(poly2nb(SPDF))

locm <- localmoran(SPDF$emp_bev,lw)  #calculate the local moran's I
summary(locm)

SPDF$lI <- localmoran(SPDF$emp_bev,lw)[,1]
tm_shape(SPDF) + tm_polygons(col='lI',title="Local Morans I",legend.format=list(flag="+")) +
  tm_scale_bar(width=0.15, position = c("left","bottom")) + tm_style("col_blind") +
  tm_layout(legend.position = c("right","bottom"),
            legend.text.size=0.5)
  
SPDF$pval <- localmoran(SPDF$emp_bev,lw)[,5]
tm_shape(SPDF) + tm_polygons(col='pval',title="p-value", breaks = c(0,0.01,0.05,0.10,1), border.col = "black",palette = "-Greens") +
  tm_scale_bar(width=0.15, position = c("left","bottom")) +
  tm_layout(legend.position = c("right","bottom"),
            legend.text.size=0.5)

# Multiple Hypothesis Testing
# Create the adjusted p-value
SPDF$pval_bonf <- p.adjust(SPDF$pval,method='bonferroni')

tm_shape(SPDF) + tm_polygons(col="pval_bonf",title="p-value", breaks = c(0,0.01,0.05,0.10,1), border.col = "black",palette = "-BuGn") +
  tm_scale_bar(width=0.15, position = c("left","bottom")) +
  tm_layout(legend.position = c("right","bottom"),
            legend.text.size=0.5)

# -------------- local morans I (based on centroids) #######################
# manually make a moran plot - standardize variables
SPDF$s_emp_bev <- scale(SPDF$emp_bev)  #save to a new column

# nb_qc <- poly2nb(SPDF, row.names=SPDF$id,queen=TRUE,snap=50)
# nb_d <- dnearneigh(coords, 0, 13000,row.names = SPDF$id)  #upper and lower distance bounds in m
wmat <- nb2mat(nb_d, style="B")
colnames(wmat) <- rownames(wmat)
rwm <- mat2listw(wmat, style='W')
# lw <- nb2listw(nb_d, style="W", zero.policy=TRUE)

# create a lagged variable
SPDF$lag_emp_bev <- lag.listw(rwm, SPDF$s_emp_bev)

# summary(SPDF$lag_emp_bev)

plot(x = SPDF$s_emp_bev, y = SPDF$lag_emp_bev, main = " Moran Scatterplot emp_bev")
abline(h = 0, v = 0)
abline(lm(SPDF$lag_emp_bev ~ SPDF$s_emp_bev), lty = 3, lwd = 4, col = "red")
# moran.test(SPDF$emp_bev,rwm)   #Überprüfung

# check out the outliers click on one or two and then hit escape (or click finish)
#identify(BT$s_empl_2013, soco$lab_sPPOV, soco$CNTY_ST, cex = 0.8)


# identify the moran plot quadrant for each observation
SPDF$quad_sig <- NA
SPDF@data[(SPDF$s_emp_bev >= 0 & SPDF$lag_emp_bev >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 1
SPDF@data[(SPDF$s_emp_bev <= 0 & SPDF$lag_emp_bev <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 2
SPDF@data[(SPDF$s_emp_bev >= 0 & SPDF$lag_emp_bev <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 3
SPDF@data[(SPDF$s_emp_bev >= 0 & SPDF$lag_emp_bev <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 4
SPDF@data[(SPDF$s_emp_bev <= 0 & SPDF$lag_emp_bev >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)

# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")

# see ?findInterval - This is necessary for making a map
np <- findInterval(SPDF$quad_sig, breaks)

# Assign colors to each map class
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(SPDF, col = colors[np])  #colors[np] manually sets the color for each county
mtext("Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")




