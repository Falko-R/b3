# Vorbereitung

## Einlesen der shape- und Exceldaten

#sessionInfo()
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18362)
# Wechsel des Arbeitsordners zu Unterordner"Daten" 
#setwd(paste(getwd(),"/Daten",sep=""))

#library(maptools)
#library(ggplot2)
#library(tmap)
#library(rgdal)
#library(gridExtra)
#library(raster)

# Einlesen der shape-Dateien
# install.packages("maptools")
# library(maptools)
# require(maptools)
# Funktion readShapePoly() ist veraltet
# http://maptools.r-forge.r-project.org/reference/readShapePoly.html

#Gemeinden <- readShapePoly("1-prep/shape/VG250_GEM.shp", 
#                           proj4string = CRS("+init=epsg:25832"))

#install.packages("rgdal")
library(rgdal)
#require(rgdal)
#Einlesen aus Shapefile: Alle 11462 deutschen Gemeindegrenzen 
#Gemeinden umbenennen: spadf-brb-gem
Gemeinden <- rgdal::readOGR(dsn="1-prep/shape",layer = "VG250_GEM")

#rgdal::readOGR(dsn="C:/Users/FalkoRichter/projects/b3/1-prep/shape", layer="VG250_GEM", verbose = TRUE,p4s=CRS("+init=epsg:25832"))

# SpatialPolygonsDataFrame as spatial object class
# proj4string : [+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs]
# EPSG code: 25832

#Einlesen aus Shapefile: Berliner Bezirksgrenzen 
#bz umbenennen: spadf-ber-bez
#p4s = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
bz <- rgdal::readOGR(dsn="1-prep/shape",layer = "bz_berlin")

# Filtern nach SN_L = 12 und 11 Brandenburg und Berlin
#GemBB <- Gemeinden[Gemeinden$SN_L %in% c("11","12"), ]
Berlin <- Gemeinden[Gemeinden$SN_L %in% c("11"), ]
BBB <- Gemeinden[Gemeinden$SN_L %in% c("12"), ]


bz <- spTransform(bz, CRS("+init=epsg:25832"))
#spTransform erzeugt andere Form von proj4string
bz@proj4string <- Gemeinden@proj4string

# Wichtig, dass die Objekte in den SpatialPolygonsDataFrames (Gemeinden,plr etc) in derselben Reihenfolge vorliegen, wie die Attributdaten, die wir sp?ter hinzuf?gen wollen. Deswegen sortieren wir die Gemeinden sp?ter nach ihrer id und die Bezirke hier nach der bz_id.

bz <- bz[order(bz@data$bz_id), ] 
bz@data$AGS <- c(11001001,11002002,11003003,11004004,11005005,11006006,11007007,11008008,11009009,11010010,11011011,11012012)


# Zusammenfuehren von Berliner Bezirken und Brandenburg

emptydf <- BBB@data[1:12,]
emptydf[1:12,] <-  Berlin@data[rep(seq_len(nrow(Berlin@data)), each = 12),]

emptydf$AGS <- bz@data$AGS
emptydf$GEN <- bz@data$bz_name

bz@data <- emptydf

#change Feature ID, nicht nötig da in rbind() die IDs angepasst werden
#bz1 <- bz
#bz1 <- spChFIDs(bz, paste("bz", row.names(bz), sep="."))

BT <- rbind(bz,BBB)
#entspricht: 
#BT <- rbind.SpatialPolygonsDataFrame(bz,BBB,makeUniqueIDs = TRUE)

#Alternativen:
#one_spdf <- do.call("rbind", c(args = list.df, makeUniqueIDs = TRUE)) 

#change Feature ID hier nötig
#BT1 <- spRbind(bz1,BBB)

BT@data$id <- as.character(1:430)
BT@data$AGS <- as.integer(BT@data$AGS)

# Now Factor-Variable for INfo about Region 123
# Raumgliederung: 1=Berlin , 2= Berliner Umland , 3= Weiterer Metropolenraum

REG <- read.csv("1-prep/ecodat/01_REG.csv", 
                sep = ";", 
                dec = ",", 
                na.strings = "NA", 
                header = TRUE)

# Umwandlung der Faktor-Variablen zu Zeichenketten
REG$AGS.Bezeichnung <- as.character(REG$AGS.Bezeichnung) 
#REG$AGS <- as.character(REG$AGS) 

REG_ORD <- REG[(order(as.numeric(REG$ID))),]
#identical(BT@data$GEN,REG_ORD$AGS.Bezeichnung)
#identical(BT@data$AGS,REG_ORD$AGS)

BT@data$reg <- as.factor(REG_ORD$REG)
# BT@data[which(BT@data$reg==2),1:8]
# levels(BT@data$reg)
# want to rename the 3 levels of factor variable "reg"
# faktor Namen noch ändern! 1=Berlin , 2= Berliner Umland , 3= Weiterer Metropolenraum
# levels(BT@data$reg) <- c("Berlin","Berliner Umland","Weiterer Metropolenraum")

# CSV einlesen
WZ3 <- read.csv("1-prep/bb-data.csv", 
                stringsAsFactors=FALSE,
                sep = ";", 
                dec = ",", 
                na.strings = "NA", 
                header = TRUE)

WZ3re <- WZ3[rank(BT@data$AGS[13:430]),]

#Aussortierung der Unternehmenshaufigkeiten
von <- which((names(WZ3re))=="WZ3.05.2")
bis <- which((names(WZ3re))=="WZ3.96.0")
WZ3re[,von:bis] <- NULL
#Aussortierung der Veränderungsangaben von Jens
WZ3re[,5:10] <- NULL

df <- WZ3re[1:12,]
df[1:12,1:24]=NA

WZ3new <- rbind(df,WZ3re)
WZ3new$Gemeindename[1:12] <- as.character(c("Mitte","Friedrichshain-Kreuzberg","Pankow","Charlottenburg-Wilmersdorf","Spandau","Steglitz-Zehlendorf","Tempelhof-Schöneberg","Neukölln","Treptow-Köpenick","Marzahn-Hellersdorf","Lichtenberg","Reinickendorf"))
WZ3new$Bev.dichte[1:12] <- as.character(c("9203","13687","3786","5106","2553","2925","6426","7302","1504","4201","5262","2869"))
WZ3new$Bev.dichte <- gsub(",", "", WZ3new$Bev.dichte) # einige wenige Tausender-Kommas m?ssen entfernt werden, da es sonst bei der as.numeric-conversion seltsamerweise Probleme gibt
WZ3new$Bev.dichte <- as.numeric(WZ3new$Bev.dichte)

WZ3new$Fläche.km2[1:12] <- as.character(c("39.47","20.16","103.01","64.72","91.91","102.50","53.09","44.93","168.42","61.74","52.29","89.46"))
WZ3new$Fläche.km2 <- as.numeric(WZ3new$Fläche.km2)

# Einwohner (31. Dezember 2015)
WZ3new$Bevölkerung.2014[1:12] <- as.character(c("363236","278393","389976","330468","234630","299765","341161","328062","253333","259373","275142","256617"))
WZ3new$Bevölkerung.2014 <- as.numeric(WZ3new$Bevölkerung.2014)

#  HIER WEITERE DATEN DER BEZIRKE EINPFLEGEN !! 


# TO CHECK:   identical(BT@data$AGS[13:430],WZ3new$AGS[13:430])
BT@data <- cbind(BT@data,WZ3new[3:24])
BT@data$id <- as.character(1:430)


# Faktorvariable WZ3.52.1_cl misst H?ufigkeit in 4 abgestuften Klassen
BT@data$Bev.dichte_cl <- cut(BT@data$Bev.dichte, 
                             breaks=c(7,100,500,1000,5000,13700),
                             right=FALSE,
                             labels=c("7-99","100-499","500-999","1000-4999","5000-13700"))

# wie häufig treten die einzelnen Merkmale auf?
# table(BT@data$Bev.dichte_cl)

#Bundesland-variable wirklich nötig? Region reicht doch?
BT@data$BUNDESLAND <- cut(as.numeric(BT@data$AGS), 
                          breaks=c(0,12000000,13000000),
                          right=FALSE,
                          labels=c("BERLIN","BRANDENBURG"))


##################################

# CSV für Jahre 2013 bis 2006 einlesen
DISTR <- BT@data

# pattern = "^BB20..\\.csv$"
# nutzen glob to reg. expr. function:  glob2rx("BB20??.csv")
# $ at the end means that this is end of string. ^ means only this folder
# "dbf$" will work too, but adding \\. (. is special character in regular expressions so you need to escape it) 
# ensure that you match only files with extension .dbf (in case you have e.g. .adbf files).

bb_input_files <- list.files("1-prep/ecodat", pattern = glob2rx("BB20??.csv"),full.names = TRUE)


for (file in bb_input_files) {
  # open the data, add to DISTR and save change:
  the_data <- read.csv(file, 
                       sep = ";", 
                       dec = ",", 
                       na.strings = "NA", 
                       header = TRUE)
  
  # ?ber die gemeinsame AGS wird WZ3_52_1_2012 angef?gt
  UNT_info <- the_data[,c(2,10)]
  # employees are roughly estimated by company-size-class
  # company-size-class:    0-9  10-49 50-249  250+
  # employee-estimate:      5     30    150   250
  UNT_info$empl <- rowSums(sweep(the_data[,c(11,12,13,14)],MARGIN=2,c(5,30,150,250),`*`))
  UNI <- merge(DISTR, UNT_info ,by="AGS",all=TRUE) 
  
  # to get the year: remove everything that is not a digit from the string a_csv
  year <- gsub("\\D", "", basename(file))
  
  names(UNI)[names(UNI)=="empl"] <- paste0("empl_", year,"_52_1")
  UNI[names(UNI)=="insgesamt"] <- NULL
  #reorder
  UNI <- UNI[(order(as.numeric(UNI$id))),]
  DISTR <- UNI
}

#------------------

# NA-Werte durch Nullen ersetzen?
# von <- which(names(DISTR)=="AN2013")
# bis <- which(names(DISTR)=="AN2006")
# DISTR[,von:bis][is.na(DISTR[,von:bis])] <- 0

# die Vereinigung wird erneut in die gleiche Reihenfolge gebracht wie die Kartendaten
#UNI1 <- UNI[(order(as.numeric(UNI$id))),]

####
# Einbringen der Häufigkeitsdaten für die Berliner Bezirke

ber_input_files <- list.files("1-prep/ecodat", pattern = glob2rx("BER20??.csv"),full.names = TRUE)

for (file in ber_input_files[1:8]) {
  # open the data, add to DISTR and save change:
  the_data <- read.csv(file, 
                       sep = ";", 
                       dec = ",", 
                       na.strings = "NA", 
                       header = TRUE)
  
  # ?ber die gemeinsame AGS wird WZ3_52_1_2012 angef?gt
  UNT_info <- the_data[,c(2,10)]
  # employees are roughly estimated by company-size-class
  # company-size-class:    0-9  10-49 50-249  250+
  # employee-estimate:      5     30    150   250
  UNT_info$empl <- rowSums(sweep(the_data[,c(11,12,13,14)],MARGIN=2,c(5,30,150,250),`*`))
  UNT_info[names(UNT_info)=="insgesamt"] <- NULL
  
  # to get the year: remove everything that is not a digit from the string a_csv
  year <- gsub("\\D", "", basename(file))
  bez <- paste0("empl_", year,"_52_1")
  #names(UNI)[names(UNI)=="empl"] <- paste0("empl_", year,"_52_1")
  UNI <- merge(DISTR[c("AGS","id",bez)], UNT_info ,by="AGS",all=TRUE) 
  #transfer Berlin data from empl-col over to existing BB data in empl_2006_52_1 col
  UNI[!is.na(UNI$empl),bez] <- UNI$empl[!is.na(UNI$empl)]
  #reorder
  UNI <- UNI[(order(as.numeric(UNI$id))),]
  
  DISTR[bez] <- UNI[bez]
}

## EINLESEN DER GESAMTUNTZAHLEN an Betriebe

all_input_files <- list.files("1-prep/ecodat", pattern = glob2rx("BBBER20??GES.csv"),full.names = TRUE)

for (file in all_input_files) {
  # open the data, add to DISTR and save change:
  the_data <- read.csv(file, 
                       sep = ";", 
                       dec = ",", 
                       na.strings = "NA", 
                       header = TRUE)
  
  # ?ber die gemeinsame AGS wird WZ3_52_1_2012 angef?gt
  UNT_info <- the_data[,c(2,10)]
  # employees are roughly estimated by company-size-class
  # company-size-class:    0-9  10-49 50-249  250+
  # employee-estimate:      5     30    150   250
  UNT_info$empl <- rowSums(sweep(the_data[,c(11,12,13,14)],MARGIN=2,c(5,30,150,250),`*`))
  UNT_info[names(UNT_info)=="insgesamt"] <- NULL
  
  # to get the year: remove everything that is not a digit from the string a_csv
  year <- gsub("\\D", "", basename(file))
  
  UNI <- merge(DISTR, UNT_info ,by="AGS",all=TRUE) 
  names(UNI)[names(UNI)=="empl"] <- paste0("empl_", year)
  #UNI <- merge(DISTR[c("AGS","id","empl")], UNT_info ,by="AGS",all=TRUE) 
  #reorder
  UNI <- UNI[(order(as.numeric(UNI$id))),]
  
  DISTR <- UNI
}


BT@data <- DISTR

################################

# Erzeugung der Gemeindezentroide
BB_centroid <- as.data.frame(coordinates(BT))
# Center of Berlin
Berlin_centroid <- as.data.frame(coordinates(Berlin))

colnames(BB_centroid) <- c("c_long", "c_lat")
#colnames(Berlin_centroid) <- c("c_long", "c_lat")
Berlin@data$c_long <- Berlin_centroid[1,1]
Berlin@data$c_lat <- Berlin_centroid[1,2]

BB_centroid$id <- BT@data$id


UNI <- merge(BT@data,BB_centroid,by="id") 
UNI <- UNI[(order(as.numeric(UNI$id))),]
# TO CHECK:   identical(BT@data$AGS[1:430],UNI$AGS[1:430])
# TO CHECK:   identical(BT@data$GEN[1:430],UNI$GEN[1:430])
BT@data <- UNI


## Abstandsmessung zum Berliner Zentroiden
#Achtung unklar! wie wird Zentroid genau ermittelt???

BERCENT <- cbind((rep(Berlin@data$c_long,430)),(rep(Berlin@data$c_lat,430)))  # BERLINER ZENTROID
GEMCENT <- cbind(BT@data$c_long,BT@data$c_lat)                       # GEMEINDE-ZENTROIDE

BT@data$dist2bercentr <- apply((BERCENT-GEMCENT), 1, function(x) sqrt(sum((x[1])^2,(x[2])^2)) )
#BT@data[which((names(BT@data))=="dist2BN")] <- NULL

BT@data$GEN <- gsub(", Stadt", "", BT@data$Gemeindename)


# Save R data objects to a file

saveRDS(BT, file = "1-prep/BT.rds")  # SpatialPolygonDataFrame
#saveRDS(BB_plot, file = "1-prep/BB_plot.rds")  # Df aus aufgespaltenen Polygonen, noch zu viele SPalten!
saveRDS(Berlin, file = "1-prep/Berlin.rds")  # SpatialPolygonDataFrame?



#CLOSEST <- BT@data[(order(BT@data$dist2bercentr)),]
#FARTHEST <- BT@data[(order(BT@data$dist2bercentr, decreasing = TRUE)),]   

# ab in plot-file

library(dplyr)
test <- GemBB_plot %>% distinct(long,lat)
BB_edge <- GemBB_plot

#unique(GemBB_plot[,2:3])
test <- GemBB_plot[-(duplicated(GemBB_plot[c(2,3)])),]

#ber wird wohl nur beim plotten gebraucht und daher in plot.R erzeugt, kann hier weg
ber <- BT@data$AGS[as.numeric(BER_BB@data$AGS)<12000000]

test$group[(test$AGS %in% ber)] <- test$group[(test$AGS %in% ber)][2]  #berliner rand
test$group[!(test$AGS %in% ber)] <- test$group[!(test$AGS %in% ber)][1]   #brandenburger rand
#GemBB_plot[(GemBB_plot$AGS %in% ber),]


###########################################################################################



