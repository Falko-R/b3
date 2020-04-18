# Vorbereitung

## Einlesen der shape- und Exceldaten

# Wechsel des Arbeitsordners zu Unterordner"Daten" 
#setwd(paste(getwd(),"/Daten",sep=""))

#library(maptools)
#library(ggplot2)
#library(tmap)
#library(rgdal)
#library(gridExtra)
#library(raster)

# Einlesen der shape-Dateien
install.packages("maptools")
#library(maptools)
#require(maptools)
# http://maptools.r-forge.r-project.org/reference/readShapePoly.html

#Gemeinden <- readShapePoly("1-prep/shape/VG250_GEM.shp", 
#                           proj4string = CRS("+init=epsg:25832"))

#install.packages("rgdal")
library(rgdal)
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
# 11000000 Berlin, Stadt (alle Bezirke)
# 11001001 Mitte
# 11002002 Friedrichshain-Kreuzberg
# 11003003 Pankow
# 11004004 Charlottenburg-Wilmersdorf
# 11005005 Spandau
# 11006006 Steglitz-Zehlendorf
# 11007007 Tempelhof-Schoeneberg
# 11008008 Neukoelln
# 11009009 Treptow-Koepenick
# 11010010 Marzahn-Hellersdorf
# 11011011 Lichtenberg
# 11012012 Reinickendorf
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
table(BT@data$Bev.dichte_cl)
##################################
#Plot Geometrie vorbereiten
################################
library(rgdal)
library(maptools)
library(ggplot2)
library(rgeos)

GemBB_geo <- fortify(BT, region = 'id') # Aufsplittung der Polygon-Objekte in data.frames
# mit den einzelnen Koordinatenpaaren

GemBB_plot <- merge(GemBB_geo,              # logische Zusammenführung 
                    BT@data,            # der Koordinatenpaare pro Polygon
                    by = "id")
# das DataFrame dient eigtl nur zum plotten, daher sollten viele Spalten raus, welche durch BT@data reingewurstet werden?

##################################
# Erzeugung der Gemeindezentroide
BB_centroid <- as.data.frame(coordinates(BT))
#BB_centroid <- as.data.frame(coordinates(BBB))
# Center of Berlin
Berlin_centroid <- as.data.frame(coordinates(Berlin))

colnames(BB_centroid) <- c("c_long", "c_lat")
#colnames(Berlin_centroid) <- c("c_long", "c_lat")
Berlin@data$c_long <- Berlin_centroid[1,1]
Berlin@data$c_lat <- Berlin_centroid[1,2]

# BB_centroid$id <- BBB@data$id
BB_centroid$id <- BT@data$id
BB_plot <- merge(GemBB_plot, 
                 BB_centroid, 
                 by = "id")

UNI <- merge(BT@data,BB_centroid,by="id") 
UNI <- UNI[(order(as.numeric(UNI$id))),]
# TO CHECK:   identical(BT@data$AGS[1:430],UNI$AGS[1:430])
# TO CHECK:   identical(BT@data$GEN[1:430],UNI$GEN[1:430])
BT@data <- UNI

# Save an object to a file
saveRDS(BT, file = "1-prep/BT.rds")
saveRDS(BB_plot, file = "1-prep/BB_plot.rds")
saveRDS(Berlin, file = "1-prep/Berlin.rds")


## Abstandsmessung zum Berliner Zentroiden
#P.S. wie wird Zentroid ermittelt?

BERCENT <- cbind((rep(Berlin@data$c_long,430)),(rep(Berlin@data$c_lat,430)))  # BERLINER ZENTROID
GEMCENT <- cbind(BT@data$c_long,BT@data$c_lat)                       # GEMEINDE-ZENTROIDE

test <- apply((BERCENT-GEMCENT), 1, function(x) sqrt(sum((x[1])^2,(x[2])^2)) )

#...weiteres....


# CSV für Jahre 2013 bis 2006 einlesen
#DISTR <- BT@data[,-c(von:bis)]
#DISTR$WZ3.52.1_2013 <- BT@data$WZ3.52.1
DISTR <- BT@data
all_input_files <- list.files("1-prep/ecodat", pattern = "*.csv",full.names = TRUE)

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
  UNI <- merge(DISTR, UNT_info ,by="AGS",all=TRUE) 
  
  # to get the year: remove everything that is not a digit from the string a_csv
  year <- gsub("\\D", "", basename(file))
  
  names(UNI)[names(UNI)=="empl"] <- paste0("empl_", year,"_52_1")
  UNI[names(UNI)=="insgesamt"] <- NULL
  #reorder
  UNI <- UNI[(order(as.numeric(UNI$id))),]
  DISTR <- UNI
}



####### trying lapply, not working yet #############
#test: a_csv <- all_input_files[1]
summarize_data <- function(a_csv, the_dir) {
  the_data <- read.csv(a_csv, 
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
  year <- gsub("\\D", "", basename(a_csv))
  
  names(UNI)[names(UNI)=="empl"] <- paste0("empl_", year,"_52_1")
  UNI[names(UNI)=="insgesamt"] <- NULL
  #reorder
  UNI <- UNI[(order(as.numeric(UNI$id))),]
  DISTR <- UNI
  
  # write the csv to a new file
  #write.csv(the_data, file = paste0(the_dir, "/", basename(a_csv)))
}

the_dir_ex <- "1-prep/ecodat"
# check_create_dir(the_dir_ex)
# get a list of all files that you want to process
# you can use a list with the lapply function
all_input_files <- list.files("1-prep/ecodat", pattern = "*.csv",
                              full.names = TRUE)

result <- lapply(all_input_files,
                 FUN = summarize_data,
                 the_dir = the_dir_ex)
invisible(lapply(all_precip_files, (FUN = summarize_data),
                 the_dir = the_dir_ex))
#------------------



