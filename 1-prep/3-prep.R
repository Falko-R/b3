# Restore SpatialDataFrame object
db_all <- readRDS(file = "1-prep/db-all.rds")
BT <- readRDS(file = "1-prep/BT.rds")
db52_1 <- readRDS(file = "1-prep/db-52_1.rds")

# Reduction of currently unused data in SpatialPolygonDF BT
db <- BT@data
rein <- c("id","AGS","GEN","SN_L","reg","Code.Amt.Typ","Amt.Stadt.und.Gemeindetyp","Code.Status","Amt.Status","Fläche.km2","c_long","c_lat","dist2bercentr","empl_2006_52_1","empl_2006" )
db1 <- db[names(db) %in% rein]

# ACHTUNG: Am 1. Januar 2014 wurde die Gemeinde „Madlitz-Wilmersdorf“ nach Briesen (Mark) eingemeindet.
# db4[!(db4$AGS %in% db1$AGS),]
# keine empl_51_1, somit ist diese Zeile egal. Im folgenden merge-Vorgang wird diese auch ignoriert

# Merge 52_1 employee data into db1
db4 <- db52_1[1:(length(db52_1$gen)-2),]
db4$gen <- NULL
UNI <- merge(db1,db4,by="AGS") 
UNI <- UNI[(order(as.numeric(UNI$id))),]
UNI$empl_2006_52_1[is.na(UNI["empl_2006_52_1"])] <- 0
UNI$empl_2006[is.na(UNI["empl_2006"])] <- 0

# ACHTUNG: Am 1. Januar 2014 wurde die Gemeinde „Madlitz-Wilmersdorf“ nach Briesen (Mark) eingemeindet.
# db5[!(db5$AGS %in% db1$AGS),]
# empl sind >0 in 2007 bis 2011, somit muss diese Zeile vorm merge-Vorgang auf Briesen addiert werden
# db5[(db5$AGS==(db1$AGS[db1$GEN %in% "Briesen"])),]
# empl_2007 empl_2008 empl_2009 empl_2010 empl_2011 empl_2012 empl_2013 empl_2014 empl_2015 empl_2016 empl_2017 empl_2018
#       130       135       135       120       115       120       150       135       130       150       180       180
 # madlitz
# empl_2007 empl_2008 empl_2009 empl_2010 empl_2011 empl_2012 empl_2013 empl_2014 empl_2015 empl_2016 empl_2017 empl_2018
#       185       305       280       270       265         0         0         0         0         0         0         0


# Merge all employee data into db1
db5 <- db_all[1:(length(db_all$gen)-2),]
db5$gen <- NULL
madlitz <- db5[!(db5$AGS %in% db1$AGS),]
#briesen <- db5[(db5$AGS==(db1$AGS[db1$GEN %in% "Briesen"])),]
#db5[(db5$AGS==(db1$AGS[db1$GEN %in% "Briesen"])),1:12] <- ................ +madlitz[1:12]

UNI2 <- merge(UNI,db5,by="AGS") 
UNI2 <- UNI2[(order(as.numeric(UNI2$id))),]


# TO CHECK:   identical(BT@data$GEN[1:430],UNI2$GEN[1:430])
BT@data <- UNI2

saveRDS(BT, file = "1-prep/BT2.rds")  # SpatialPolygonDataFrame

# kann weg
#db2 <- db1[rank(db1$AGS[1:length(db1$id)]),]
#db4 <- db52_1[rank(db52_1$AGS[1:length(db52_1$gen)]),]




