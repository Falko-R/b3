library(tidyverse)

# Einlesen der Anzahl an WZ3.52.1 Unternehmen und Abschätzung der Beschäftigungszahlen
file <- "1-prep/ecodat/db/DB07-18-52_1.csv"
# open the data, add to DISTR and save change:
db <- read.csv(file ,sep = ";", dec = ",", na.strings = "NA", header = TRUE, stringsAsFactors = FALSE)

db[c("Datenbasis","X","Wirtschaftszweig..WZ2008.")] <- NULL
names(db) <- c("year","gen","size","amt")
db <- db[1:(length(db$year)-3),] 

#levels(db$size)[2]

db$est <- sapply(db$size, switch, 
                 "0  bis     9" = "5", 
                 "10  bis   49" = "30", 
                 "50  bis 249" = "150", 
                 "250 und mehr" = "250")

#db %>% 
#  mutate(est = case_when(
#    size == "0  bis     9" ~ 5,
#    cyl == 8 & disp > median(disp) ~ "8 cylinders, large displacement",
#    TRUE ~ "other"
#  )
#)



#changelevels <- function(f, ...) {
#  f <- as.factor(f)
#  levels(f) <- list(...)
#  f
#}
#db$est <- changelevels(db$size, "5"="0  bis     9", "30"="10  bis   49", "150"="50  bis 249" , "250"="250 und mehr")
#db[,"est"] <- as.numeric(as.character(db[,"est"]))
db[,"est"] <- as.numeric(db[,"est"])
db[,"amt"] <- as.numeric(db[,"amt"])
db$amt[is.na(db["amt"])] <- 0
db$empl <- db$amt*db$est
db1 <- aggregate(empl ~ year+gen, data=db, FUN=sum, na.rm=TRUE)  # aggregate(. ~gen,db,sum) with "." meaning "aggregate all" columns wrt "gen"

#alternativ:
#library(dplyr)
#library(tidyr)
#db2 <- db %>% group_by(gen,year) %>% summarize(sum_empl = sum(empl))

db3 <- spread(db1,year,empl)

db3 <- db3[2:(length(db3$gen)),]
db3$AGS <-  as.integer(1:(length(db3$gen)))
db3$AGS[1:12] <-   c(11001001,11002002,11003003,11004004,11005005,11006006,11007007,11008008,11009009,11010010,11011011,11012012)
db3$AGS[13:(length(db3$gen)-2)] <- as.integer(paste0("120", gsub("\\D", "", db3$gen[13:(length(db3$gen)-2)])))
db3$AGS[(length(db3$gen)-1):(length(db3$gen))] <-   c(11000000,12000000)

# number anywhere in the string
#as.integer(gsub("([0-9]+).*$", "\\1", db3$gen[13:(length(db3$gen)-2)]))

#gsub("\\D", "", db3$gen[13:(length(db3$gen)-2)])

years <- gsub("\\D", "", names(db3))
names(db3)[years!=""] <- paste0("empl_", years,"_52_1")[years!=""]

# Save R data objects to a file
saveRDS(db3, file = "1-prep/db-52_1.rds")  # R-dataframe derived from database-csv
# Restore SpatialDataFrame object
# db52_1 <- readRDS(file = "1-prep/db-52_1.rds")


# Einlesen der Anzahl an allen Unternehmen und Abschätzung der Beschäftigungszahlen
file <- "1-prep/ecodat/db/DB07-18-all.csv"
# open the data, add to DISTR and save change:
db <- read.csv(file ,sep = ";", dec = ",", na.strings = "NA", header = TRUE, stringsAsFactors = FALSE)

db[c("Datenbasis","X")] <- NULL
names(db) <- c("year","gen","size","amt")
db <- db[1:(length(db$year)-3),] 

db$est <- sapply(db$size, switch, 
                 "0  bis     9" = "5", 
                 "10  bis   49" = "30", 
                 "50  bis 249" = "150", 
                 "250 und mehr" = "250")

db[,"est"] <- as.numeric(db[,"est"])
db[,"amt"] <- as.numeric(db[,"amt"])
db$amt[is.na(db["amt"])] <- 0
db$empl <- db$amt*db$est
db1 <- aggregate(empl ~ year+gen, data=db, FUN=sum, na.rm=TRUE)  # aggregate(. ~gen,db,sum) with "." meaning "aggregate all" columns wrt "gen"
db3 <- spread(db1,year,empl)

db3 <- db3[2:(length(db3$gen)),]
db3$AGS <-  as.integer(1:(length(db3$gen)))
db3$AGS[1:12] <-   c(11001001,11002002,11003003,11004004,11005005,11006006,11007007,11008008,11009009,11010010,11011011,11012012)
db3$AGS[13:(length(db3$gen)-2)] <- as.integer(paste0("120", gsub("\\D", "", db3$gen[13:(length(db3$gen)-2)])))
db3$AGS[(length(db3$gen)-1):(length(db3$gen))] <-   c(11000000,12000000)

years <- gsub("\\D", "", names(db3))
names(db3)[years!=""] <- paste0("empl_", years)[years!=""]

# Save R data objects to a file
saveRDS(db3, file = "1-prep/db-all.rds")  # R-dataframe derived from database-csv
# Restore SpatialDataFrame object
# db-all <- readRDS(file = "1-prep/db-all.rds")