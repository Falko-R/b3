# Kartenerstellung

## Vorbereitung

library(maptools)
library(ggplot2)
library(tmap)
library(rgdal)
library(gridExtra)
library(raster)

#library(rgdal)
#library(maptools)
library(ggplot2)
#library(rgeos)

# Restore SpatialDataFrame object
BT <- readRDS(file = "1-prep/BT.rds")
#BB_plot <- readRDS(file="1-prep/BB_plot.rds") #ersetzt GemBB_plot?
Berlin <- readRDS(file="1-prep/Berlin.rds")
#DISTR <- readRDS(file="1-prep/DISTR.rds") # nicht benötigt, da in BT@data enthalten

## Vorbereitung
GemBB_geo <- fortify(BT, region = 'id') # Aufsplittung der Polygon-Objekte in data.frames
# mit den einzelnen Koordinatenpaaren

GemBB_plot <- merge(GemBB_geo,          # logische Zusammenführung 
                    BT@data,            # der Koordinatenpaare pro Polygon
                    by = "id")
# auch join()   m?glich 
# das DataFrame dient eigtl nur zum plotten, daher sollten viele Spalten raus, welche durch BT@data reingewurstet werden?

# Erzeugung der Gemeindezentroide
BB_centroid <- as.data.frame(coordinates(BT))
# Center of Berlin
#Berlin_centroid <- as.data.frame(coordinates(Berlin))

#colnames(BB_centroid) <- c("c_long", "c_lat")
#colnames(Berlin_centroid) <- c("c_long", "c_lat")
#Berlin@data$c_long <- Berlin_centroid[1,1]
#Berlin@data$c_lat <- Berlin_centroid[1,2]

BB_centroid$id <- BT@data$id
#BB_plot gibt Gemeindezenter für Beschriftungen wieder
BB_plot <- merge(GemBB_plot, 
                 BB_centroid, 
                 by = "id")



BER_BB <- BT

# Brandenburger Außengrenze?
BB.union <- unionSpatialPolygons(BER_BB, (as.numeric(BER_BB@data$AGS))<12000000)  #spatial polygon without extra data
BB.union_geo <- fortify(BB.union, region = 'id')                 # polygon als data frame zum plotten
#BB.union.coords <- coordinates(BB.union)


## Kartenerstellung 1 

p6 <- ggplot(data = GemBB_plot,    # vorbereiteter Datensatz (fortify, merge)
             aes(x = long,         # X-Koordinaten
                 y = lat,          # Y-Koordinaten
                 group = group)) + # Zusammengehoerigkeit der Knoten pro Polygon
  geom_polygon() +                 # neue Geometrie: Polygon
  geom_path(color = "white") +     # Umrissfarbe weiss
  geom_path(data = BB.union_geo,   # Datensatz mit Berliner und Brandenburger Grenzumrandung
            aes(x = long,
                y = lat,
                group = group),
            #  fill = NA,
            color = "red",
            size = 0.5) +
  coord_equal() +                  # Skalierung der X- & Y-Koordinaten gleich
  labs(x = "Rechtswert",           # Beschriftungen und Titel
       y = "Hochwert") + 
  ggtitle("Gemeinden in Brandenburg")
p6

png(filename="3-plot/Gemeinden.png")
#plot(p6)
print(p6)
dev.off()
#dev.print(pdf, 'filename.pdf')

## Kartenerstellung 2

BB_area <- raster::aggregate(BER_BB,c('BUNDESLAND','id'))
#BB_area <- raster::aggregate(BER_BB,'BUNDESLAND')

BB_area_geo <- fortify(BB_area, region = 'BUNDESLAND') 
#BB_area_geo <- fortify(BB_area, region = c('BUNDESLAND','id'))

# Faktorvariable WZ3.52.1_cl misst H?ufigkeit in 4 abgestuften Klassen
GemBB_plot$AN2013_cl <- cut(GemBB_plot$empl_2013_52_1, 
                          breaks=c(0,1,15,30,50,100,200,300,500,701),
                          right=FALSE,
                          labels=c("0","1-14","15-29","30-49","50-99","100-199","200-299","300-499","500-700"))

# wie h?ufig treten die einzelnen Merkmale auf?
# table(BER_BB@data$AN2013_cl)

p7 <- ggplot() + labs(x = "Rechtswert",  y = "Hochwert") + ggtitle("WZ3.52.1 Lagereiunternehmen \nin BB-Gemeinden und BER-Bezirken in 2013")
p7 <- p7+ geom_polygon(data = GemBB_plot,    # vorbereiteter Datensatz (fortify, merge)
                       aes(x = long,         # X-Koordinaten
                           y = lat,          # Y-Koordinaten
                           group = group,    # Zusammengehoerigkeit der Knoten pro Polygon
                           fill = AN2013_cl),
                       color = "white", 
                       alpha = .7,
                       size = 0.1) + 
  geom_path(color = "white") +     # Umrissfarbe weiss
  coord_equal()                  # Skalierung der X- & Y-Koordinaten gleich ?nochmal?

p7 <- p7 + geom_path(data = BB_area_geo,
                     aes(x = long,
                         y = lat,
                         group = group),
                     #  fill = NA,
                     color = "red",
                     size = 0.5) 

p7  <- p7 +  labs(fill = "Anzahl \nArbeitnehmer \n(Schätzung)") + 
  theme(
    plot.title = element_text(lineheight=.8, face="bold"),
    axis.line = element_blank(),          # Entfernung der Hintergrundlinien
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_blank()) +
  scale_fill_brewer()
p7


## Kartenerstellung 3
# absteigende Sortierung der empl und Speicherung der 10 ersten Werte in Variable "top"
top <- (BT@data$AGS[order(BT@data$empl_2013_52_1,
                          decreasing = TRUE)])[1:10]

p9 <- ggplot() +                             # neue Geometrie: Polygon
  geom_polygon(data = GemBB_plot,            # vorbereiteter Datensatz (fortify, merge)
               aes(x = long,                 # X-Koordinaten
                   y = lat,                  # Y-Koordinaten
                   group = group,            # Zusammengehoerigkeit der Knoten pro Polygon
                   fill = AN2013_cl),          # farbliche Unterschiedung gem?? Variable WZ3.52.1_cl
               color = "white", 
               alpha = .7,
               size = 0.1) + 
  geom_text(data = BB_plot[(BB_plot$AGS %in% top),], 
            aes(label = GEN, 
                x = c_long, 
                y = c_lat), 
            size = 2, 
            color = "black") + 
  coord_equal() +                             # Skalierung der X- & Y-Koordinaten gleich
  labs(fill = "Unternehmenshäufigkeit") +     # Beschriftungen und Titel
  ggtitle("Häufigkeit 52.1 in Brandenburg") + 
  theme(
    axis.line = element_blank(),              # Entfernung der Hintergrundlinien
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_blank()) +
  scale_fill_brewer(palette="Spectral")
p9


########### Kartendarstellung4





# absteigende Sortierung der Bev-dichte und Speicherung der 20 ersten Werte in Variable "top"
top <- (BT@data$AGS[order(BT@data$Bev.dichte,
                          decreasing = TRUE)])[1:20]
ber <- BT@data$AGS[as.numeric(BT@data$AGS)<12000000]


p9 <- ggplot() +                             # neue Geometrie: Polygon
  geom_polygon(data = BB_plot,            # vorbereiteter Datensatz (fortify, merge)
               aes(x = long,                 # X-Koordinaten
                   y = lat,                  # Y-Koordinaten
                   group = group,            # Zusammengehoerigkeit der Knoten pro Polygon
                   fill = Bev.dichte_cl),          # farbliche Unterschiedung gem?? Variable WZ3.52.1_cl
               color = "white", 
               alpha = .7,
               size = 0.1) + 
  geom_polygon(data = BB_plot[(BB_plot$AGS %in% ber),],
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "red",
               size = 0.5) +
  #   geom_text(data = BB_plot[(BB_plot$AGS %in% top),], 
  #             aes(label = Gemeindename, 
  #                 x = c_long, 
  #                 y = c_lat), 
  #             size = 2, 
  #             color = "black") + 
  coord_equal() +                             # Skalierung der X- & Y-Koordinaten gleich
  labs(fill = "Bev. je km?") +     # Beschriftungen und Titel
  ggtitle("Bev-dichte in Brandenburg") + 
  theme(
    axis.line = element_blank(),              # Entfernung der Hintergrundlinien
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_blank()) +
  scale_fill_brewer()
p9

p8 <- ggplot() +                             # neue Geometrie: Polygon
  geom_polygon(data = BB_plot,               # vorbereiteter Datensatz (fortify, merge)
               aes(x = long,                 # X-Koordinaten
                   y = lat,                  # Y-Koordinaten
                   group = group,            # Zusammengehoerigkeit der Knoten pro Polygon
                   fill = WZ3.52.1_cl),      # farbliche Unterschiedung gem?? Variable WZ3.52.1_cl
               color = "white", 
               alpha = .7,
               size = 0.1) + 
  geom_text(data = BB_plot[(BB_plot$AGS %in% top),], 
            aes(label = Gemeindename, 
                x = c_long, 
                y = c_lat), 
            size = 2, 
            color = "white") + 
  coord_equal() +                             # Skalierung der X- & Y-Koordinaten gleich
  labs(fill = "Unternehmensh?ufigkeit") +     # Beschriftungen und Titel
  ggtitle("H?ufigkeit 52.1 in Brandenburg") + 
  theme(
    axis.line = element_blank(),              # Entfernung der Hintergrundlinien
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_blank()) +
  scale_fill_brewer(palette="Set1")
p8