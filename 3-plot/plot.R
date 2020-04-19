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
BB_plot <- readRDS(file="1-prep/BB_plot.rds") #ersetzt GemBB_plot
Berlin <- readRDS(file="1-prep/Berlin.rds")
DISTR <- readRDS(file="1-prep/DISTR.rds")

## Kartenerstellung 1 

p6 <- ggplot(data = BB_plot,    # vorbereiteter Datensatz (fortify, merge)
             aes(x = long,         # X-Koordinaten
                 y = lat,          # Y-Koordinaten
                 group = group)) + # Zusammengehoerigkeit der Knoten pro Polygon
  geom_polygon() +                 # neue Geometrie: Polygon
  geom_path(color = "white") +     # Umrissfarbe weiss
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