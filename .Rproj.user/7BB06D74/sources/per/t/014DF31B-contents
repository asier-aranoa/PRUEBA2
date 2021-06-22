library(leaflet)
library(rgdal)#colores en leaflet
library(RColorBrewer) #gestion paletas de color en R

# Carga de datos ----------------------------------------------------------

directorio <- 'G:/.shortcut-targets-by-id/17RLyiFCEqM5dibFCKwnx8Yk7Ao1Msqea/GradoBDATA/2020_2021_BDATA1/M331_visualizacion/materia/Reto4/meteorite'
setwd(directorio)

df <- read.csv('meteorite_coma.csv', sep=';', dec=',', header = TRUE)

#Grafico basico:

m1 <- leaflet(df) %>%
  addTiles() %>%
  addMarkers(lng=~reclong, lat=~reclat)


m1

# Grafico con marcadores "cluster"

m2 <- leaflet(df)%>%
  addTiles()%>%
  addCircleMarkers(lng=~reclong, lat=~reclat,
                   clusterOptions = markerClusterOptions() )

m2

# Grafico con variable cuantitativa (color)

pal <- colorNumeric(palette="YlGnBu", domain = df$mass)# mapeo de colores con variable numerica

m3 <- leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(lng=~reclong, lat=~reclat, color = ~pal(mass))

m3
