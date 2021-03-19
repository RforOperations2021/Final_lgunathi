require(rgdal)
require(leaflet)
require(leaflet.extras)

require(dplyr)
require(readxl)
require(stringr)
library(sf)
library(geojsonio)

setwd("C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/Final_lgunathi/datacleaning")
nybikes <- readOGR("C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/Final_lgunathi/datacleaning/Bicycle Routes")







leaflet(data = nybikes) %>%
  addProviderTiles("Esri.NatGeoWorldMap")%>%
  addPolylines(color = "#63CBD3",popup = ~street)