library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)

# Data Source
#https://data.cityofnewyork.us/Transportation/Bicycle-Routes/7vsa-caz7
nybikes <- readOGR("C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/Final_lgunathi/datacleaning/Bicycle Routes")
bikeracks <- readOGR("C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/Final_lgunathi/finalprojectmap/cityracks-shp")


ui <- navbarPage("NYC Cycling Guide"
                 
                 
                 
                 
)
                 
                 
                 
                 
server <- function(input, output)  {}





# Run the application 
shinyApp(ui = ui, server = server)

                 
                 
                 
                 
                

