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
bikeracks.load <- readOGR("C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/Final_lgunathi/finalprojectmap/cityracks-shp")



ui <- navbarPage("NYC Cycling Guide",
                tabPanel("Map",
                  sidebarLayout(
                    sidebarPanel(
                  
                      
                  # Select NYC Borough
                  radioButtons("boroSelect",
                  "Borough Filter:",
                  choices = unique(sort(bikeracks.load$Borough)),
                  selected = "Bronx")
                  
                  ),
                  
                  # Map Panel
                  mainPanel(
                    shinyjs::useShinyjs(),
                    # Style the background and change the page
                    tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                               body {background-color: #D4EFDF;}"),
                    #Map page
                    leafletOutput("leaflet")
                    
                  )
                )  
                    
            )
)

                 
                 
                 
                 
server <- function(input, output)  {
  #Basic Map
  #output$leaflet <- renderLeaflet({
   # leaflet(data = nybikes) %>%
    #  addProviderTiles("Esri.WorldTerrain")%>%
    #  addPolylines(color = "#63CBD3",popup = ~street) %>%
    #  setView(-74.0060, 40.7128, 9) 
    
    
  #})
  output$leaflet <- renderLeaflet({
  leaflet() %>%
    addProviderTiles("Esri.WorldTerrain") %>%
    #addCircleMarkers(data = bikeracks, lng = ~Longitude, lat = ~Latitude,radius = 0.5)%>%
    setView(-74.0060, 40.7128, 9) 
  })
  #borough filtered data
  
  
  # Borough Filter
  boroInputs <- reactive({
    boros <- subset(bikeracks.load, Borough == input$boroSelect)
    
    return(boros)
    
    })
  
  # Replace the layer with filtered bike rack data
  observe({
    boros <- boroInputs()
    leafletProxy("leaflet", data = boros) %>%
    clearGroup(group = "boros")%>%
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude,radius = 0.5,group = "boros")%>%
    setView(lng = boros$Longitude[1],lat = boros$Latitude[1],zoom = 9)  
  })
  
}





# Run the application 
shinyApp(ui = ui, server = server)

                 
                 
                 
                 
                

