library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)
library(plotly)

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Data Source
#https://data.cityofnewyork.us/Transportation/Bicycle-Routes/7vsa-caz7
nybikes.load <- readOGR("C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/Final_lgunathi/datacleaning/Bicycle Routes")
bikeracks.load <- readOGR("C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/Final_lgunathi/finalprojectmap/cityracks-shp")
df <- read.csv("Bikes_in_Buildings_Requests.csv")


ui <- navbarPage("NYC Cycling Guide",
                tabPanel("Map",
                  sidebarLayout(
                    sidebarPanel(
                  
                      
                  # Select NYC Borough
                  radioButtons("boroSelect",
                  "Borough Filter:",
                  choices = unique(sort(bikeracks.load$Borough)),
                  selected = "Bronx"),
                  
                  
                  selectInput("laneSelect",
                              "Bike Lane Type:",
                              choices = unique(nybikes.load$tf_facilit),
                              selected = c("Sharrows")),
                  
                  sliderInput("numbikes",
                              "Number of bikes Requested",
                              min = min(df$NoOfBicycleRequested,na.rm = T),
                              max = max(df$NoOfBicycleRequested,na.rm = T),
                              value = c(min(df$NoOfBicycleRequested,na.rm = T),max(df$NoOfBicycleRequested,na.rm = T)),
                              step =1)     
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
                    
            ),
            
            # Data Table Pannel
            tabPanel("Data",
                     fluidPage(plotlyOutput("plot_type"))
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
    addProviderTiles("Esri.NatGeoWorldMap") %>%
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
  
  #reactive function for lanes
  laneInfInput <- reactive({
    laneInf <- nybikes.load
    
   # req(input$boroSelect)
    
    #Boros
    #laneInf <- subset(laneInf,Borough == input$boroselect)
    
    #Lane
    if(length(input$laneSelect)>0){
      laneInf <- subset(laneInf,tf_facilit %in% input$laneSelect)      
      
    }
    return(laneInf)
    
  })
  
  #Adding the leaflet layer
  observe({
    laneInf <- laneInfInput()
    leafletProxy("leaflet",data = laneInf)%>%
    addPolylines(color = "#63CBD3", popup = ~street)
    
  })
  
  onScreen <- reactive({
    req(input$leaflet_bounds)
    bounds <- input$leaflet_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(laneInfInput()@data, latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # Reactive Data function for the bike requests
  #  Reactive data function-----------------------------
  bikereqinput <- reactive({
    bikedf <- df %>%
      filter(NoOfBicycleRequested >= input$numbikes[1] & NoOfBicycleRequested <= input$numbikes[2])
  })
  
  
  # Bar plot for Bike requests 
  output$plot_type <- renderPlotly({
    dat <- bikereqinput()
    ggplot(data = dat)+geom_bar(aes(x = RequestStatus, fill = RequestStatus))+
      xlab("Request type")+
      ylab("Number of Requests")+theme_light()
  })
  
  
}





# Run the application 
shinyApp(ui = ui, server = server)

                 
                 
                 
                 
                

