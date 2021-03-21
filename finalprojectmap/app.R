library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)
library(plotly)
library(DT)
library(dplyr)

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Data Source
#https://data.cityofnewyork.us/Transportation/Bicycle-Routes/7vsa-caz7
nybikes.load <- readOGR("C:/Users/lakna/OneDrive/Desktop/R Shiny Operations Mgmt/Final_lgunathi/finalprojectmap/Bicycle Routes")
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
                              selected = c("Sharrows"),
                              multiple = T),

                  #Adding a Download Button
                  downloadButton("downloadData", "Download"),
                  

                  sliderInput("numbikes",
                              "Number of Bikes Requested",
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
                               body {background-color: #BCECE0;}"),
                    #Map page
                    leafletOutput("leaflet")
                    
                  )
                )  
                    
            ),
            
            # Data Table Pannel
            tabPanel("Bikes Requested in Buildings",
                     tabBox(
                       tabPanel("Bikes Requested in Buildings",plotlyOutput("plot_type")),
                       tabPanel("Building Owners",plotlyOutput("ownertype"))
    )
  ),
          tabPanel("Data",
              fluidPage(DT::dataTableOutput(outputId = "datasummary") 
                
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
    addProviderTiles("Stamen.Terrain") %>%
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
    setView(lng = boros$Longitude[1],lat = boros$Latitude[1],zoom = 12)  
  })
  
  #reactive function for lanes
  laneInfInput <- reactive({
    laneInf <- nybikes.load
    

    
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
    addPolylines(color = "#F652A0", popup = ~street)
    
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
  
  output$ownertype <- renderPlotly({
    dat <- bikereqinput()
    bikereq<- dat%>%group_by(OwnerName)%>%summarize(n=n())%>%arrange(desc(n))%>%head(10)
    ggplot(data = bikereq)+geom_bar(aes(x = OwnerName,y=n),stat = "identity")+coord_flip()
    

      
  })
  
  # Print data table if checked -------------------------------------
  output$datasummary <- DT::renderDataTable(DT::datatable(data = df[, 1:7], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
  )
  
  # Assigning the data to the download button
  data <- df
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  # Enable button once a marker has been selected
  observeEvent(input$leaflet_marker_click$id, {
    enable("delete")
  })
  
  # Add layerID to list of removed projects
  observeEvent(input$delete, {
    enable("restore")
    isolate({
      values$removed <- c(values$removed, input$leaflet_marker_click$id)
    })
  })
  # Reset removed Projects
  observeEvent(input$restore, {
    values$removed <- c()
    disable("restore")
  })
}





# Run the application 
shinyApp(ui = ui, server = server)

                 
                 
                 
                 
                

