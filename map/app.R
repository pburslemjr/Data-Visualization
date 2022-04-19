library(shiny)
library(leaflet)

setwd("C:/Users/Arya Ramchandani/Desktop/VIST 489/Data-Visualization")

source("locations.r")

ui <- fluidPage(
  # sliderInput(inputId = "slider",
  #             label = "values",
  #             min = 0,
  #             max = 100,
  #             value = 0,
  #             step = 1),
  leafletOutput("my_leaf", width="100%", height=400)
)

server <- function(input, output, session){
  
  output$my_leaf <- renderLeaflet({
    
    leaflet(geocoded) %>%
      addTiles()%>%
      addMarkers(~lon, ~lat, popup=~name) %>%
      # addTiles(providers$OpenStreetMap, group='Hydda.Full') %>%
      setView(lat = 35.1, lng = -90.3, zoom = 5)
      # add()
    
  })
}

shinyApp(ui, server)