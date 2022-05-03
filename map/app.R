library(shiny)
library(leaflet)
library(DT)

setwd("C:/Users/Arya Ramchandani/Desktop/VIST 489/Data-Visualization")

name <- c('University of Alabama at Tuscaloosa', 
                  'University of Arkansas at Fayetteville', 'Auburn University', 'University of Florida','University of Georgia', 'University of Kentucky', 'Louisiana State University at Baton Rouge', 'Mississippi State University', 'University of Missouri at Columbia', 'University of Mississippi', 'University of South Carolina at Columbia', 'Texas A&M University at College Station', 'University of Tennessee at Knoxville', 'Vanderbilt University')
layerID <- c('Alabama', 'Arkansas', 'Auburn', 'Florida', 'Georgia', 'Kentucky', 'LSU', 'Mississippi', 'Missouri', 'OleMiss', 'USC', 'TAMU', 'Tennessee', 'Vanderbilt')
lat <- c(33.21402, 36.06869, 32.59336, 29.64363, 33.94801, 38.03065, 30.41326, 33.45517, 38.94038, 34.36473, 33.99376, 30.60661, 35.95440, 36.14470)
lon <- c(-87.53914, -94.17485, -85.49517, -82.35493, -83.37732, -84.50397, -91.18000, -88.79438, -92.32774, -89.53838, -81.02992, -96.35684, -83.92946, -86.80266)

geocoded <- data.frame(name, lon, lat, layerID)

selectedSchools <-data.frame(name=geocoded$layerID, clicked=1)

icon_list <- iconList(
  
  Alabama = makeIcon("C:/Users/Arya Ramchandani/Desktop/VIST 489/Data-Visualization/images/AlabamaLogo.png", iconWidth = 24, iconHeight = 30)
)

ui <- fluidPage(
  # sliderInput(inputId = "slider",
  #             label = "values",
  #             min = 0,
  #             max = 100,
  #             value = 0,
  #             step = 1),
  leafletOutput("my_leaf", width="100%", height=400)
  #DT::dataTableOutput("table")
  
)

server <- function(input, output, session){
  
  values <- reactiveValues(selected = selectedSchools)
  observeEvent(input$my_leaf_marker_click, 
               {
                 index <- which(values$selected$name == input$my_leaf_marker_click$id)
                 
                 if(values$selected$clicked[index] == 1)
                 {
                   print("index is")
                   print(index)
                   values$selected$clicked[index] <- 0
                   
                   print("values selected is: ")
                   #$
                   print(values$selected)
                   temp <- values$selected
                   print("temp is")
                   print(temp)
                   values$selected <- temp
                   
                 }
                 else
                 {
                   print("index is")
                   print(index)
                   values$selected$clicked[index] <- 1
                   
                   print("values selected is: ")
              
                   print(values$selected)
                   temp <- values$selected
                   print("temp is")
                   print(temp)
                   values$selected <- temp
                 }
        
            
              })

  
  
output$my_leaf <- renderLeaflet({
    
    leaflet(geocoded, selectedSchools) %>%
      addTiles()%>%
      addMarkers(~lon, ~lat, layerId = ~layerID, popup = ~name, ) %>%
      # addTiles(providers$OpenStreetMap, group='Hydda.Full') %>%
      setView(lat = 35.1, lng = -90.3, zoom = 5)
      # add()
    
    
  })
  
  #output$table <- renderTable(values$selected)
  
  # index <- reactive({
  #   selectedSchools$selected[which(selectedSchools$name == input$my_leaf_marker_click$id)] <- 0
  # })
  # 
  # observe({
  #   
  #   
  #   print(input$my_leaf_marker_click$id)
  #   print(index)
  #   print(selectedSchools)
  #   
  #   
  # })
  
  # selectedRV <- reactive({
  #   
  #   print("Calling Reactive this is current SelectedSchools")
  #   print(selectedSchools)
  #   selectedSchools$selected[which(selectedSchools$name == input$my_leaf_marker_click$id)] <- 0
  #   newselectedSchools<-selectedSchools
  #   selectedSchools <- newselectedSchools
  #   
  # })
  # 
  # observe({
  #   print(selectedRV())
  # })
  
  
}

shinyApp(ui, server)