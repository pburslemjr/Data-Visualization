library(shiny)
library(leaflet)


# setting up all variables needed
name <- c('University of Alabama at Tuscaloosa', 
          'University of Arkansas at Fayetteville', 'Auburn University', 'University of Florida','University of Georgia', 'University of Kentucky', 'Louisiana State University at Baton Rouge', 'Mississippi State University', 'University of Missouri at Columbia', 'University of Mississippi', 'University of South Carolina at Columbia', 'Texas A&M University at College Station', 'University of Tennessee at Knoxville', 'Vanderbilt University')
layerID <- c('Alabama', 'Arkansas', 'Auburn', 'Florida', 'Georgia', 'Kentucky', 'LSU', 'Mississippi', 'Missouri', 'OleMiss', 'USC', 'TAMU', 'Tennessee', 'Vanderbilt')
lat <- c(33.21402, 36.06869, 32.59336, 29.64363, 33.94801, 38.03065, 30.41326, 33.45517, 38.94038, 34.36473, 33.99376, 30.60661, 35.95440, 36.14470)
lon <- c(-87.53914, -94.17485, -85.49517, -82.35493, -83.37732, -84.50397, -91.18000, -88.79438, -92.32774, -89.53838, -81.02992, -96.35684, -83.92946, -86.80266)

geocoded <- data.frame(name, lon, lat, layerID)

selectedSchools <-data.frame(name=geocoded$layerID, clicked=1)





possibleSchools <- list("alabama", "auburn", "TAMU")
possibleSchoolVals <- list(1, 2, 3)
axisChoices <- list("Time", "Record")

possibleRaces <- list("Asian", "Black", "Hispanic", "Non-Hispanic", "White")
raceVals <- list("a", "b", "h", "n", "w")

possibleGenders <- list("Male", "Female")
genderVals <- list("m", "f")


ui <- fluidPage(
  
  #drawing map
  leafletOutput("my_leaf", width="100%", height=400),
  tags$head(
  tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')),
),
fluidRow(column(width = 12, 
  plotOutput("map"))),
  fluidRow(
    column(width = 12,
      splitLayout(
        verticalLayout(
          plotOutput("line_data"),
          plotOutput("scatter_data"), 
        ),
        verticalLayout(
          plotOutput("violin_data"),
          plotOutput("coordinate_data")
        )
      )
    )
  ),
  fluidRow (
    column(width = 4,
       radioButtons("activeXAxis", "Axis", choices = axisChoices)),
    column(width = 4,
       checkboxGroupInput("activeRaces", "Select Race", choiceNames = possibleRaces, choiceValues = raceVals)),
    column(width = 4,
       checkboxGroupInput("activeGenders", "Select Genders", choiceNames = possibleGenders, choiceValues = genderVals)),
    column(width = 6,
       numericInput("dateRangeBegin", "Begin", value = 1950)),
    column(width = 6,
       numericInput("dateRangeEnd", "End", value = 2022))
    )
  
)

server <- function(input, output){
  
  values <- reactiveValues(selected = selectedSchools)
  observeEvent(input$my_leaf_marker_click, 
  {
     index <- which(values$selected$name == input$my_leaf_marker_click$id)
     
     if(values$selected$clicked[index] == 1)
     {
       values$selected$clicked[index] <- 0
       temp <- values$selected
       values$selected <- temp
       print(values$selected)
       
     }
     else
     {
       
       values$selected$clicked[index] <- 1
       temp <- values$selected
       values$selected <- temp
       print(values$selected)
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
  
  races <- reactive({
    Ai <- as.numeric(is.element('a', input$activeRaces))
    B <- as.numeric(is.element('b', input$activeRaces))
    H <- as.numeric(is.element('h', input$activeRaces))
    X <- as.numeric(is.element('n', input$activeRaces))
    W <- as.numeric(is.element('w', input$activeRaces))
    A <- as.numeric(length(input$activeRaces) == 5)
    data.frame(Ai, B, H, X, W, A)
    
  })
  
  genders <- reactive({
    M <- as.numeric(is.element('m', input$activeGenders))
    F <- as.numeric(is.element('f', input$activeGenders))
    B <- as.numeric(length(input$activeGenders) == 2)
    data.frame(M, F, B)
  })
  
  output$map <- renderPlot({
    hist(rnorm(10))}
  )
  
  output$line_data <- renderPlot({
    hist(rnorm(10))}
  )
  
  output$scatter_data <- renderPlot(
    hist(rnorm(10))
  )
  
  output$violin_data <- renderPlot(
    hist(rnorm(10))
  )
  
  output$coordinate_data <- renderPlot(
    hist(rnorm(10))
  )

  #Visualization 1
  #Change x based on dateRange (begin and end)
  #data based off activeSchools, activeRaces/activeGenders
  # output$line_data <- renderPlot(
  #   {
  #     lines(x_axis[input$activeXAxis], schoolData)
  #   }
  # )
  # 
  # 
  # #Visualization 2
  # 
  # #TODO
  # 
  # #Visualization 3
  # #Change x based on activeXAxis, dateRange (begin and end)
  # #data based off activeSchools, activeRaces/activeGenders
  # output$scatter_data <- renderPlot(
  #   
  # )
  # 
  # #Visualization 4
  # #Change x based on  dateRange (begin and end)
  # #data based off activeSchools, activeRaces/activeGenders
  # output$violin_data <- renderPlot(
  #   
  # )
  # 
  # #Visualization 5
  # #Change x based on dateRange (begin and end)
  # #data based off activeSchools, activeRaces/activeGenders
  # output$coordinate_data <- renderPlot(
  #   
  # )
}
shinyApp(ui = ui, server = server)