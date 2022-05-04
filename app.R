library(shiny)
library(plotly)
library(leaflet)

# setwd("C:\\Users\\CRAUST~1\\DOCUME~1\\GitHub\\DATA-V~1")

name <- c('University of Alabama at Tuscaloosa', 
          'University of Arkansas at Fayetteville', 'Auburn University', 'University of Florida','University of Georgia', 'University of Kentucky', 'Louisiana State University at Baton Rouge', 'Mississippi State University', 'University of Missouri at Columbia', 'University of Mississippi', 'University of South Carolina at Columbia', 'Texas A&M University at College Station', 'University of Tennessee at Knoxville', 'Vanderbilt University')
layerID <- c('Alabama', 'Arkansas', 'Auburn', 'Florida', 'Georgia', 'Kentucky', 'LSU', 'Mississippi State', 'Missouri', 'Ole Miss', 'South Carolina', 'Texas A&M', 'Tennessee', 'Vanderbilt')
lat <- c(33.21402, 36.06869, 32.59336, 29.64363, 33.94801, 38.03065, 30.41326, 33.45517, 38.94038, 34.36473, 33.99376, 30.60661, 35.95440, 36.14470)
lon <- c(-87.53914, -94.17485, -85.49517, -82.35493, -83.37732, -84.50397, -91.18000, -88.79438, -92.32774, -89.53838, -81.02992, -96.35684, -83.92946, -86.80266)

geocoded <- data.frame(name, lon, lat, layerID)

selectedSchools <-data.frame(name=geocoded$layerID, clicked=1)


possibleSchools <- list("alabama", "auburn", "TAMU")
possibleSchoolVals <- list(1, 2, 3)
axisChoices <- list("Time", "Record")

possibleRaces <- list("Asian", "Black", "Hispanic", "Non-Hispanic", "White")
raceVals <- list("a", "b", "h", "n", "w")
races <-  list("Ai", "B", "H", "X", "W")

possibleGenders <- list("Male", "Female")
genderVals <- list("m", "f")
genders <- list("M", "F")


ui <- fluidPage(
  
  tags$head(
    tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')),
  ),
  fluidRow(column(width = 12, style='padding:20px;', 
                  leafletOutput("my_leaf", width="100%", height=400))),
  fluidRow(
    column(width = 12, 
           splitLayout(
             verticalLayout(
               plotlyOutput("line_data"),
               plotlyOutput("scatter_data"), 
             ),
             verticalLayout(
               plotlyOutput("violin_data"),
               plotlyOutput("coordinate_data")
             )
           )
    )
  ),
  fluidRow (
    column(width = 6,
           checkboxGroupInput("activeRaces", "Select Race", choiceNames = possibleRaces, choiceValues = races, selected = races)),
    column(width = 6,
           checkboxGroupInput("activeGenders", "Select Genders", choiceNames = possibleGenders, choiceValues = genders, selected = genders)),
    column(width = 6,
           numericInput("dateRangeBegin", "Begin", value = 2002, min = 2002, max = 2013)),
    column(width = 6,
           numericInput("dateRangeEnd", "End", value = 2013, min = 2002, max = 2013))
  )
  
)

server <- function(input, output, session){
  
  schoolIconList <- iconList(
    
    
    "Alabama" = makeIcon(iconUrl = "images/Logos/AlabamaLogo.png", 
                         iconWidth = 30, 
                         iconHeight = 30),
    
    "Arkansas" = makeIcon(iconUrl = "images/Logos/ArkansasLogo.png", 
                          iconWidth = 30, 
                          iconHeight = 30),
    
    "Auburn" = makeIcon(iconUrl = "images/Logos/AuburnLogo.png", 
                        iconWidth = 30, 
                        iconHeight = 30),
    
    "Florida" = makeIcon(iconUrl = "images/Logos/FloridaLogo.png", 
                         iconWidth = 30, 
                         iconHeight = 30),
    
    "Georgia" = makeIcon(iconUrl = "images/Logos/GeorgiaLogo.png", 
                         iconWidth = 30, 
                         iconHeight = 30),
    
    "Kentucky" = makeIcon(iconUrl = "images/Logos/KentuckyLogo.png", 
                          iconWidth = 30, 
                          iconHeight = 30),
    
    "LSU" = makeIcon(iconUrl = "images/Logos/LSULogo.png", 
                     iconWidth = 30, 
                     iconHeight = 30),
    
    "Mississippi State" = makeIcon(iconUrl = "images/Logos/MissLogo.png", 
                                   iconWidth = 30, 
                                   iconHeight = 30),
    
    "Missouri" = makeIcon(iconUrl = "images/Logos/MissouriLogo.png", 
                          iconWidth = 30, 
                          iconHeight = 30),
    
    "Ole Miss" = makeIcon(iconUrl = "images/Logos/OleMissLogo.png", 
                          iconWidth = 30, 
                          iconHeight = 30),
    
    "South Carolina" = makeIcon(iconUrl = "images/Logos/SouthCarolinaLogo.png", 
                                iconWidth = 30, 
                                iconHeight = 30),
    
    "Texas A&M" = makeIcon(iconUrl = "images/Logos/TamuLogo.png", 
                           iconWidth = 30, 
                           iconHeight = 30),
    
    "Tennessee" = makeIcon(iconUrl = "images/Logos/TenLogo.png", 
                           iconWidth = 30, 
                           iconHeight = 30),
    
    "Vanderbilt" = makeIcon(iconUrl = "images/Logos/VandyLogo.png", 
                            iconWidth = 30, 
                            iconHeight = 30)
    
  )
  
  schoolIconListRemo <- iconList(
    
    
    "Alabama" = makeIcon(iconUrl = "images/Logos/AlabamaLogo.png", 
                         iconWidth = 15, 
                         iconHeight = 15),
    
    "Arkansas" = makeIcon(iconUrl = "images/Logos/ArkansasLogo.png", 
                          iconWidth = 15, 
                          iconHeight = 15),
    
    "Auburn" = makeIcon(iconUrl = "images/Logos/AuburnLogo.png", 
                        iconWidth = 15, 
                        iconHeight = 15),
    
    "Florida" = makeIcon(iconUrl = "images/Logos/FloridaLogo.png", 
                         iconWidth = 15, 
                         iconHeight = 15),
    
    "Georgia" = makeIcon(iconUrl = "images/Logos/GeorgiaLogo.png", 
                         iconWidth = 15, 
                         iconHeight = 15),
    
    "Kentucky" = makeIcon(iconUrl = "images/Logos/KentuckyLogo.png", 
                          iconWidth = 15, 
                          iconHeight = 15),
    
    "LSU" = makeIcon(iconUrl = "images/Logos/LSULogo.png", 
                     iconWidth = 15, 
                     iconHeight = 15),
    
    "Mississippi State" = makeIcon(iconUrl = "images/Logos/MissLogo.png", 
                                   iconWidth = 15, 
                                   iconHeight = 15),
    
    "Missouri" = makeIcon(iconUrl = "images/Logos/MissouriLogo.png", 
                          iconWidth = 15, 
                          iconHeight = 15),
    
    "Ole Miss" = makeIcon(iconUrl = "images/Logos/OleMissLogo.png", 
                          iconWidth = 15, 
                          iconHeight = 15),
    
    "South Carolina" = makeIcon(iconUrl = "images/Logos/SouthCarolinaLogo.png", 
                                iconWidth = 15, 
                                iconHeight = 15),
    
    "Texas A&M" = makeIcon(iconUrl = "images/Logos/TamuLogo.png", 
                           iconWidth = 15, 
                           iconHeight = 15),
    
    "Tennessee" = makeIcon(iconUrl = "images/Logos/TenLogo.png", 
                           iconWidth = 15, 
                           iconHeight = 15),
    
    "Vanderbilt" = makeIcon(iconUrl = "images/Logos/VandyLogo.png", 
                            iconWidth = 15, 
                            iconHeight = 15)
    
  )
  
  
  
  
  fullData<-read.csv('fullData.csv')
  widerData<-read.csv('widerData.csv')
  fullData$raceAndGender <- paste(fullData$race,fullData$gender)
  
  values <- reactiveValues(selected = selectedSchools)
  mapProxy <- leafletProxy(mapId = "my_leaf", session)
  
  observeEvent(input$my_leaf_marker_click, 
               {
                 index <- which(values$selected$name == input$my_leaf_marker_click$id)
                 
                 if(values$selected$clicked[index] == 1)
                 {
                   values$selected$clicked[index] <- 0
                   temp <- values$selected
                   values$selected <- temp
                   print(values$selected)
                   removeMarker(map = mapProxy, layerId = values$selected$name[index])
                   addMarkers(map = mapProxy, lng = geocoded$lon[index], 
                              lat = geocoded$lat[index], popup = 
                                values$selected$name[index], 
                              layerId = values$selected$name[index],
                              icon = schoolIconListRemo[index])
                   
                 }
                 else
                 {
                   
                   values$selected$clicked[index] <- 1
                   temp <- values$selected
                   values$selected <- temp
                   print(values$selected)
                   
                   removeMarker(map = mapProxy, layerId = values$selected$name[index])
                   addMarkers(map = mapProxy, lng = geocoded$lon[index], 
                              lat = geocoded$lat[index], popup = 
                                values$selected$name[index], 
                              layerId = values$selected$name[index],
                              icon = schoolIconList[index])
                   
                 }
                 
                 
               })
  
  output$my_leaf <- renderLeaflet({
    
    leaflet(geocoded, selectedSchools) %>%
      addTiles()%>%
      addMarkers(~lon, ~lat, layerId = ~layerID, icon = ~schoolIconList[geocoded$layerID], popup = ~name) %>%
      # addTiles(providers$OpenStreetMap, group='Hydda.Full') %>%
      setView(lat = 35.1, lng = -90.3, zoom = 5)
    # add()
    
    
  })
  
  finalNames <- reactive({
    names <- values$selected$name
    clicked <- values$selected$clicked
    data.frame(names, clicked)
    
  })
  
  races <- reactive({
    if (length(input$activeRaces) == 5)
    {
      append(input$activeRaces, "A")
    } else
    {
      input$activeRaces
    }
  })
  
  genders <- reactive({
    if (length(input$activeGenders) == 2)
    {
      
      append(input$activeGenders, "B")
      input$activeGenders
    } else
    {
      input$activeGenders
      
    }
  })
  
  observeEvent(input$dateRangeEnd, {
    updateNumericInput(inputId = "dateRangeBegin", max = input$dateRangeEnd - 1)
  })
  
  observeEvent(input$dateRangeBegin, {
    updateNumericInput(inputId = "dateRangeEnd", min = input$dateRangeBegin + 1)
  })
  
  
  # races <- reactive({
  #   Ai <- as.numeric(is.element('a', input$activeRaces))
  #   B <- as.numeric(is.element('b', input$activeRaces))
  #   H <- as.numeric(is.element('h', input$activeRaces))
  #   X <- as.numeric(is.element('n', input$activeRaces))
  #   W <- as.numeric(is.element('w', input$activeRaces))
  #   A <- as.numeric(length(input$activeRaces) == 5)
  #   data.frame(Ai, B, H, X, W, A)
  #   
  # })
  # 
  # genders <- reactive({
  #   M <- as.numeric(is.element('m', input$activeGenders))
  #   F <- as.numeric(is.element('f', input$activeGenders))
  #   B <- as.numeric(length(input$activeGenders) == 2)
  #   data.frame(M, F, B)
  # })
  
  
  ###### Line 1 ######
  
  plotDataLine <- reactive({
    subset(fullData, 
           fullData$race == 'A' & fullData$gender == 'B' & fullData$chronname %in% finalNames()$names[finalNames()$clicked == 1] & fullData$year<=input$dateRangeEnd & fullData$year>=input$dateRangeBegin)
  })
  
  output$line_data <- renderPlotly(
    plot_ly() %>% 
      add_trace(x=plotDataLine()$year,
                y=plotDataLine()$grad_100_rate,
                type='scatter',
                color=plotDataLine()$chronname,
                mode='lines') %>% layout(title="On-time Graduation Rates by Year and University")
  )
  
  ###### Scatter 1 ######
  
  lm3 <- lm(fullData$grad_100_rate ~ fullData$total.wins + fullData$chronname + fullData$raceAndGender)
  
  scatterIndex<-reactive({
    (fullData$race %in% races()& 
       fullData$gender %in% genders() &
       fullData$chronname %in% finalNames()$names[finalNames()$clicked == 1] &
       fullData$year<=input$dateRangeEnd & 
       fullData$year>=input$dateRangeBegin
       ) 
  })
    
  plotDataScatter <- reactive({
    fullData[scatterIndex(),]
  })

  plotDatafitted <- reactive({
    fitted(lm3)[scatterIndex()]  
  })
  
  axisCh<-c("Time","Record")
  realAxis<-c("year","total.wins")
  axisChooser<-data.frame(axisCh,realAxis)
  i<- reactive({
    axisChooser[axisChooser$axisCh==input$activeXAxis,2]
  })
  output$scatter_data <- renderPlotly(
    
    plot_ly() %>% 
      add_trace(x=plotDataScatter()$total.wins,
                y=plotDatafitted(),
                type='scatter',
                mode='markers', 
                color=plotDataScatter()$chronname)%>% 
      layout(title="Model Predictions by University")
    
  )
  
  output$violin_data <- renderPlotly(
    fig <- plotDataScatter() %>%
      plot_ly(type = 'violin')  %>%
      
      add_trace(
        x = ~chronname[plotDataScatter()$gender == 'M'],
        y = ~total.wins[plotDataScatter()$gender == 'M'],
        legendgroup = 'M',
        scalegroup = 'M',
        width=plotDataScatter()$grad_100_rate[plotDataScatter()$gender == 'M'],
        name = 'M',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        ),
        color = I("blue")
      )  %>%
       add_trace(
    x = ~chronname[plotDataScatter()$gender == 'F'],
    y = ~total.wins[plotDataScatter()$gender == 'F'],
    legendgroup = 'F',
    scalegroup = 'F',
    width=plotDataScatter()$grad_100_rate[plotDataScatter()$gender == 'F'],
    name = 'F',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ),
         color = I("pink")
       ) %>%
       layout(
         xaxis = list(title = 'On-Time Graduation Percentage'), 
         yaxis = list(title = 'Total Football Wins'),
         violinmode = 'group'
       )

  )
  
  output$coordinate_data <- renderPlotly(
    plot_ly() %>% 
      add_trace(x=plotDataScatter()$total.wins,
                y=plotDatafitted(),
                type='scatter',
                mode='markers', 
                color=plotDataScatter()$raceAndGender) %>% 
      layout(title="Model Predictions by Race and Gender")
    
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