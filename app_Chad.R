library(shiny)

setwd("C:\\Users\\CRAUST~1\\DOCUME~1\\GitHub\\DATA-V~1")

possibleSchools <- list("alabama", "auburn", "TAMU")
possibleSchoolVals <- list(1, 2, 3)
axisChoices <- list("Time", "Record")


ui <- fluidPage(sidebarLayout(sidebarPanel(
      checkboxGroupInput("activeSchools", "Selected Schools", choices = possibleSchools),
      radioButtons("activeXAxis", "Axis", choices = axisChoices),
      checkboxGroupInput("activeRaces", "Select Race", choices = possibleSchools),
      checkboxGroupInput("activeGenders", "Select Genders", choices = possibleSchools),
      numericInput("dateRangeBegin", "Begin", value = 1950),
      numericInput("dateRangeEnd", "End", value = 2022)
  ), 
  mainPanel(
      plotOutput("line_data"),
      plotOutput("scatter_data"),
      plotOutput("violin_data"),
      plotOutput("coordinate_data")
    )
  )
)


server <- function(input, output){
  

fullData<-read.csv('fullData.csv')
widerData<-read.csv('widerData.csv')

plotData <- reactive({
  subset(fullData, 
         fullData$race %in% races[races[,2]==1,1] & fullData$gender %in% gender[gender[,2]==1,1])
})

  output$line_data <- renderPlot(
    plot_ly() %>% 
      add_trace(x=plotData$year,
                y=plotData$grad_100_rate,type='scatter',
                color=allRaceallGender$chronname,
                mode='lines') %>% 
      layout(title="On-time Graduation Rates by Year and University")
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