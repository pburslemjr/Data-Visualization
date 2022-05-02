library(shiny)

possibleSchools <- list("alabama", "aurburn", "TAMU")
possibleSchoolVals <- list(1, 2, 3)
axisChoices <- list("Time", "Record")
beginMax <- 1950
endMin <-2022

ui <- fluidPage(
  checkboxGroupInput("activeSchools"),
  radioButtons("activeXAxis"),
  numericInput("dateRangeBegin"),
  numericInput("dateRangeEnd"),
  checkboxGroupInput("activeRaces"),
  checkboxGroupInput("activeGenders"),
  actionButton("update", "Update Results"),
  plotOutput("line_data"),
  plotOutput("scatter_data"),
  plotOutput("violin_data"),
  plotOutput("coordinate_data")
)


server <- function(input, output){
  #only update these reactive variables when update button is pressed
  submit <- eventReactive(input$update, {list(input$activeSchools, 
                                              input$activeXAxis,
                                              input$dateRangeBegin,
                                              input$dateRangeEnd,
                                              input$activeRaces,
                                              input$activeGenders)})
  #Visualization 1
  #Change x based on dateRange (begin and end)
  #data based off activeSchools, activeRaces/activeGenders
  output$line_data <- renderPlot(
    {
      lines(x_axis[input$activeXAxis], schoolData)
    }
  )
  
  #Visualization 2
  
  #TODO
  
  #Visualization 3
  #Change x based on activeXAxis, dateRange (begin and end)
  #data based off activeSchools, activeRaces/activeGenders
  output$scatter_data <- renderPlot()
  
  #Visualization 4
  #Change x based on  dateRange (begin and end)
  #data based off activeSchools, activeRaces/activeGenders
  output$violin_data <- renderPlot()
  
  #Visualization 5
  #Change x based on dateRange (begin and end)
  #data based off activeSchools, activeRaces/activeGenders
  output$coordinate_data <- renderPlot()
}
shinyApp(ui = ui, server = server)