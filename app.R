library(shiny)

possibleSchools <- list("alabama", "auburn", "TAMU")
possibleSchoolVals <- list(1, 2, 3)
axisChoices <- list("Time", "Record")

possibleRaces <- list("Asian", "Black", "Hispanic", "Non-Hispanic", "White")
raceVals <- list("a", "b", "h", "n", "w")

possibleGenders <- list("Male", "Female")
genderVals <- list("m", "f")


ui <- fluidPage(tags$head(
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