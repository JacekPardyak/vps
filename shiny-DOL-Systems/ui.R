library(shiny)

shinyUI(fluidPage(
  titlePanel("Curves based on L-systems"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cur", "Choose a curve:",
                  c("","Koch Island",
                    "Quadratic Snowflake",
                    "Koch Variation 1",
                    "Koch Variation 2",
                    "Koch Variation 3",
                    "Koch Variation 4",
                    "Koch Variation 5",
                    "Koch Variation 6",
                    "Sierpinsky Triangle",
                    "Dragon Curve",
                    "Hexagonal Gosper Curve",
                    "Quadratic Gosper Curve",
                    "Koch snowflake"),
                  selected = ""),
      
      conditionalPanel(
        condition = "input.cur != \"\"",
        uiOutput("Iterations")),
      
      conditionalPanel(
        condition = "input.cur != \"\"",
        uiOutput("Angle")),
      
      conditionalPanel(
        condition = "input.cur != \"\"",
        selectInput("lic", label = "Line color:", choices = colors(), selected = "black")),
      
      
      conditionalPanel(
        condition = "input.cur != \"\"",
        selectInput("bac", label = "Background color:", choices = colors(), selected = "white")),
      
      conditionalPanel(
        condition = "input.cur != \"\"",
        actionButton(inputId = "go", label = "Go!", 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
      
      
      
      
    ), 
    mainPanel(plotOutput("curve", height="550px", width = "100%"))
  )
  
))