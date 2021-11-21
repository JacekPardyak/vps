library(shiny)

shinyUI(fluidPage(
  titlePanel("Plants based on L-systems"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cur", "Choose a curve:",
                  c("",
                    "Plant 0",
                    "Plant 1",
                    "Plant 2",
                    "Plant 5"),
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