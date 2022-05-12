library(shiny)
library(tidyverse)
#library()

data <- feather::read_feather("./retail-and-retailers-sales-time-series-collection.feather")
names <- data %>% select(name) %>% distinct() %>% pull()

ui <- fluidPage(
  
  title = "Diamonds Explorer",
  
  
  
  hr(),
  
  fluidRow(
    column(3,
           h4("Diamonds Explorer"),
           sliderInput('sampleSize', 'Sample Size', 
                       min=1, max=10, value = 3),
           br(),
           checkboxInput('jitter', 'Jitter'),
           checkboxInput('smooth', 'Smooth'),
           selectizeInput('sname', choices = NULL, label = "Series name", 
                          selected = 'MRTSMPCSM4400CUSN.csv')
    ),
    #    column(4, offset = 1,
    
    #    ),
    column(6,
           plotOutput('plot')
    )
  )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'sname', choices = names, server = TRUE)
  
  df <- reactive(
    data %>% filter(name == input$sname)
  )
  output$plot <- renderPlot({
    df <- data %>% filter(name == input$sname)
    t <- df %>% select(date) %>% pull() %>% as.numeric()
    y <- df %>% select(value) %>% pull()
    
    gridsize <- length(y)
    bw <- KernSmooth::dpill(t, y, gridsize = gridsize)
    lp <- KernSmooth::locpoly(x = t, y = y, bandwidth = bw, gridsize = gridsize)
    smt <- lp$y
    df <- df %>% mutate(smooth = smt)
    
    df %>%
      ggplot() +
      geom_line(aes(x = date, y = value), alpha=0.5) +
      geom_line(aes(x = date, y = smooth), colour = "red")
    
    
  }, res = 96)
}

shinyApp(ui, server)
