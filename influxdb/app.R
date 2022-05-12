library(shiny)
library(tidyverse)
#library()

data <- feather::read_feather("./retail-and-retailers-sales-time-series-collection.feather")
names <- data %>% select(name) %>% distinct() %>% pull()

ui <- fluidPage(
  fluidRow(
    column(5,
           selectizeInput('sname', choices = NULL, label = "Series name") #, selected = 'MRTSMPCSM4400CUSN.csv')
    ),
    column(1,
           h4("TX Explorer")
    )
  ),
  fluidRow(
    column(3,
           h4("Local Polynomials"),
           sliderInput('sampleSize', 'Sample Size', 
                       min=1, max=10, value = 3),
           br(),
           checkboxInput('jitter', 'Jitter'),
           checkboxInput('smooth', 'Smooth'),
     ),
    column(8,
           plotOutput('plot_locpoly')
    )
  ),
  fluidRow(
    column(3,
           h4("Kernel Regression Smoother"),
           sliderInput('bandwidth', 'Bandwidth', 
                       min=1, max=10, value = 3)
 ),
    column(8,
           plotOutput('plot_ksmooth')
    )
  ),
 fluidRow(
   column(3,
          h4("Seasonal Decomposition by Loess"),
          sliderInput('s.window', 's.window', 
                      min=1, max=10, value = 7),
          sliderInput('s.window', 't.window', 
                      min=10, max=100, value = 50)
   ),
   column(8,
          plotOutput('plot_stl')
   )
 )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'sname', choices = names, server = TRUE)
  
  df <- reactive(
   data %>% filter(name == input$sname))
  
 output$plot_locpoly <- renderPlot({
   df <- df()
   t <- df %>% select(date) %>% pull() %>% as.numeric()
   y <- df %>% select(value) %>% pull()
   gridsize <- length(y)
   bw <- KernSmooth::dpill(t, y, gridsize = gridsize)
   smt <- KernSmooth::locpoly(x = t, y = y, bandwidth = bw, gridsize = gridsize)$y
   df %>% mutate(smooth = smt) %>%
     ggplot() +
     geom_line(aes(x = date, y = value), alpha=0.5) +
     geom_line(aes(x = date, y = smooth), colour = "red")
 })
 
 output$plot_ksmooth <- renderPlot({
   df <- df()
   t <- df %>% select(date) %>% pull() %>% as.numeric()
   y <- df %>% select(value) %>% pull()
   bw <- KernSmooth::dpill(t, y, gridsize = length(y))
   #bw <- 10^input$bandwidth 
   smt <- ksmooth(x = t, y = y, bandwidth = bw)$y
   df %>% mutate(smooth = smt) %>%
     ggplot() +
     geom_line(aes(x = date, y = value), alpha=0.5) +
     geom_line(aes(x = date, y = smooth), colour = "red")
 })
 
 output$plot_stl <- renderPlot({
   df <- df()
   y <- df %>% select(value) %>% pull()
   ts <- ts(y, frequency = 30)
   model <- stl(ts,  s.window = input$s.window, t.window = input$t.window)
   stl <- model$time.series %>% as_tibble() %>% select(trend) %>% pull()

   df %>% mutate(smooth = stl) %>%
     ggplot() +
     geom_line(aes(x = date, y = value), alpha=0.5) +
     geom_line(aes(x = date, y = smooth), colour = "red")
 })
}

shinyApp(ui, server)
