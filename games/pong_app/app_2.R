library( shiny )
library( shinyWidgets )

if (interactive()) {
  
  ###  basic usage ----
  

  
  ui <- fluidPage(
    
    tags$br(),
    
    noUiSliderInput(
      inputId = "y_paddle",
      min = 0, max = 100,
      value = 20,
      orientation = "vertical",
      width = "100px", height = "300px"
    ),
    verbatimTextOutput(outputId = "res1"),
    plotOutput("plot1", click = "plot_click"),
    verbatimTextOutput("info")
    
  )
  
  server <- function(input, output) {
    psize = 9
    ypaddle = 7
    output$plot1 <- renderPlot({
      ypaddle = input$y_paddle
      #ypaddle = y_paddle()
      
      par(mar = rep(1,4), bg = "black")
      plot.new()
      plot.window(xlim = c(0,30), ylim = c(0,30))
      lines(c(1, 30, 30, 1), c(0, 0, 30, 30), type = "l", lwd = 5, col = "white")
      x <- sample(5:25, 1)
      y <- sample(5:25, 1)
      points(x, y, pch = 1, col = "white", cex = 2)
      
      #points(-1, y_paddle(), pch = 1, col = "white", cex = 2)
      lines(c(0, 0), c(ypaddle - (psize / 2), ypaddle + (psize / 2)), type = "l", lwd = 8, col = "white")
      
    })
    
    
    y_paddle <- reactive({input$plot_click$y})
    
    output$info <- renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y,
             "\nextra=" , y_paddle())
    })
  }
  }
  
  shinyApp(ui, server)
  
}
