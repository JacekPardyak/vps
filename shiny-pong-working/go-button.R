library(shiny)
HEIGHT = 300
WIDTH = 300 
SIZE = 30

RV <- reactiveValues(data = list(x = 150, y = 150))

ui <- fluidPage(
  actionButton("go", "Go"),
  plotOutput("plot")
)

server <- function(input, output) {
  
  observeEvent(input$go, {
    dx <- runif(1, .5, 1)
    dy <- runif(1, .5, 1)
    
    RV$data$x <- RV$data$x + dx
    RV$data$y <- RV$data$y + dy
  }) 
  
  output$plot <- renderPlot({
    par(mar = rep(1, 4), bg = "black")
    plot.new()
    plot.window(xlim = c(0, WIDTH), ylim = c(0, HEIGHT))
    
    # fence
    lines(
      x = c(WIDTH, WIDTH, 0, 0),
      y = c(0, HEIGHT, HEIGHT, 0),
      type = "l",
      lwd = 5,
      col = "white"
    )
    # ball
    points(
      x = RV$data$x, 
      y = RV$data$y,
      pch = 15,
      cex = 5,
      col = "white"
    )
  })
}

shinyApp(ui, server)