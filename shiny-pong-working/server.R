# Define server logic required to draw a scene
shinyServer(function(input, output) {
  observeEvent(input$go, {
    dx <- runif(1, .5, 10)
    dy <- runif(1, .5, 10)
    RV$data$x <- RV$data$x + dx
    RV$data$y <- RV$data$y + dy
    # Collision detection
    if (RV$data$x > WIDTH) {
      dx <- -dx * runif(1, .9, 1.1) # Bounce
      RV$data$x <- RV$data$x + dx
      #   if (x > xmin) x <- xmax # Boundary
  #    sound <- 10 # Set sound
    }
#    if (y < ymin | y > ymax) {
#      if (y < ymin) y <- ymin
#      if (y > ymax) y <- ymax
#      dy <- -dy * runif(1, .9, 1.1)
#      sound <- 10
#    }
    
  }) 
  
  output$pongPlot <- renderPlot({
            par(mar = rep(1, 4), bg = "black")
            plot.new()
            plot.window(xlim = c(0, WIDTH), ylim = c(0, HEIGHT))
            
            # court
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
            # paddle - slider
            lines(
                x = c(input$paddle, input$paddle + SIZE),
                y = c(0, 0),
                type = "l",
                lwd = 5,
                col = "white"
            )
    })
    
    output$info <- renderText({
        paste0("x=", unlist(input$plot_click$x), "\ny=", input$plot_click$y)
    })
    
    
 #   observe(if (ball()['x'] %e% set(0, WIDTH) |
 #               ball()['y'] %e% set(HEIGHT) |
 #               (input$ball_y == 0 &
 #                ball()['x'] %e% interval(input$paddle, input$paddle + SIZE))) {
 #       beep(1)
 #   })
    
 #   observe(if (ball()['y'] == 0 &
 #               !ball()['x'] %e% interval(input$paddle, input$paddle + SIZE)) {
 #       beep(2)
 #   })
    
})












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
    RV$data$x <- RV$data$x + 3
    RV$data$y <- RV$data$y + 1
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

