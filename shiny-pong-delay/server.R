# Define server logic required to draw a scene

ball <- reactiveValues()

# Assign values ball and move
ball$x <- 280
ball$y <- 150

ball$dx = 1 #(10 + runif(1)) * (-1) ^ sample(c(0, 1), 1)
ball$dy = 1 #(10 + runif(1)) * (-1) ^ sample(c(0, 1), 1)

shinyServer(function(input, output) {
 
  session <- reactiveValues()
#  session$timer <- reactiveTimer(Inf)
  
  observeEvent(input$go,{
    session$timer <- reactiveTimer(30)
    observeEvent(session$timer(),{
      forward()
    })
  })
  
  forward <- function(){
      if (ball$x <= 0 | ball$x >= WIDTH){
        ball$dx = -1 * ball$dx
     #beep(1)
        }
        ball$x = ball$x + ball$dx
        ball$x = min(max(ball$x, 0), WIDTH)
    
        if (ball$y <= 0 | ball$y >= HEIGHT){
          ball$dy = -1 * ball$dy
     #beep(1)
        }
        ball$y = ball$y + ball$dy
        ball$y = min(max(ball$y, 0), HEIGHT) 

  }
    

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
              x = ball$x, 
              y = ball$y,
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
        paste0("x=", ball$x , "\ny=", ball$y,
               "\ndx=", ball$dx , "\ndy=", ball$dy  )
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

