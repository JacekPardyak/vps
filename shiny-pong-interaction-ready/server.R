

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$pongPlot <- renderPlot({
        
        par(mar = rep(1,4), bg = "black")
        plot.new()
        plot.window(xlim = c(0, WIDTH), ylim = c(0, HEIGHT))
        # fence
        lines(x = c(WIDTH, WIDTH, 0, 0), 
              y = c(0, HEIGHT, HEIGHT, 0), type = "l", lwd = 5, col = "white")
        # paddle
        lines(x = c(input$paddle, input$paddle + SIZE), 
              y = c(0, 0), type = "l", lwd = 5, col = "white")
        points(x = input$ball_x,
               y = input$ball_y, pch = 15, cex = 5, col = "white")

    })
    
    beep_1 <- observe(
    if(input$ball_x %in% c(0, WIDTH) | input$ball_y %in% c(HEIGHT) |
       ( input$ball_y == 0 & input$ball_x %e% interval(input$paddle, input$paddle + SIZE) )) {
       beep(1)
       })
    
    beep_2 <- observe(
    if(input$ball_y == 0 & ! input$ball_x %e% interval(input$paddle, input$paddle + SIZE) ) {
        beep(2)
    })
    
})




