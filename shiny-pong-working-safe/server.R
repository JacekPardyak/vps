


# Define server logic required to draw a scene
shinyServer(function(input, output) {
    
    time <- reactive(
        Sys.time()
    )

    autoInvalidate <- reactiveTimer(DELAY)

    ball = reactive({
        autoInvalidate()
        
    #    step = step + 1
        print(paste("The paddle position:", isolate(time())))
    #    print(paste("The paddle position:", isolate(input$paddle)))
        
        #start = get_ball(list(sample(c(1:WIDTH), 1), sample(c(1:HEIGHT), 1)))
        #end  = get_ball(start)
        #end  = get_ball(end)
        list(x = sample(c(0: WIDTH), 1), y = sample( c(0: HEIGHT), 1))
    })
    
    # Generate a new scene each time the timer fires, but not when paddle moves
    
    
#    for(i in c(1:100)){
#    Sys.sleep(0.1)
    output$pongPlot <- renderPlot({
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
        # paddle - slider
        lines(
            x = c(input$paddle, input$paddle + SIZE),
            y = c(0, 0),
            type = "l",
            lwd = 5,
            col = "white"
        )
        
        points(
            x =  150, 
            y = ball()[['y']], #sample(c(1:300), 1),
            pch = 15,
            cex = 5,
            col = "white"
        )
        
    })
    #}
    
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
