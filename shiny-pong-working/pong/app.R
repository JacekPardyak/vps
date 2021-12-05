library(shiny)
HEIGHT = 300
WIDTH = 300 
SIZE = 30
ui <- basicPage(

plotOutput("pongPlot"),
sliderInput("paddle",
            "Paddle position:",
            min = 0,
            max = WIDTH - SIZE,
            value = WIDTH/2)
)

server <- function(input, output) {
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
        # ball
        points(
            x = 0, 
            y = 0,
            pch = 15,
            cex = 5,
            col = "white"
        )
})
}    

shinyApp(ui, server)