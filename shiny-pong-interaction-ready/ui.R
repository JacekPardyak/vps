
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Pong R"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("pongPlot"),
            sliderInput("paddle",
                        "Paddle position:",
                        min = 0,
                        max = WIDTH - SIZE,
                        value = WIDTH/2),
            sliderInput("ball_x",
                        "Ball position X:",
                        min = 0,
                        max = WIDTH,
                        value = sample(c(1: WIDTH), 1)),
            sliderInput("ball_y",
                        "Ball position Y:",
                        min = 0,
                        max = HEIGHT,
                        value = sample(c(1: HEIGHT), 1))
        )
    )
))
