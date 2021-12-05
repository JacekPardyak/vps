
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
            plotOutput("pongPlot", click = "plot_click"),
            sliderInput("paddle",
                        "Paddle position:",
                        min = 0,
                        max = WIDTH - SIZE,
                        value = WIDTH/2),
            sliderInput("ball_x",
                        "Ball speed:",
                        min = 0,
                        max = 111,
                        value = 10),
            sliderInput("ball_y",
                        "Ball position Y:",
                        min = 0,
                        max = HEIGHT,
                        value = sample(c(1: HEIGHT), 1)),
            verbatimTextOutput("info")
        )
    )
))
