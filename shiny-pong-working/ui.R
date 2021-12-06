
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Pong R"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            actionButton("go", "Go"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("pongPlot", click = "plot_click"),
            sliderInput("paddle",
                        "Paddle position:",
                        min = 0,
                        max = WIDTH - SIZE,
                        value = WIDTH/2),
            verbatimTextOutput("info"),
            useShinyjs(),
            p(id = "text", "This text will disappear after 3 seconds"),
            actionButton("close", "Close the app in half a second")
        )
    )
))
