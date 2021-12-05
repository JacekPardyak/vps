#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
## Sound library
library(beepr) 

## Game parameters
skill <- 0.87 # Skill (0-1)
score <- 0
high.score <- 0
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("y_paddle",
                        "Number of bins:",
                        min = 5,
                        max = 25,
                        value = 17)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      par(mar = rep(1,4), bg = "black")
      plot.new()
      plot.window(xlim = c(0,30), ylim = c(0,30))
      lines(c(1, 30, 30, 1), c(0, 0, 30, 30), type = "l", lwd = 5, col = "white")
      
      ## Playing field boundaries (depends on cex)
      xmin <- 0.5
      xmax <- 29.4
      ymin <- 0.5
      ymax <- 29.4
      
      ## initial position
      x <- sample(5:25, 1)
      y <- sample(5:25, 1)
      points(x, y, pch = 15, col = "white", cex = 2)
      
      ## Paddle control
      psize <- 4
      ypaddle <- input$y_paddle
      
      ## Set direction
      dx <- runif(1, .5, 1)
      dy <- runif(1, .5, 1)
      
      ## Game play
      while (x > xmin - 1) {
        sound <- 0 # Silence
        Sys.sleep(.05) # Pause screen
        points(x, y, pch = 15, col = "black", cex = 2) # Erase ball
        # Move ball
        x <- x + dx
        y <- y + dy
        # Collision detection
        if (x > xmax) {
          dx <- -dx * runif(1, .9, 1.1) # Bounce
          if (x > xmin) x <- xmax # Boundary
          sound <- 10 # Set sound
        }
        if (y < ymin | y > ymax) {
          if (y < ymin) y <- ymin
          if (y > ymax) y <- ymax
          dy <- -dy * runif(1, .9, 1.1)
          sound <- 10
        }
        # Caught by paddle?
        if (x < xmin & (y > ypaddle - (psize / 2)) & y < ypaddle + (psize / 2)) {
          if (x < xmin) x <- xmin
          dx <- -dx * runif(1, .9, 1.1)
          sound <- 2
          score <- score + 1
        }
        # Draw ball
        points(x, y, pch = 15, col = "white", cex = 2)
        if (sound !=0) beep(sound)
        # Move paddle
        if (runif(1, 0, 1) < skill) ypaddle <- ypaddle + dy # Imperfect follow
        # Draw paddle
        # Erase back line
        lines(c(0, 0), c(0, 30), type = "l", lwd = 8, col = "black")
        # Keep paddle in window
        if (ypaddle < (psize / 2)) ypaddle <- (psize / 2)
        if (ypaddle > 30 - (psize / 2)) ypaddle <- 30 - (psize / 2)
        # Draw paddle
        lines(c(0, 0), c(ypaddle - (psize / 2), ypaddle + (psize / 2)), type = "l", lwd = 8, col = "white")
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
