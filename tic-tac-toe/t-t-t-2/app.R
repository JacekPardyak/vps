#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
if (!require("sets")) install.packages("sets")
library(sets)
if (!require("tictactoe")) install.packages("tictactoe")
library(tictactoe)

HEIGHT = 300
WIDTH = 300 

#  |A|B|C
# P|
# R|
# S|

A = interval(        0, WIDTH/3, "[)")
B = interval(  WIDTH/3, 2*WIDTH/3, "[)")
C = interval(2*WIDTH/3, WIDTH, "[]")

P = interval(2*HEIGHT/3, HEIGHT, "[]")
R = interval(  HEIGHT/3, 2*HEIGHT/3, "[)")
S = interval(         0, HEIGHT/3, "[)")

get_position <- function(p, q){
    x = interval(p, p, "[]")
    y = interval(q, q, "[]")
    position = 0
    position = ifelse(interval_intersection(A, x) == x & interval_intersection(P, y) == y, 1, position)
    position = ifelse(interval_intersection(A, x) == x & interval_intersection(R, y) == y, 2, position)
    position = ifelse(interval_intersection(A, x) == x & interval_intersection(S, y) == y, 3, position)
    position = ifelse(interval_intersection(B, x) == x & interval_intersection(P, y) == y, 4, position)
    position = ifelse(interval_intersection(B, x) == x & interval_intersection(R, y) == y, 5, position)
    position = ifelse(interval_intersection(B, x) == x & interval_intersection(S, y) == y, 6, position)
    position = ifelse(interval_intersection(C, x) == x & interval_intersection(P, y) == y, 7, position)
    position = ifelse(interval_intersection(C, x) == x & interval_intersection(R, y) == y, 8, position)
    position = ifelse(interval_intersection(C, x) == x & interval_intersection(S, y) == y, 9, position)
    position
    }

ttt_rv <- reactiveValues()
ttt_rv$x = 0
ttt_rv$y = 0
ttt_rv$position = 0
ttt_rv$game <- ttt_game()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tic-Tac-Toe Game"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot", click = "plot_click"),
           verbatimTextOutput("stats"),
           tableOutput("game")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observe({
        ttt_rv$x <- input$plot_click$x
        ttt_rv$y <- input$plot_click$y
        ttt_rv$position <- get_position(input$plot_click$x, input$plot_click$y)
    })
    
    observe({
        position = 7
        ttt_rv$game$play(position)
        
    })
    
    output$distPlot <- renderPlot({

        
        par(mar = rep(1, 4), bg = "black")
        plot.new()
        plot.window(xlim = c(0, WIDTH), ylim = c(0, HEIGHT))
        
        # court
        lines(
            x = c(0       ,   WIDTH ),
            y = c(HEIGHT/3, HEIGHT/3),
            type = "l",
            lwd = 5,
            col = "white"
        )
        
        lines(
            x = c(0         , WIDTH     ),
            y = c(2*HEIGHT/3, 2*HEIGHT/3),
            type = "l",
            lwd = 5,
            col = "white"
        )
        
        lines(
            x = c(WIDTH/3, WIDTH/3),
            y = c(0      , HEIGHT),
            type = "l",
            lwd = 5,
            col = "white"
        )
        
        lines(
            x = c(2*WIDTH/3, 2*WIDTH/3),
            y = c(0        , HEIGHT),
            type = "l",
            lwd = 5,
            col = "white"
        )
        
        # ball
 #       points(
 #           x = plot_click$x, 
 #           y = plot_click$y,
 #           pch = 4, # 4 -cross , 1 - circle
 #           cex = 15,
 #           lwd = 5,
 #           col = "white"
 #       )        
        
    })
    
    output$stats <- renderText({
#        paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
        paste0("x=", input$plot_click$x, "\ny=", ttt_rv$y, "\nposition=", ttt_rv$position)
    })
    
    output$game <- renderTable({    
        ttt_rv$game$show_board() %>% capture.output() %>% I() %>% read_delim(delim = " ")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
