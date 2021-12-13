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
if (!require("plot.matrix")) install.packages("plot.matrix")
library(plot.matrix)
HEIGHT = 300
WIDTH = 300 

#  |A|B|C
# S|
# R|
# P|

A = interval( 0.5, 1.5, "[)")
B = interval( 1.5, 2.5, "[)")
C = interval( 2.5, 3.5, "[]")

S = interval( 0.5, 1.5, "[)")
R = interval( 1.5, 2.5, "[)")
P = interval( 2.5, 3.5, "[]")

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
ttt_rv$state = matrix(rep(0, 9), nrow = 3)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tic-Tac-Toe Game"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            verbatimTextOutput("info")),
# Show a plot of the generated distribution
        mainPanel(
    #       plotOutput("distPlot", click = "plot_click"),
           plotOutput("matrixPlot", click = "plot_click"),
           verbatimTextOutput("matrixInfo")
            

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$info <- renderPrint({
        req(input$plot_click)
        x <- round(input$plot_click$x, 2)
        y <- round(input$plot_click$y, 2)
        cat("Click position: ", "[", x, ", ", y, "]", "\n", sep = "")
        ttt_rv$position <- get_position(input$plot_click$x, input$plot_click$y)
        cat("Matrix position: ",  ttt_rv$position , "\n")
        cat("Is move legal?: ",  ttt_rv$game$play(ttt_rv$position), "\n") # and game has not been over.
        cat("Board:\n ")
        cat(ttt_rv$game$show_board(), "\n")
        cat("Current state:\n")
        cat(ttt_rv$state, "\n")
        cat("Next state:\n")
        cat(ttt_rv$game$next_state(position = 1 + ttt_rv$position), "\n")
        ttt_rv$state <- ttt_rv$game$state
    })

    #observe({
    #    ttt_rv$state <- ttt_rv$game$state
    #  })
    
        
    output$matrixPlot <- renderPlot({
        state <- ttt_rv$state
        state[state == 0] <- '0'
        state[state == 1] <- '1'
        state[state == 2] <- '2'
        col <- colorRampPalette(c("grey", "white", "black"))
        #plot(cst$residuals, breaks=c(-7.5,7.5))
        plot(state, col=col) #, key=NULL

    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
