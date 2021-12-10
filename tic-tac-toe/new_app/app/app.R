#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
    plotOutput("plot", click = "plot_click"),
    verbatimTextOutput("info")
)

server <- function(input, output) {
    output$plot <- renderPlot({
        plot(mtcars$wt, mtcars$mpg)
    }, res = 96)
    
    output$info <- renderPrint({
        req(input$plot_click)
        x <- round(input$plot_click$x, 2)
        y <- round(input$plot_click$y, 2)
        cat("[", x, ", ", y, "]", sep = "")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
