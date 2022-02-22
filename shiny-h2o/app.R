library(shiny)
library(tidyverse)
library(h2o)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(

        sidebarPanel(
            actionButton("init", "h2o.init()"),
            actionButton("predict", "h2o.predict()"),
            actionButton("shutdown", "h2o.shutdown()")
        ),
        mainPanel(
            textInput("data", "Data for prediction", "100, 10, 1, 2, 1, 4, 0, 16"),
            verbatimTextOutput("value")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(input$init, {
        h2o.init()   
    })
    observeEvent(input$shutdown, {
        h2o.shutdown(prompt = F)   
    })
    
    object <- eventReactive(input$init, {
        h2o.loadModel(path = "GLM_model_R_1645556006926_1")
        })
    
    tbl <- eventReactive(input$predict, {
        newdata <- input$data %>% I() %>%
            read_csv(col_names = c("ID", "AGE", "RACE", "DPROS", "DCAPS", "PSA", "VOL", "GLEASON"),
                     show_col_types = FALSE) %>% 
            as.h2o()
        h2o.predict(object = object(), newdata = newdata)
    })
    output$value <- renderPrint(tbl())
}

# Run the application 
shinyApp(ui = ui, server = server)
