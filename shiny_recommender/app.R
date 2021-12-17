#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)

contracts_old <- I("c_id, c_name, p_id, p_name
                   1, Jacek, 1, A
                   1, Jacek, 2, B
                   2, Marko, 1, A
                   2, Marko, 3, C
                   3, Joost, 1, A
                   3, Joost, 2, B
                   4, Rafał, 1, A
                   4, Rafał, 3, C
                   5, Raghu, 1, A
                   5, Raghu, 3, C") %>% read_csv()
contracts_old <- read_csv("Contract.csv") %>% 
  mutate(customerId = as.character(customerId)) %>% 
  mutate(productId = as.character(productId)) %>% 
  select(!all_of(c('brand', 'rating')))

contracts_new <- I("c_id, c_name, p_id, p_name, rating
                   1, Jacek, 3, C, 0.96
                   2, Marko, 1, A, 0.94
                   3, Joost, 2, B, 0.65
                   4, Rafał, 2, B, 0.72
                   5, Raghu, 2, B, 0.95") %>% read_csv()

contracts_new <- read_csv("newContract.csv") %>% 
  mutate(customerId = as.character(customerId)) %>% 
  mutate(productId = as.character(productId)) %>% 
  select(!all_of('brand'))

# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("Signet recommender"),
                fluidRow(column(6, tags$h3("Purchased customer products")),
                         column(6, tags$h3("Recommended customer products"))),
                fluidRow(column(6, dataTableOutput("table_old")),
                         column(6, dataTableOutput("table_new"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$table_old <- renderDataTable({
        contracts_old
    },
    filter = 'top',
    rownames = FALSE)
    
    output$table_new <- renderDataTable({
        contracts_new
    },
    filter = 'top',
    rownames = FALSE)
}



# Run the application
shinyApp(ui = ui, server = server)
