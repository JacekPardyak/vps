library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("N", label = "N", value = 10)
    ),
    mainPanel(
      plotOutput("posteriors"),
      hr(),
      ## limit the height of the progress message box
      tags$head(tags$style("#progress{overflow-y:scroll; max-height: 500px;}")),
      verbatimTextOutput("progress")
    )
  )
)

server <- function(input, output, session) {
  
  ## file to write progress
  tfile <- tempfile(fileext = ".txt")
  
  ## reactive values
  r <- reactiveValues(
    progress_mtime = -1
  )
  
  observeEvent(input$N, {
    ## start sampling in background process
    r$bg_process <- callr::r_bg(
      ## this is a long running computation
      func = function(N) {
        rstan::sampling(
          object = shinyStanModels:::stanmodels[["lm"]],
          data = list(N = N, x = seq_len(N), y = rnorm(N, seq_len(N), 0.1)),
          open_progress = FALSE,
          iter = 1000,
          chains = 2
        )
      },
      args = list(N = input$N),
      stdout = tfile,
      supervise = TRUE
    )
    ## start polling bg process
    r$poll <- TRUE   
  })
  
  ## observe status of bg process
  observe({
    req(r$bg_process, r$poll)
    ## keep re-executing observer as 
    ## long as bg process is running
    invalidateLater(millis = 1000, session)
    ## read current progress if file is modified
    mtime <- file.info(tfile)$mtime
    if(mtime > r$progress_mtime) {
      r$progress <- readLines(tfile) 
      r$progress_mtime <- mtime
    }
    ## extract draws when bg process is finished
    if(!r$bg_process$is_alive()) {
      r$draws <- r$bg_process$get_result()
      r$poll <- FALSE  ## stop polling bg process
    } 
  })
  
  ## plot histograms
  output$posteriors <- renderPlot({
    req(r$draws)
    op <- par(mfrow = c(1, 2), cex = 1.25)
    hist(rstan::extract(r$draws, "alpha")[[1]], main = bquote("Posterior samples"~alpha), xlab = expression(alpha))
    hist(rstan::extract(r$draws, "beta")[[1]], main = bquote("Posterior samples"~beta), xlab = expression(beta))
    par(op)
  })
  
  ## print progress
  output$progress <- renderText({
    req(r$progress)
    paste(r$progress, collapse = "\n")
  })
  
}

shinyApp(ui = ui, server = server)