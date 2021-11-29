
shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({df %>% select(all_of(input$sur_var)) %>% distinct() %>% pull()

    #df[, c(input$sur_var)]
  })
  
  # This is a caption that will show on top of the graph; the name will change based on which variable you choose
  output$caption <- renderText({
    paste("Survival Graph of", input$sur_var, sep="\n")
  })
  
  # Running the survival function
  runSur <- reactive({
    survfit(as.formula(paste("Surv(time,delta) ~ ",paste(input$sur_var))),data=df)
  })
 
  # Plot the survival graph
  output$plot1 <- renderPlot({
    
    plot(runSur(), col = c(1:length(levels(selectedData()))), xscale=365.25, lwd=2, mark.time=TRUE,
         xlab="Years since study entry", ylab="Survival")
    legend("bottomleft", legend = levels(selectedData()),
           col = c(1:length(levels(selectedData()))), lwd=2, bty='n')
    abline(v=input$xvalue,col=1,lty=2)
    })
  
  # This table will give you the probability of survival for each class at a given time
  output$center <- renderTable({
    summary(runSur(), times=input$xvalue )[c("surv", "time", "strata")] %>% 
      data.frame() %>% arrange(desc(surv))
    #as.data.frame(summary(runSur(), times=input$xvalue )[c("surv", "time", "strata")])
  })
  
  
})
