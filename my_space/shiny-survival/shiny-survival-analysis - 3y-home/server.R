
shinyServer(function(input, output, session) {
  selectedData <- reactive({df %>% select(all_of(input$sur_var)) %>% distinct() %>% pull()  })
  # This is a caption that will show on top of the graph; the name will change based on which variable you choose
  output$prod_caption <- renderText({
    paste("Survival Curve of Products")
  })
  
  output$group_caption <- renderText({
    paste("Survival Graph of", input$sur_var, sep="\n")
  })
  
  df_sub <- reactive({
    df  %>% filter(PROD %in% input$prod_sel)
  })
  
  
  # Running the survival function
  prod_fit <- reactive({
    survfit(as.formula("Surv(duration, event) ~ PROD"), data = df_sub())
  })
 
  group_fit <- reactive({
    survfit(as.formula(paste("Surv(duration, event) ~ ", paste(input$sur_var))), data=df)
  })
  
  # Plot the survival graph
  output$group_plot <- renderPlot({
    
    group_fit() %>% tidy() %>% ggplot(aes(time, estimate)) +
      geom_line(aes(color = strata)) + 
      geom_vline(xintercept = input$group_xvalue, linetype = "dashed") +
      labs(x = "Months since contract entry", y = "Survival")
    
  })
  
  
  # Plot the survival graph
  output$prod_plot <- renderPlot({
    
    prod_fit() %>% tidy() %>% ggplot(aes(time, estimate)) +
      geom_line(aes(color = strata)) + 
      geom_vline(xintercept = input$prod_xvalue, linetype = "dashed") +
      labs(x = "Months since contract entry", y = "Survival")
    
    })
  
  # This table will give you the probability of survival for each class at a given time
  output$group_table <- renderTable({
    summary(group_fit(), times=input$group_xvalue )[c("surv", "time", "strata")] %>% 
      data.frame() %>% arrange(desc(surv))
  })
  
  output$prod_table <- renderTable({
    summary(prod_fit(), times=input$prod_xvalue )[c("surv", "time", "strata")] %>% 
      data.frame() %>% arrange(desc(surv))
  })
  
  
})
