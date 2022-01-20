library(shiny)
library(gsubfn)
library(stringr)
library(dplyr)
library(ggplot2)
library(rlist)

app <- shinyApp(
  ui <- fluidPage(
    titlePanel("Curves based on L-systems"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "cur",
          "Choose a curve:",
          c(
            "",
            "Koch Island",
            "Cuadratic Snowflake",
            "Koch Variation 1",
            "Koch Variation 2",
            "Koch Variation 3",
            "Koch Variation 4",
            "Koch Variation 5",
            "Koch Variation 6",
            "Sierpinsky Triangle",
            "Dragon Curve",
            "Hexagonal Gosper Curve",
            "Quadratic Gosper Curve"
          ),
          selected = ""
        ),
        
        conditionalPanel(condition = "input.cur != \"\"",
                         uiOutput("Iterations")),
        
        conditionalPanel(condition = "input.cur != \"\"",
                         uiOutput("Angle")),
        
        conditionalPanel(
          condition = "input.cur != \"\"",
          selectInput(
            "lic",
            label = "Line color:",
            choices = colors(),
            selected = "black"
          )
        ),
        
        
        conditionalPanel(
          condition = "input.cur != \"\"",
          selectInput(
            "bac",
            label = "Background color:",
            choices = colors(),
            selected = "white"
          )
        ),
        
        conditionalPanel(
          condition = "input.cur != \"\"",
          actionButton(
            inputId = "go",
            label = "Go!",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          )
        )
        
        
        
        
      ),
      mainPanel(plotOutput(
        "curve", height = "550px", width = "100%"
      ))
    )
    
  )
  ,
  
server = function(input, output) {
   function(input, output) {
   curves = list(
    list(
      name = "Koch Island",
       axiom = "F-F-F-F",
       rules = list("F" = "F-F+F+FF-F-F+F"),
       angle = 90,
       n = 2,
       alfa0 = 90
     ),
     list(
       name = "Cuadratic Snowflake",
       axiom = "-F",
       rules = list("F" = "F+F-F-F+F"),
       angle = 90,
       n = 4,
       alfa0 = 90
     ),
     list(
       name = "Koch Variation 1",
       axiom = "F-F-F-F",
       rules = list("F" = "FF-F-F-F-F-F+F"),
       angle = 90,
       n = 3,
       alfa0 = 90
     ),
     list(
       name = "Koch Variation 2",
       axiom = "F-F-F-F",
       rules = list("F" = "FF-F-F-F-FF"),
       angle = 90,
       n = 4,
       alfa0 = 90
     ),
     list(
       name = "Koch Variation 3",
       axiom = "F-F-F-F",
       rules = list("F" = "FF-F+F-F-FF"),
       angle = 90,
       n = 3,
       alfa0 = 90
     ),
     list(
       name = "Koch Variation 4",
       axiom = "F-F-F-F",
       rules = list("F" = "FF-F--F-F"),
       angle = 90,
       n = 4,
       alfa0 = 90
     ),
     list(
       name = "Koch Variation 5",
       axiom = "F-F-F-F",
       rules = list("F" = "F-FF--F-F"),
       angle = 90,
       n = 5,
       alfa0 = 90
     ),
     list(
       name = "Koch Variation 6",
       axiom = "F-F-F-F",
       rules = list("F" = "F-F+F-F-F"),
       angle = 90,
       n = 4,
       alfa0 = 90
     ),
     list(
       name = "Sierpinsky Triangle",
       axiom = "R",
       rules = list("L" = "R+L+R", "R" = "L-R-L"),
       angle = 60,
       n = 6,
       alfa0 = 0
     ),
     list(
       name = "Dragon Curve",
       axiom = "L",
       rules = list("L" = "L+R+", "R" = "-L-R"),
       angle = 90,
       n = 10,
       alfa0 = 90
     ),
     list(
       name = "Hexagonal Gosper Curve",
       axiom = "L",
       rules = list("L" = "L+R++R-L--LL-R+", "R" = "-L+RR++R+L--L-R"),
       angle = 60,
       n = 4,
       alfa0 = 60
     ),
     list(
       name = "Quadratic Gosper Curve",
       axiom = "-R",
       rules = list("L" = "LL-R-R+L+L-R-RL+R+LLR-L+R+LL+R-LR-R-L+L+RR-",
                    "R" = "+LL-R-R+L+LR+L-RR-L-R+LRR-L-RL+L+R-R-L+L+RR"),
       angle = 90,
       n = 2,
       alfa0 = 90
     )
   )
   
   output$Iterations <-
     renderUI({
       if (input$cur != "")
         curve = list.filter(curves, name == input$cur)
       else
         {curve = list.filter(curves, name == "Koch Island") 
       iterations = list.select(curve, n) %>% unlist}
         numericInput("ite",
                      "Depth:",
                      iterations,
                      min = 1,
                      max = (iterations + 2))
     })
   
   output$Angle <-
     renderUI({
       curve = list.filter(curves, name == input$cur)
       angle = list.select(curve, angle) %>% unlist
       numericInput("ang", "Angle:", angle, min = 0, max = 360)
     })
   
   data <-
     eventReactive(input$go, {
       curve = list.filter(curves, name == input$cur)
       axiom = list.select(curve, axiom) %>% unlist
       rules = list.select(curve, rules)[[1]]$rules
       alfa0 = list.select(curve, alfa0) %>% unlist
       
       for (i in 1:input$ite)
         axiom = gsubfn(".", rules, axiom)
       actions = str_extract_all(axiom, "\\d*\\+|\\d*\\-|F|L|R|\\[|\\]|\\|") %>% unlist
       
       points = data.frame(x = 0, y = 0, alfa = alfa0)
       for (i in 1:length(actions))
       {
         if (actions[i] == "F" | actions[i] == "L" | actions[i] == "R")
         {
           x = points[nrow(points), "x"] + cos(points[nrow(points), "alfa"] * (pi /
                                                                                 180))
           y = points[nrow(points), "y"] + sin(points[nrow(points), "alfa"] *
                                                 (pi / 180))
           alfa = points[nrow(points), "alfa"]
           points %>% rbind(data.frame(
             x = x,
             y = y,
             alfa = alfa
           )) -> points
         }
         else{
           alfa = points[nrow(points), "alfa"]
           points[nrow(points), "alfa"] = eval(parse(text = paste0("alfa", actions[i], input$ang)))
         }
       }
       return(points)
     })
   
   output$curve <- renderPlot({
     ggplot(data(), aes(x, y)) +
       geom_path(color = input$lic) +
       coord_fixed(ratio = 1) +
       theme(
         legend.position = "none",
         panel.background = element_rect(fill = input$bac),
         panel.grid = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank()
       )
   })
   
 }
}
)

runApp(app)
