library(shiny)
library(gsubfn)
library(stringr)
library(dplyr)
library(ggplot2)
library(rlist)

shinyServer(function(input, output) {
  curves = list(
    list(
      name = "Plant 0",
      axiom = "F",
      rules = list("F" = "F[+F]F[-F]F"),
      angle = 25.7,
      depth = 5,
      alfa0 = 90
    ),
    list(
      name = "Plant 1",
      axiom = "F",
      rules = list("F" = "FF-[-F+F+F]+[+F-F-F]"),
      angle = 22.5,
      depth = 4,
      alfa0 = 90
    ),
    list(
      name = "Plant 2",
      axiom = "X",
      rules = list("X" = "F[+X][-X]FX", "F" = "FF"),
      angle = 25.7,
      depth = 7,
      alfa0 = 90
    ),
    list(
      name = "Plant 5",
      axiom = "F",
      rules = list("F" = "F[+F]F[-F]F"),
      angle = 25.7,
      depth = 5,
      alfa0 = 90
    ),
    list(
      name = "Plant 6",
      axiom = "X",
      rules = list("X" = "F−[[X]+X]+F[+FX]−X",
                   "F" = "FF"),
      angle = 25.7,
      depth = 5,
      alfa0 = 90
    ),
    list(name="Koch snowflake",
         axiom="F",
         rules=list("F"="F+F--F+F"),
         angle=60,
         n=4,
         alfa0=180)
  )


    
  output$Iterations <- renderUI({ 
    if (input$cur != "")
    curve=list.filter(curves, name==input$cur) 
    else curve = list.filter(curves, name=="Plant 1")
    iterations = list.select(curve, depth) %>% unlist
  numericInput("ite", "Depth:", iterations, min = 1, max = (iterations+2))
  })
  
  output$Angle <- renderUI({ 
    curve=list.filter(curves, name==input$cur)
    angle=list.select(curve, angle) %>% unlist
  numericInput("ang", "Angle:", angle, min = 0, max = 360)
  })
  
  output$Rules <- renderUI({ 
    curve=list.filter(curves, name==input$cur)
    rules=list.select(curve, rules) %>% unlist
    textInput("rul", "Rules:", rules)
  })
  
  data <- eventReactive(
    input$go, {
    curve = list.filter(curves, name==input$cur)
    axiom = list.select(curve, axiom) %>% unlist
   rules = list.select(curve, rules)[[1]]$rules
   #rules = input$rul
  alfa0 = list.select(curve, alfa0) %>% unlist
  
  for (i in 1:input$ite) axiom=gsubfn(".", rules, axiom)
  actions = str_extract_all(axiom, "\\d*\\+|\\d*\\-|F|L|R|\\[|\\]|\\|") %>% unlist
  
  status=data.frame(x=numeric(0), y=numeric(0), alfa=numeric(0))
  points=data.frame(x1 = 0, y1 = 0, x2 = NA, y2 = NA, alfa=90, depth=1)
  
  
  for (action in actions) 
  {
    if (action=="F")
    {
      x=points[1, "x1"]+cos(points[1, "alfa"]*(pi/180))
      y=points[1, "y1"]+sin(points[1, "alfa"]*(pi/180))
      points[1,"x2"]=x
      points[1,"y2"]=y
      data.frame(x1 = x, y1 = y, x2 = NA, y2 = NA, 
                 alfa=points[1, "alfa"],
                 depth=points[1,"depth"]) %>% rbind(points)->points
    }
    if (action %in% c("+", "-")){
      alfa=points[1, "alfa"]
      points[1, "alfa"]=eval(parse(text=paste0("alfa",action, input$ang)))
    }
    if(action=="["){ 
      data.frame(x=points[1, "x1"], y=points[1, "y1"], alfa=points[1, "alfa"]) %>% 
        rbind(status) -> status
      points[1, "depth"]=points[1, "depth"]+1
    }
    
    if(action=="]"){ 
      depth=points[1, "depth"]
      points[-1,]->points
      data.frame(x1=status[1, "x"], y1=status[1, "y"], x2=NA, y2=NA, 
                 alfa=status[1, "alfa"],
                 depth=depth-1) %>% 
        rbind(points) -> points
      status[-1,]->status
    }
  }
  points <- points %>%  na.omit() 
  return(points)
  })
  
  output$curve <- renderPlot({    
    data() %>% ggplot() + 
      aes(x = x1, y = y1, xend = x2, yend = y2) +
      geom_segment(lineend = 'round', colour='white') + 
      coord_fixed(ratio = 1) +
      theme_void() + 
      theme(plot.background = element_rect(fill = "gray20"))
  })
  
})