library(shiny)
# ---- To get mouse events, run once
# library(devtools)
# devtools::install_github("shiny", "jcheng5", ref="plot-mouse-events")

shinyUI(pageWithSidebar(

  headerPanel('Diagnostics of outliers in regression'),

  sidebarPanel(
    helpText("Choose the sample index of the point you would like to move, then click the new location of the point on the graph"),
    sliderInput('i', 'Sample index:', min = 1, max = 30, value = 10)
  ),

  mainPanel(
    plotOutput('diagPlot', clickId="coords", hoverId="hover", height="auto", width="100%"),
    tableOutput("clickinfo"), 
    tableOutput("hoverinfo")
  )
))
