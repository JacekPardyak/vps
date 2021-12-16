shinyUI(navbarPage(
  "Survival Analysis",
  tabPanel(
    "Kaplan-Meier Survival Curves Groups",
    sidebarLayout(sidebarPanel(
      h3("Survivial Graph"),
      selectInput('sur_var', 'Factor of Survival', setdiff(
        names(df), c("duration", "event", "id", "PROD")
      )),
      sliderInput(
        'group_xvalue',
        'Survival time (months)',
        value = 9,
        min = 1,
        max = max(df$duration)
      )
    )
    ,
    mainPanel(
      h3(textOutput("group_caption")),
      plotOutput("group_plot"),
      tableOutput("group_table")
      ))
    ),
    tabPanel(
      "Kaplan-Meier Survival Curves Products",
      sidebarLayout(
        sidebarPanel(
          h3("Survivial Graph"),
          selectizeInput(
            'prod_sel',
            label = 'Products of Survival',
            choices = prod_all,
            selected = list("domain-reg-nl", "domain-reg-be"),
            multiple = TRUE,
            options = list(maxItems = 3, placeholder = 'pick at least 2 products')
          ),
          sliderInput(
            'prod_xvalue',
            'Survival time (months)',
            value = 9,
            min = 1,
            max = max(df$duration)
          )
        )
        ,
        mainPanel(
          h3(textOutput("prod_caption")),
          plotOutput("prod_plot"),
          tableOutput("prod_table")
        )
      )
    )
    ))
  