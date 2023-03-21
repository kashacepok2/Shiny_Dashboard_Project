


ui <- fluidPage(
  titlePanel("Covid App"),
  tabsetPanel(
    tabPanel(
      "Demographics",
      fluidRow(
        column(
          2,
          radioButtons(
            inputId = "lockdown_input",
            label = h3("Select dates relative to first UK lockdown:"),
            choices = c(
              "Before first UK lockdown" = "pre",
              "After first UK lockdown" = "post",
              "Both" = "both"
            )
          )
        ),
        column(
          2,
          checkboxGroupInput(
            inputId = "age_input",
            label = h3("Select age ranges:"), 
            choices = ages_list
            ))
        ),
        column(
          3,
          offset = 4,
          selectInput(
            inputId =  "bar_input", 
            label = h3("Select bar style for SIMD plot:"), 
            choices = c(
              "Side-by-side Bar" = "simd_dodge",
              "Horizontal Bar" = "simd_flip",
              "Stacked Bar" = "simd_stack")
          )
        )
      ),
      tags$hr(),
      fluidRow(
        column(
          6,
          plotlyOutput(
            "age_range_plot"
          )
        ),
        column(
          6,
          plotlyOutput(
            "simd_plot"
          )
        )
      )
    )
  )
