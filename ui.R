


ui <- fluidPage(
  titlePanel(title = "NHS Data"),
  tabsetPanel(
    tabPanel(
      h4("Demographics"),
      fluidRow(
        column(
          2,
          radioButtons(
            inputId = "lockdown_input",
            label = h3("Select dates relative to first UK lockdown:"),
            choices = c(
              "Before first UK lockdown" = "pre",
              "After first UK lockdown" = "post",
              "All available dates" = "both"
            )
          )
        ),
        column(
          2,
          offset = 1,
          checkboxGroupInput(
            inputId = "age_input",
            label = h3("Select age ranges:"), 
            choices = ages_list
            )),
        column(
          6,
          plotlyOutput(
            "age_range_plot"
          )
          
        ),
        
        
      ),
      tags$br(),
      tags$hr(),
      tags$br(),
      fluidRow(
        column(
          4,
          selectInput(
            inputId =  "bar_input", 
            label = h3("Select bar style for SIMD plot:"), 
            choices = c(
              "Side-by-Side Bar" = "simd_dodge",
              "Horizontal Bar" = "simd_flip",
              "Stacked Bar" = "simd_stack")
          )
        ),
        column(
          6,
          offset = 1,
          plotlyOutput(
            "simd_plot"
          )
        )
      ),
      tags$br(),
      tags$hr(),
      tags$br(),
      fluidRow(
        column(
          2,
          radioButtons(
            inputId = "covid_simd_input",
            label = h3("Covid Infections by SIMD Quintile"),
            choices = c(
              "First Infections" = "first",
              "Reinfections" = "re"
            )
          )
          
        ),
        column(
          6, 
          offset = 3,
          plotlyOutput(
          "simd_covid_plot"
          )
        )
      )
    )
  )
)
