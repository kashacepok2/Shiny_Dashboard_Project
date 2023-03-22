



ui <- fluidPage(
  titlePanel(title = "NHS Data"),
  theme = shinytheme("flatly"),
  tabsetPanel(
    tabPanel("A&E Activity", 
             
             fluidRow(
               
               HTML("<br><br><br>"),
               
               HTML("<h3><b> Yearly </b> </h3>"),
               
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("measure_1", "", choices = c("Patients attending" = "number_of_attendances_aggregate","Patients meeting care target" = "number_meeting_target_aggregate"))
                 ),
                 mainPanel(
                   plotOutput("year_plot"),
                   dataTableOutput("table_output_2")
                 )
               )
             ),
             
             fluidRow(
               
               HTML("<br><br><br>"),
               
               HTML("<h3> <b> Seasonal </b> </h3>"),
               
               mainPanel(
                 #selectInput("period", "Pandemic or Pre-pandemic", choices = c("Pandemic", "Pre-pandemic")),
                 radioButtons("measure_2", "", choices = c( "Patients attending" = "number_of_attendances_aggregate", "Patients meeting care target" = "number_meeting_target_aggregate")),
                 plotOutput("a_e_plot"),
                 dataTableOutput("table_output")
               )
             )
    ), 
    
    tabPanel("Hospital Activity", 
             
             fluidRow(
               
               HTML("<br><br><br>"),
               
               HTML("<h3> <b> Yearly for Planned and Emergency Patients </b> </h3>"),
               
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("mes_1", "", choices = c("Total average episodes" = "episodes","Total average length of stay" = "length_of_stay"))
                 ),
                 mainPanel(
                   plotOutput("year_p"),
                   dataTableOutput("table_1")
                 )
               )
             ),
             
             fluidRow(
               HTML("<br><br><br>"),
               
               HTML("<h3> <b> Seasonal for Planned and Emergency Patients </b> </h3>"),
               
               mainPanel(
                 radioButtons("period", "", choices = c("Pandemic" = "Pandemic","Prior to pandemic" = "Pre-pandemic")),
                 radioButtons("mes_2", "", choices = c("Total average episodes" = "episodes","Total average length of stay" = "length_of_stay")),
                 plotOutput("season_p"),
                 dataTableOutput("table_2")
               )
             ),
             
             fluidRow(
               HTML("<br><br><br>"),
               
               HTML("<h3> <b> Intensive Care Medicine Before and During the Pandemic </b> </h3>"),
               
               mainPanel(
                 #selectInput("period_2", "Before or During", choices = c("Pandemic" = "Pandemic","Before Pandemic" = "Pre-pandemic")),
                 radioButtons("measure_3", "", choices = c("Total average episodes" = "episodes","Total average length of episodes" = "length_of_episode")),
                 plotOutput("season_i"),
                 dataTableOutput("table_3")
               )
             )
    ), 
    
    tabPanel("Bed Occupancy", 
             
             fluidRow(
               HTML("<br><br><br>"),
               
               HTML("<h3> <b> Yearly </b> </h3>"),
               
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("m_1", "", choices = c("Percentage of available beds occupied" = "percentage_occupancy","Average number of beds occupied" = "average_occupied_beds"))
                 ),
                 mainPanel(
                   plotOutput("year_b"),
                   dataTableOutput("table_b")
                 )
               )
             ), 
             fluidRow( 
               
               HTML("<br><br><br>"),
               
               HTML("<h3> <b> Seasonal </b> </h3>"),
               
               mainPanel(
                 radioButtons("period_1", "", choices = c("Pandemic" = "Pandemic","Prior to pandemic" = "Pre-pandemic")),
                 radioButtons("m_2", "", choices = c("Percentage of available beds occupied" = "percentage_occupancy","Average number of beds occupied" = "average_occupied_beds")),
                 plotOutput("season_b"),
                 dataTableOutput("table_b2")
               )
             )
    ),
    tabPanel(
      ("Demographics"),
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
  ))

