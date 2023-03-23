ui <- fluidPage(
  titlePanel(title = "Scottish Health Data Dashboard"),
  theme = shinytheme("flatly"),
  tabsetPanel(
    tabPanel(
      h4("A&E Activity"), 
             
             fluidRow(
               column(width =6,   
               
               
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
      
             column(width = 6, 
                    
               
               HTML("<h3> <b> Seasonal </b> </h3>"),
               
               column(width= 2, 
               
               mainPanel(
                 radioButtons("measure_2", "", choices = c( "Patients attending" = "number_of_attendances_aggregate", "Patients meeting care target" = "number_meeting_target_aggregate")))),
                column(width = 10, plotOutput("a_e_plot"),
                 dataTableOutput("table_output")
               )
             ),
             
      tags$br(), 
      tags$hr(), 
      tags$br(),
      
      fluidRow(
        
        HTML("<h3><b> Care Targets for Different Healthboards </b> </h3>"), 
        
        mainPanel(
          plotOutput("graph_output_ae")
        )
      )
            
    )
    ),
     
     tabPanel(h4("Map"), 
              fluidRow(
                mainPanel(
                  leafletOutput("map_output_ae",
                                width = 600,
                                height = 1000)
                )
              ), 
              fluidRow(
                sidebarPanel(
                  dateInput(inputId = "date",
                            label = ("Date"),
                            min = "2018-01-01",
                            max = "2022-07-01",
                            value = "2022-04-01"
                  )
                )
              ),
              
     ), 
    
    tabPanel(
      h4("Hospital Activity"), 
             
             fluidRow(
               column(width = 6, 
               
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
             
             column(width = 6, 

               HTML("<h3> <b> Seasonal for Planned and Emergency Patients </b> </h3>"),
               
               column(width = 4, 
               
               mainPanel(
                 radioButtons("period", "", choices = c("Pandemic" = "Pandemic","Prior to pandemic" = "Pre-pandemic")),
                 radioButtons("mes_2", "", choices = c("Total average episodes" = "episodes","Total average length of stay" = "length_of_stay")))),
                 column(width = 8, plotOutput("season_p"),
                 dataTableOutput("table_2")
               )
             ),
             
             tags$br(), 
             tags$hr(), 
             tags$br(),
             
             fluidRow(

               HTML("<h3> <b>   Intensive Care Medicine Before and During the Pandemic </b> </h3>"),
               
               mainPanel(
                 #selectInput("period_2", "Before or During", choices = c("Pandemic" = "Pandemic","Before Pandemic" = "Pre-pandemic")),
                 radioButtons("measure_3", "", choices = c("Total average episodes" = "episodes","Total average length of episodes" = "length_of_episode")),
                 plotOutput("season_i"),
                 dataTableOutput("table_3")
               )
             )
    )
    ), 
    
    tabPanel(
      h4("Bed Occupancy"), 
             
             fluidRow(
               column(width = 6, 

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
             column(width =6,  
               
               HTML("<h3> <b> Seasonal </b> </h3>"),
               
               column(width = 4, 
               
               mainPanel(
                 radioButtons("period_1", "", choices = c("Pandemic" = "Pandemic","Prior to pandemic" = "Pre-pandemic")),
                 radioButtons("m_2", "", choices = c("Percentage of available beds occupied" = "percentage_occupancy","Average number of beds occupied" = "average_occupied_beds")))),
                column(width = 8, plotOutput("season_b"),
                 dataTableOutput("table_b2")
               )
             )
            ,
             
            tags$br(), 
            tags$hr(), 
            tags$br(),
            
             fluidRow(
               mainPanel(
                 plotOutput("graph_output_bed")
               )
             )
      )
      ),
   
     tabPanel(h4("Map"), 
              fluidRow(
                mainPanel(
                  leafletOutput("map_output_bed",
                                width = 600,
                                1000)
                )
              ),
              fluidRow(
                sidebarPanel(
                  dateInput(inputId = "date",
                            label = ("Date"),
                            min = "2018-01-01",
                            max = "2022-07-01",
                            value = "2022-04-01"
                  )
                )
              ),
     ),
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
              "Stacked Bar" = "simd_stack",
              "Side-by-Side Bar" = "simd_dodge",
              "Horizontal Bar" = "simd_flip"
              )
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
      ),
      tags$br(),
      tags$hr(),
      tags$br(),
      fluidRow(
        column(
          6,
          offset = 3,
          plotOutput("mh_euan_plot")
        )
      )
    )
  ))

