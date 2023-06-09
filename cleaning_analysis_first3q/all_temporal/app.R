library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(shinythemes)

source(here("cleaning_script.R"))


ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Temporal Analysis"),
  
  HTML("<br><br><br>"),
  
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
  )
  ),
)


server <- function(input, output) {
  summaries <- reactive({
    AE_activity_clean %>%
      filter(pandemic_years %in% c("Pandemic", "Pre-pandemic")) %>% 
      group_by(month,pandemic_years) %>% 
      summarise(count = sum(!!sym(input$measure_2)))
  }) 
  
  summaries_2 <- reactive({
    AE_activity_clean %>%
      group_by(year) %>% 
      summarise(count_2 = sum(!!sym(input$measure_1)))
    
  }) 
  
  output$table_output <- DT::renderDataTable({
    summaries()
  }) 
  
  output$table_output_2 <- DT::renderDataTable({
    summaries_2()
  }) 
  
  output$a_e_plot <- renderPlot({
    ggplot(summaries(), aes(x = month, y = count, colour = pandemic_years, group = pandemic_years)) +
      geom_line() +
      theme_minimal() +
      labs(x = "", y = "", colour = "")
  })
  
  output$year_plot <- renderPlot({
    ggplot(summaries_2(), aes(x = year, y = count_2)) +
      geom_line(group =1) +
      theme_minimal() +
      labs(x = "", y = "")
  })
  
  summary_table_1 <- reactive({
    treatment_activity_clean %>%
      filter(admission_type %in% c("Emergency Inpatients", "Elective Inpatients")) %>% 
      group_by(year,admission_type) %>% 
      summarise(m_year = mean(!!sym(input$mes_1)))
  }) 
  
  summary_table_2 <- reactive({
    treatment_activity_clean %>%
      filter(pandemic_years == input$period) %>% 
      filter(admission_type %in% c("Emergency Inpatients", "Elective Inpatients")) %>% 
      group_by(quarter_extr, admission_type) %>% 
      summarise(m_month = mean(!!sym(input$mes_2)))
    
  }) 
  
  summary_table_3 <- reactive({
    treatment_specialty_clean %>%
      filter(pandemic_years %in% c("Pandemic", "Pre-pandemic")) %>% 
      filter(specialty_name == "Intensive Care Medicine") %>% 
      group_by(quarter_extr, pandemic_years) %>% 
      summarise(m_season = mean(!!sym(input$measure_3)))
    
  }) 
  
  output$table_1 <- DT::renderDataTable({
    summary_table_1()
  }) 
  
  output$table_2 <- DT::renderDataTable({
    summary_table_2()
  }) 
  
  output$table_3 <- DT::renderDataTable({
    summary_table_3()
    
  }) 
  
  output$year_p <- renderPlot({
    
    ggplot(summary_table_1(), aes(x = year, y = m_year, colour = admission_type, group = admission_type)) +
      geom_line() +
      theme_minimal() +
      labs(x = "", y = "", colour = "")
  })
  
  output$season_p <- renderPlot({
    ggplot(summary_table_2(), aes(x = quarter_extr, y = m_month, colour = admission_type, group = admission_type)) +
      geom_line() +
      theme_minimal() +
      labs(x = "", y = "", colour = "")
  })
  
  output$season_i <- renderPlot({
    ggplot(summary_table_3(), aes(x = quarter_extr, y = m_season, colour = pandemic_years, group = pandemic_years)) +
      geom_line() +
      theme_minimal() +
      labs(x = "", y = "", colour = "")
  })
  
  summary_table_b1 <- reactive({
    beds_clean %>%
      filter(specialty_name %in% c("Surgery Grouping", "Women and Newborn Grouping", "Intensive Care Medicine", "Mental Health Grouping", "Rehabilitation Medicine")) %>% 
      group_by(year,specialty_name) %>% 
      summarise(m_year_b = mean(!!sym(input$m_1)))
  }) 
  
  summary_table_b2 <- reactive({
    beds_clean %>%
      filter(pandemic_years == input$period_1) %>% 
      filter(specialty_name %in% c("Surgery Grouping", "Women and Newborn Grouping", "Intensive Care Medicine", "Mental Health Grouping", "Rehabilitation Medicine"))%>% 
      group_by(quarter_extr, specialty_name) %>% 
      summarise(m_month_b = mean(!!sym(input$m_2)))
    
  }) 
  
  output$table_b <- DT::renderDataTable({
    summary_table_b1()
  }) 
  
  output$table_b2 <- DT::renderDataTable({
    summary_table_b2()
  }) 
  
  output$year_b <- renderPlot({
    
    ggplot(summary_table_b1(), aes(x = year, y = m_year_b, colour = specialty_name, group = specialty_name)) +
      geom_line() +
      theme_minimal() +
      labs(x = "", y = "", colour = "")
  })
  
  output$season_b <- renderPlot({
    ggplot(summary_table_b2(), aes(x = quarter_extr, y = m_month_b, colour = specialty_name, group = specialty_name)) +
      geom_line() +
      theme_minimal() +
      labs(x = "", y = "", colour = "")
  })
}

shinyApp(ui = ui, server = server)