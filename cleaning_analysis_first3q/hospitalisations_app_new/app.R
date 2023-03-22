library(shiny)
library(tidyverse)
library(here)
library(janitor)

source(here("cleaning_script.R"))


ui <- fluidPage(
  titlePanel("Temporal Analysis"),
  
  HTML("<br><br><br>"),
  
  HTML("<h3> Yearly Hospital Activity for Planned and Emergency Patients <h3>"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("measure_1", "Measure", choices = c("Number of Episodes" = "episodes","Length of Stay" = "length_of_stay"))
    ),
    mainPanel(
      plotOutput("year_p"),
      dataTableOutput("table_1")
    )
  ),
  
  HTML("<br><br><br>"),
  
  HTML("<h3> Seasonal Hospital Activity for Planned and Emergency Patients <h3>"),
  
  HTML("<br><br><br>"),
  
  mainPanel(
    selectInput("period", "Before or During", choices = c("Pandemic" = "Pandemic","Before Pandemic" = "Pre-pandemic")),
    selectInput("measure_2", "Measure", choices = c("Number of Episodes" = "episodes","Length of Stay" = "length_of_stay")),
    plotOutput("season_p"),
    dataTableOutput("table_2")
  ),
  HTML("<br><br><br>"),
  
  HTML("<h3> Intensive Care Medicine Before and During the Pandemic <h3>"),
  
  HTML("<br><br><br>"),
  
  mainPanel(
    #selectInput("period_2", "Before or During", choices = c("Pandemic" = "Pandemic","Before Pandemic" = "Pre-pandemic")),
    selectInput("measure_3", "Measure", choices = c("Number of Episodes" = "episodes","Length of Episodes" = "length_of_episode")),
    plotOutput("season_i"),
    dataTableOutput("table_3")
  ),
)


server <- function(input, output) {
  summary_table_1 <- reactive({
    treatment_activity_clean %>%
      filter(admission_type %in% c("Emergency Inpatients", "Elective Inpatients")) %>% 
      group_by(year,admission_type) %>% 
      summarise(m_year = mean(!!sym(input$measure_1)))
  }) 
  
  summary_table_2 <- reactive({
    treatment_activity_clean %>%
      filter(pandemic_years == input$period) %>% 
      filter(admission_type %in% c("Emergency Inpatients", "Elective Inpatients")) %>% 
      group_by(quarter_extr, admission_type) %>% 
      summarise(m_month = mean(!!sym(input$measure_2)))
    
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
  
}

shinyApp(ui = ui, server = server)