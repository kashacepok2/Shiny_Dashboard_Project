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
  
  output$table_1 <- DT::renderDataTable({
    summary_table_1()
  }) 
  
  output$table_2 <- DT::renderDataTable({
    summary_table_2()
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
}

shinyApp(ui = ui, server = server)
