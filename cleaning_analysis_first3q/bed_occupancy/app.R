library(shiny)
library(tidyverse)
library(here)
library(janitor)

source(here("cleaning_script.R"))


ui <- fluidPage(
  titlePanel("Temporal Analysis"),
  
  HTML("<br><br><br>"),
  
  HTML("<h3> Yearly Bed Availability in Intensive Care and Other Departments <h3>"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("m_1", "Measure", choices = c("Percentage Occupancy" = "percentage_occupancy","Average Occupancy" = "average_occupied_beds"))
    ),
    mainPanel(
      plotOutput("year_b"),
      dataTableOutput("table_b")
    )
  ),
  
  HTML("<br><br><br>"),
  
  HTML("<h3> Seasonal Bed Occupancy Before and After the Pandemic  <h3>"),
  
  HTML("<br><br><br>"),
  
  mainPanel(
    selectInput("period", "Before or During", choices = c("Pandemic" = "Pandemic","Before Pandemic" = "Pre-pandemic")),
    selectInput("m_2", "Measure", choices = c("Percentage Occupancy" = "percentage_occupancy","Average Occupancy" = "average_occupied_beds")),
    plotOutput("season_b"),
    dataTableOutput("table_b2")
  ),
)


server <- function(input, output) {
  summary_table_b1 <- reactive({
    beds_clean %>%
      filter(specialty_name %in% c("Surgery Grouping", "Women and Newborn Grouping", "Intensive Care Medicine", "Mental Health Grouping", "Rehabilitation Medicine")) %>% 
      group_by(year,specialty_name) %>% 
      summarise(m_year_b = mean(!!sym(input$m_1)))
  }) 
  
  summary_table_b2 <- reactive({
    beds_clean %>%
      filter(pandemic_years == input$period) %>% 
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

