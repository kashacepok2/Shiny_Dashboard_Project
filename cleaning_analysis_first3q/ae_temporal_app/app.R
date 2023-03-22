
library(shiny)
library(tidyverse)
library(here)
library(janitor)

source(here("cleaning_script.R"))


ui <- fluidPage(
  titlePanel("Temporal Analysis"),
  
  HTML("<br><br><br>"),
  
  HTML("<h3> Yearly A&E Activity<h3>"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("measure_1", "Number of Patients", choices = c("Attending A&E" = "number_of_attendances_aggregate","Meeting Care Target" = "number_meeting_target_aggregate"))
    ),
    mainPanel(
      plotOutput("year_plot"),
      dataTableOutput("table_output_2")
    )
  ),
  
  HTML("<br><br><br>"),
  
  HTML("<h3> Seasonal A&E Activity<h3>"),
  
  HTML("<br><br><br>"),
  
  mainPanel(
    #selectInput("period", "Pandemic or Pre-pandemic", choices = c("Pandemic", "Pre-pandemic")),
    selectInput("measure_2", "Number of Patients", choices = c( "Attending A&E" = "number_of_attendances_aggregate", "Meeting Care Target" = "number_meeting_target_aggregate")),
    plotOutput("a_e_plot"),
    dataTableOutput("table_output")
  )
)

server <- function(input, output) {
  summaries <- reactive({
    AE_activity_clean %>%
      #filter(pandemic_years %in% input$period) %>% 
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
}

shinyApp(ui = ui, server = server)
