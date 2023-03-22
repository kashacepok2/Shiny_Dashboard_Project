#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#

library(shiny)
library(tidyverse)
library(here)
library(janitor)

source(here("cleaning_script.R"))


ui <- fluidPage(
  titlePanel("Temporal Analysis"),
  
  HTML("<br><br><br>"),
  
  HTML("<h3>A&E Activity<h3>"),
  
  HTML("<br><br><br>"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("time", "Impact Over Time", choices = c("year", "month")),
      selectInput("measure", "Number of Patients", choices = c("number_of_attendances_aggregate", "number_meeting_target_aggregate"))
    ),
    mainPanel(
      plotOutput("a_e_plot"),
      dataTableOutput("table_output")
    )
  )
)

server <- function(input, output) {
  summaries <- reactive({
    AE_activity_clean %>%
      group_by(!!sym(input$time)) %>% 
      summarise(count = sum(!!sym(input$measure)))
  }) 
  
  output$table_output <- DT::renderDataTable({
    summaries()
  }) 
  
  output$a_e_plot <- renderPlot({
    ggplot(summaries(), aes(x = !!sym(input$time), y = count)) +
      geom_line(group =1) +
      theme_minimal() +
      labs(x = "", y = "")
  })
}

shinyApp(ui = ui, server = server)
