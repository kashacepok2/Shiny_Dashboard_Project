
library(shiny)
library(tidyverse)
library(here)
library(janitor)


source(here("cleaning_script.R"))

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)


ui <- fluidPage(
  titlePanel("Temporal Analysis"),
  sidebarLayout(
    sidebarPanel(
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
      filter(pandemic_years %in% c("Pandemic", "Pre-pandemic")) %>% 
      summarise(count = sum(!!sym(names(input$measure))))
  }) 
  
  output$table_output <- DT::renderDataTable({
    summaries()
  }) 
  
  output$a_e_plot <- renderPlot({
    ggplot(summaries(), aes(x = pandemic_years, y = count)) +
      geom_line()
  })
}

shinyApp(ui = ui, server = server)


