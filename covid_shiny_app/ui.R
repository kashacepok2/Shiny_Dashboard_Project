library(shiny)
library(tidyverse)
library(bslib)

source("covid_cleaning_script.R")

beds_clean 

ui <- fluidPage(
  titlePanel("Geographic Visualisation of Covid Data"),
  theme = bs_theme(bootswatch = "minty"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "dataset",
        label = ("Which data do you want to look at?"),
        choices = c("ae_times_clean", "beds_clean") 
      ),
      dateInput("date",
                     label = ("Date range"),
                     min = "2018-01-01",
                     max = "2022-07-01"
      )
     ),
    mainPanel(
      plotOutput("map_output"),
      plotOutput("graph_output")
    )
  )
)

server <- function(input, output, session) {
  output$map_output <- renderPlot(expr = {
    
      scotland_hb_xform %>%
      leaflet() %>% 
      addTiles() %>% 
      addPolygons(label = ~hb_name,
                  fillColor = getcolour(input$dataset$percentage), 
                  opacity = 0.8)
  }
  )
  output$graph_output <- renderPlot(expr = {
    input$dataset %>% 
      group_by(hb_name, date) %>%
      summarise(Percentage = mean(percentage)) %>% 
      ggplot() +
      aes(x = date, y = Percentage, colour = hb_name) +
      geom_line() +
      scale_colour_manual(values = c("yellow1", "darkorchid4", "burlywood3", "firebrick4", "forestgreen", "hotpink", "darkorange3",  "palegreen2", "cadetblue2", "orchid3", "black", "navy", "antiquewhite4", "sienna2")) +
      theme_classic()
  }
  )
}

shinyApp(ui, server)
