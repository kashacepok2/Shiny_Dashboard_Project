
library(shiny)
library(tidyverse)
library(here)
library(janitor)

waiting_times <- read_csv("raw_data/by_board_of_treatment/monthly_ae_waitingtimes_202301.csv") %>% clean_names()

AE_activity_clean <-  
  waiting_times %>% 
  mutate(year = str_extract(month, "\\d{4}")) %>% 
  mutate(month = str_extract(month, "\\d{2}$")) %>% 
  mutate(month = case_when(month == "01" ~ "Jan", month == "02" ~ "Feb", month == "03" ~ "Mar", month == "04" ~ "Apr", month == "05" ~ "May", month == "06" ~ "June", month == "07" ~ "July", month == "08" ~ "Aug", month == "09" ~ "Sep", month == "10" ~ "Oct", month == "11" ~ "Nov", month == "12" ~ "Dec")) %>% 
  mutate(month = as.factor(month)) %>% 
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>% 
  mutate(pandemic_years = case_when(year == "2020" ~ "Pandemic", year == "2021" ~ "Pandemic", year == "2017" ~ "Pre-pandemic", year == "2018" ~ "Pre-pandemic"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Temporal Analysis"),
    
    HTML("<br><br><br>"),
    
    HTML("<h3>A&E Activity<h3>"),
    
    HTML("<br><br><br>"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("period", "Before or During Pandemic", choices = c("Pandemic", "Pre-pandemic"))
          ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("period")
        ),
    )
    )


# Define server logic 
server <- function(input, output) {

    output$period <- renderPlot({
      
      AE_activity_clean %>% 
        filter(pandemic_years %in% input$period) %>% 
        group_by(month, pandemic_years) %>% 
        summarise(sum_attendances = sum(number_of_attendances_aggregate)) %>% 
        ggplot(aes(x = month, y = sum_attendances, colour = pandemic_years, group = pandemic_years)) +
        geom_line()+
        theme_minimal()+
        labs(x = "", y = "", colour = "")
      
      # AE_activity_clean %>% 
      #   filter(pandemic_years %in% input$period) %>% 
      #   group_by(month, pandemic_years) %>% 
      #   summarise(sum_month = sum(number_of_attendances_aggregate)) %>% 
      #   ggplot(aes(x = month, y = sum_month)) +
      #   geom_line(aes(group = pandemic_years))
       
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
