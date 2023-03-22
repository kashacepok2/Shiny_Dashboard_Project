server <- function(input, output, session) {
  
  
  
  output$simd_plot <- renderPlotly({
    if (input$bar_input == "simd_stack") 
    {simd_stack}
    else if (input$bar_input == "simd_flip") 
    {simd_flip}
    else {simd_dodge}
    
  }
  )
  
  
  output$age_range_plot <- renderPlotly({
    if (input$lockdown_input == "pre" ){
      ggplotly(bot_sex_clean %>% 
                 filter(age %in% input$age_input) %>% 
                 filter(lockdown == "pre") %>% 
                 group_by(age,sex) %>% 
                 summarise(mean_episodes = mean(episodes)) %>% 
                 ggplot()+
                 aes(x = age,
                     y = mean_episodes,
                     fill = sex,
                     text = mean_episodes) +
                 geom_col(position = "dodge") +
                 theme_minimal()+
                 scale_fill_manual(values = c("red", "red4")) +
                 labs(
                   x = "\nAge Ranges",
                   y = "Mean Episodes per Location by Quarter\n",
                   fill = "Sex",
                   title = "Episodes Across Age and Gender"
                 ) +
                 theme(
                   axis.text.x = element_text(angle = 315,
                                              vjust = 0.2,
                                              hjust = 0.5)
                 ) +
                 scale_y_continuous(
                   n.breaks = 10
                 ),
               tooltip = "text"
      )
    }
    else if(input$lockdown_input == "post"){
      ggplotly(bot_sex_clean %>% 
                 filter(age %in% input$age_input) %>% 
                 filter(lockdown == "post") %>% 
                 group_by(age, sex) %>% 
                 summarise(mean_episodes = mean(episodes)) %>% 
                 ggplot() +
                 aes(x = age,
                     y = mean_episodes,
                     fill = sex,
                     text = mean_episodes) +
                 geom_col(position = "dodge") +
                 theme_minimal() +
                 scale_fill_manual(values = c("red", "red4")) +
                 labs(
                   x = "\nAge Ranges",
                   y = "Mean Episodes per Location by Quarter\n",
                   fill = "Sex",
                   title = "Episodes Across Age and Gender"
                 ) +
                 theme(
                   axis.text.x = element_text(angle = 315,
                                              vjust = 0.2,
                                              hjust = 0.5)
                 ) +
                 scale_y_continuous(
                   n.breaks = 10),
               tooltip = "text"
      )
    }
    else{
      ggplotly(bot_sex_clean %>% 
                 filter(age %in% input$age_input) %>% 
                 group_by(age, sex) %>% 
                 summarise(mean_episodes = mean(episodes)) %>% 
                 ggplot()+
                 aes(x = age,
                     y = mean_episodes,
                     fill = sex,
                     text = mean_episodes) +
                 geom_col(position = "dodge") +
                 theme_minimal()+
                 scale_fill_manual(values = c("red", "red4")) +
                 labs(
                   x = "\nAge Ranges",
                   y = "Mean Episodes per Location by Quarter\n",
                   fill = "Sex",
                   title = "Episodes Across Age and Gender"
                 ) +
                 theme(
                   axis.text.x = element_text(angle = 315,
                                              vjust = 0.2,
                                              hjust = 0.5)
                 ) +
                 scale_y_continuous(
                   n.breaks = 10),
               tooltip = "text"
      )
    }
  }
  )
  
  output$simd_covid_plot <- renderPlotly({
    if (input$covid_simd_input == "first") {
      ggplotly(covid_simd %>% 
                 
                 ggplot() +
                 aes(
                   x = simd_quintile,
                   y = first_infections,
                   text = first_infections
                 ) +
                 geom_col(fill = "red4")+
                 theme_minimal() +
                 labs(
                   x = "\n SIMD Quintile",
                   y = "Total Infections\n\n",
                   title = "Distribution of Infections across SIMD Quintile\n"
                 ) +
                 scale_y_continuous(
                   n.breaks = 10
                 ),
               tooltip = "text"
      )
    }
    else{
      ggplotly(covid_simd %>% 
                 
                 ggplot() +
                 aes(
                   x = simd_quintile,
                   y = reinfections,
                   text = reinfections
                 ) +
                 geom_col(fill = "red") +
                 theme_minimal() +
                 labs(
                   x = "\nSIMD Quintile\n",
                   y = "Total Reinfections\n\n",
                   title = "Distribution of Reinfections across SIMD Quintile\n"
                 ) +
                 scale_y_continuous(
                   n.breaks = 10
                 ),
               tooltip = "text"
      )
    }
  }
  )
}