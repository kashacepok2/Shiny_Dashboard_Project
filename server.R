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
               summarise(total_episodes = sum(episodes)) %>% 
               ggplot()+
               aes(x = age,
                   y = total_episodes,
                   fill = sex,
                   text = total_episodes) +
               geom_col(position = "dodge") +
               theme_minimal()+
               labs(
                 x = "\nAge Ranges",
                 y = "Total Episodes\n",
                 fill = "Sex"
               ) +
               theme(
                 axis.text.x = element_text(angle = 315,
                                            vjust = 0.2,
                                            hjust = 0.5)
               ),
             tooltip = "text"
    )
   }
    else if(input$lockdown_input == "post"){
      ggplotly(bot_sex_clean %>% 
                 filter(age %in% input$age_input) %>% 
                 filter(lockdown == "post") %>% 
                 group_by(age, sex) %>% 
                 summarise(total_episodes = sum(episodes)) %>% 
                 ggplot()+
                 aes(x = age,
                     y = total_episodes,
                     fill = sex,
                     text = total_episodes) +
                 geom_col(position = "dodge") +
                 theme_minimal()+
                 labs(
                   x = "\nAge Ranges",
                   y = "Total Episodes\n",
                   fill = "Sex"
                 ) +
                 theme(
                   axis.text.x = element_text(angle = 315,
                                              vjust = 0.2,
                                              hjust = 0.5)
                 ),
               tooltip = "text"
      )
    }
  else{
    ggplotly(bot_sex_clean %>% 
               filter(age %in% input$age_input) %>% 
               group_by(age,sex) %>% 
               summarise(total_episodes = sum(episodes)) %>% 
               ggplot()+
               aes(x = age,
                   y = total_episodes,
                   fill = sex,
                   text = total_episodes) +
               geom_col(position = "dodge") +
               theme_minimal()+
               labs(
                 x = "\nAge Ranges",
                 y = "Total Episodes\n",
                 fill = "Sex"
               ) +
               theme(
                 axis.text.x = element_text(angle = 315,
                                            vjust = 0.2,
                                            hjust = 0.5)
               ),
             tooltip = "text"
    )
  }
  }
)
}