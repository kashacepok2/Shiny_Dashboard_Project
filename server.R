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
                 ggplot() +
                 aes(x = age,
                     y = mean_episodes,
                     fill = sex,
                     text = mean_episodes) +
                 geom_col(position = "dodge") +
                 theme_minimal()+
                 scale_fill_manual(values = c("cyan", "green4")) +
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
                 scale_fill_manual(values = c("cyan", "green4")) +
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
                 scale_fill_manual(values = c("cyan", "green4")) +
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
                 geom_col(fill = "green4")+
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
                 geom_col(fill = "cyan") +
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

  summaries <- reactive({
    AE_activity_clean %>%
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
  
  summary_table_1 <- reactive({
    treatment_activity_clean %>%
      filter(admission_type %in% c("Emergency Inpatients", "Elective Inpatients")) %>% 
      group_by(year,admission_type) %>% 
      summarise(m_year = mean(!!sym(input$mes_1)))
  }) 
  
  summary_table_2 <- reactive({
    treatment_activity_clean %>%
      filter(pandemic_years == input$period) %>% 
      filter(admission_type %in% c("Emergency Inpatients", "Elective Inpatients")) %>% 
      group_by(quarter_extr, admission_type) %>% 
      summarise(m_month = mean(!!sym(input$mes_2)))
    
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
  
  summary_table_b1 <- reactive({
    beds_clean %>%
      filter(specialty_name %in% c("Surgery Grouping", "Women and Newborn Grouping", "Intensive Care Medicine", "Mental Health Grouping", "Rehabilitation Medicine")) %>% 
      group_by(year,specialty_name) %>% 
      summarise(m_year_b = mean(!!sym(input$m_1)))
  }) 
  
  summary_table_b2 <- reactive({
    beds_clean %>%
      filter(pandemic_years == input$period_1) %>% 
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
