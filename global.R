library(shiny)
library(tidyverse)
library(janitor)
library(here)
library(plotly)

source(here("cleaning.R"))

ages_list <- list(
  "0-9 years",
  "10-19 years",
  "20-29 years",
  "30-39 years",
  "40-49 years",
  "50-59 years",
  "60-69 years",
  "70-79 years",
  "80-89 years",
  "90 years and over"
)

simd_stack <- ggplotly(
  bot_simd_clean %>% 
    drop_na(simd) %>% 
    filter(admission_type %in% c("Elective Inpatients",
                                 "Emergency Inpatients")) %>% 
    group_by(simd, admission_type) %>% 
    summarise(total_episodes = sum(episodes)) %>% 
    ggplot()+
    aes(x = simd,
        y = total_episodes,
        fill = admission_type,
        text = total_episodes) +
    geom_col(position = "stack")+
    theme_minimal(
    ) +
    labs(
      x = "\nSIMD Ranking",
      y = "Total Episodes\n",
      fill = "Admission Type",
      title = "Total Episodes by Inpatient Type and Deprivation Quintile"
    ),
  tooltip = "text"
)

simd_flip <- ggplotly(
  bot_simd_clean %>% 
    drop_na(simd) %>% 
    filter(admission_type %in% c("Elective Inpatients",
                                 "Emergency Inpatients")) %>% 
    group_by(simd, admission_type) %>% 
    summarise(total_episodes = sum(episodes)) %>% 
    ggplot()+
    aes(x = simd,
        y = total_episodes,
        fill = admission_type,
        text = total_episodes) +
    geom_col(position = "stack")+
    theme_minimal(
    ) +
    labs(
      x = "\nSIMD Ranking",
      y = "Total Episodes\n",
      fill = "Admission Type",
      title = "Total Episodes by Inpatient Type and Deprivation Quintile"
    ) +
    coord_flip(),
  tooltip = "text"
)

simd_dodge <- ggplotly(
  bot_simd_clean %>% 
    drop_na(simd) %>% 
    filter(admission_type %in% c("Elective Inpatients",
                                 "Emergency Inpatients")) %>% 
    group_by(simd, admission_type) %>% 
    summarise(total_episodes = sum(episodes)) %>% 
    ggplot()+
    aes(x = simd,
        y = total_episodes,
        fill = admission_type,
        text = total_episodes) +
    geom_col(position = "dodge")+
    theme_minimal(
    ) +
    labs(
      x = "\nSIMD Ranking",
      y = "Total Episodes\n",
      fill = "Admission Type",
      title = "Total Episodes by Inpatient Type and Deprivation Quintile"
    ),
  tooltip = "text"
)








