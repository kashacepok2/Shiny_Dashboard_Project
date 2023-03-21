library(shiny)
library(tidyverse)
library(janitor)
library(here)
library(plotly)

source(here("cleaning.R"))

ages_list <- bot_sex_clean %>% 
  distinct(age) %>% 
  pull()

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
      fill = "Admission Type"
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
      fill = "Admission Type"
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
      fill = "Admission Type"
    ),
  tooltip = "text"
)








