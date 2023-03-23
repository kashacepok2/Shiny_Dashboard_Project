library(here)
library(tidyverse)
library(janitor)
library(patchwork)

bot_sex <- clean_names(read_csv(here("raw_data/by_board_of_treatment/inpatient_and_daycase_by_nhs_board_of_treatment_age_and_sex.csv")))

bot_simd <- clean_names(read_csv(here("raw_data/by_board_of_treatment/inpatient_and_daycase_by_nhs_board_of_treatment_and_simd.csv")))

covid_simd <- clean_names(read_csv(here("raw_data/total_cases_simd_20230315.csv")))

mh_waiting_times_euan <- clean_names(read_csv(here("raw_data/camhs-adjusted-patients-seen.csv")))

clean_location<- function(table) {
  unique(table %>% 
           select(!ends_with("qf"))) %>% 
    mutate(quarter = as.factor(quarter)) %>% 
  mutate(lockdown = case_when(
    quarter %in% c("2017Q3", "2017Q4", "2018Q1", "2018Q2", "2018Q3", "2018Q4",
                   "2019Q1", "2019Q2", "2019Q3", "2019Q4", "2020Q1") ~ "pre",
    TRUE ~ "post"
  ))
  }


bot_sex_clean <- clean_location(bot_sex)

bot_simd_clean <- clean_location(bot_simd) %>% 
  mutate(simd = as.factor(simd)) 
  

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
      x = "\nSIMD Quintile",
      y = "Total Episodes\n",
      fill = "Admission Type",
      title = "Total Episodes by Inpatient Type and Deprivation Quintile"
    ) +
    scale_y_continuous(
      n.breaks = 10
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
      x = "\nSIMD Quintile",
      y = "Total Episodes\n",
      fill = "Admission Type",
      title = "Total Episodes by Inpatient Type and Deprivation Quintile"
    ) +
    theme(
      axis.text.x = element_text(angle = 315,
                                 hjust = 0,
                                 vjust = 1) 
    ) +   coord_flip() +
    scale_y_continuous(
      n.breaks = 10
    ),
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
      x = "\nSIMD Quintile",
      y = "Total Episodes\n",
      fill = "Admission Type",
      title = "Total Episodes by Inpatient Type and Deprivation Quintile"
    )+
    scale_y_continuous(
      n.breaks = 10
    ),
  tooltip = "text"
)


mh_waiting_times_clean_euan <- mh_waiting_times_euan %>% 
  mutate(percentage_trgt_met = 100*(number_of_patients_seen0to18weeks/total_patients_seen)) %>% 
  mutate(pandemic = case_when(
    month <= 202003 ~ "pre",
    month > 202003 ~ "post"
  )) %>% 
  filter(total_patients_seen > 0)

pre_mh <- mh_waiting_times_clean_euan %>% 
  filter(pandemic == "pre") %>% 
  ggplot()+
  aes(x = percentage_trgt_met)+
  geom_histogram(fill = "#00bfc4") +
  theme_minimal() +
  labs(x = "",
       y = "Count",
       title = "Distribution of Healthboards Meeting Target Children and Adolescents 
Mental Health Referral Times",
subtitle = "\nBefore the first UK COVID-19 lockdown"
  )

post_mh <- mh_waiting_times_clean_euan %>% 
  filter(pandemic == "post") %>% 
  ggplot()+
  aes(x = percentage_trgt_met)+
  geom_histogram(fill = "#F8766D") +
  theme_minimal() +
  labs(x = "Percentage of Patients Seen in the Target Timeframe",
       y = "Count",
       subtitle = "After the first UK COVID-19 lockdown"
  )

mh_plot_euan <- pre_mh/post_mh

