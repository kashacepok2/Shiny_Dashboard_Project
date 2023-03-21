library(here)
library(tidyverse)
library(janitor)

bot_sex <- clean_names(read_csv(here("raw_data/by_board_of_treatment/inpatient_and_daycase_by_nhs_board_of_treatment_age_and_sex.csv")))

bot_simd <- clean_names(read_csv(here("raw_data/by_board_of_treatment/inpatient_and_daycase_by_nhs_board_of_treatment_and_simd.csv")))

covid_simd <- clean_names(read_csv(here("raw_data/total_cases_simd_20230315.csv")))

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
  



