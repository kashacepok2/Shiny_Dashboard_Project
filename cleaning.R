library(here)
library(tidyverse)
library(janitor)

bot_sex <- clean_names(read_csv(here("raw_data/by_board_of_treatment/inpatient_and_daycase_by_nhs_board_of_treatment_age_and_sex.csv")))

bot_simd <- clean_names(read_csv(here("raw_data/by_board_of_treatment/inpatient_and_daycase_by_nhs_board_of_treatment_and_simd.csv")))

clean_location<- function(table) {
  unique(table %>% 
           select(!ends_with("qf"))) %>% 
    mutate(quarter = as.factor(quarter))
  }


bot_sex_clean <- clean_location(bot_sex)

bot_simd_clean <- clean_location(bot_simd)



