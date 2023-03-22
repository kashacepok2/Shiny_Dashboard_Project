
# libraries 

library(tidyverse)
library(ggplot2)
library(lubridate)
library(tsibble)
library(stringr)
library(infer)
library(janitor)


# read in data-sets 

residence_activity <- read_csv("raw_data/by_board_of_residence/inpatient_and_daycase_by_nhs_board_of_residence.csv") %>% clean_names()

treatment_activity <- read_csv("raw_data/by_board_of_treatment/inpatient_and_daycase_by_nhs_board_of_treatment.csv") %>% clean_names()

residence_age_sex <- read_csv("raw_data/by_board_of_residence/inpatient_and_daycase_by_nhs_board_of_residence_age_and_sex.csv") %>% clean_names()


residence_simd <- read_csv("raw_data/by_board_of_residence/inpatient_and_daycase_by_nhs_board_of_residence_and_simd.csv") %>% clean_names()


residence_speciality <- read_csv("raw_data/by_board_of_residence/inpatient_and_daycase_by_nhs_board_of_residence_and_specialty.csv") %>% clean_names()


beds <- read_csv("raw_data/by_board_of_treatment/beds_by_nhs_board_of_treatment_and_specialty.csv") %>% clean_names()

treatment_age_sex <- read_csv("raw_data/by_board_of_treatment/inpatient_and_daycase_by_nhs_board_of_treatment_age_and_sex.csv") %>% clean_names()

treatment_simd <- read_csv("raw_data/by_board_of_treatment/inpatient_and_daycase_by_nhs_board_of_treatment_and_simd.csv") %>% clean_names()


treatment_speciality <- read_csv("raw_data/by_board_of_treatment/inpatient_and_daycase_by_nhs_board_of_treatment_and_specialty.csv") %>% clean_names()


waiting_times <- read_csv("raw_data/by_board_of_treatment/monthly_ae_waitingtimes_202301.csv") %>% clean_names()


AE_activity_clean <-  
  waiting_times %>% 
  mutate(year = str_extract(month, "\\d{4}")) %>% 
  mutate(month = str_extract(month, "\\d{2}$")) %>% 
  mutate(month = case_when(month == "01" ~ "Jan", month == "02" ~ "Feb", month == "03" ~ "Mar", month == "04" ~ "Apr", month == "05" ~ "May", month == "06" ~ "June", month == "07" ~ "July", month == "08" ~ "Aug", month == "09" ~ "Sep", month == "10" ~ "Oct", month == "11" ~ "Nov", month == "12" ~ "Dec")) %>% 
  mutate(month = as.factor(month)) %>% 
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>% 
  mutate(pandemic_years = case_when(year == "2020" ~ "Pandemic", year == "2021" ~ "Pandemic", year == "2017" ~ "Pre-pandemic", year == "2018" ~ "Pre-pandemic")) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!year %in% c(2023,2007))


write_csv(AE_activity_clean, "ae_activity.csv")



# clean treatment activity 

treatment_activity_clean <-
  treatment_activity %>% 
  mutate(year = str_extract(quarter,"\\d{4}")) %>%
  mutate(quarter_extr = substr(quarter,5,6)) %>% 
  mutate(quarter_extr = case_when(quarter_extr == "Q1" ~ "Jan-Mar", quarter_extr == "Q2" ~ "Apr-June", quarter_extr == "Q3" ~ "July-Sep", quarter_extr == "Q4" ~ "Oct-Dec" )) %>% 
  mutate(quarter_extr = as.factor(quarter_extr)) %>% 
  mutate(quarter_extr = factor(quarter_extr, levels = c("Jan-Mar","Apr-June","July-Sep","Oct-Dec"))) %>% 
  mutate(pandemic_years = case_when(year == "2020" ~ "Pandemic", year == "2021" ~ "Pandemic", year == "2017" ~ "Pre-pandemic", year == "2018" ~ "Pre-pandemic"))


# clean treatment specialty 

treatment_specialty_clean <-
  treatment_speciality %>% 
  mutate(year = str_extract(quarter,"\\d{4}")) %>%
  mutate(quarter_extr = substr(quarter,5,6)) %>% 
  mutate(quarter_extr = case_when(quarter_extr == "Q1" ~ "Jan-Mar", quarter_extr == "Q2" ~ "Apr-June", quarter_extr == "Q3" ~ "July-Sep", quarter_extr == "Q4" ~ "Oct-Dec" )) %>% 
  mutate(quarter_extr = as.factor(quarter_extr)) %>% 
  mutate(quarter_extr = factor(quarter_extr, levels = c("Jan-Mar","Apr-June","July-Sep","Oct-Dec"))) %>% 
  mutate(pandemic_years = case_when(year == "2020" ~ "Pandemic", year == "2021" ~ "Pandemic", year == "2019" ~ "Pre-pandemic", year == "2018" ~ "Pre-pandemic"))

# clean beds

beds_clean_ellen <- 
  beds %>% 
  mutate(year = str_extract(quarter,"\\d{4}")) %>%
  mutate(quarter_extr = substr(quarter,5,6)) %>% 
  mutate(quarter_extr = case_when(quarter_extr == "Q1" ~ "Jan-Mar", quarter_extr == "Q2" ~ "Apr-June", quarter_extr == "Q3" ~ "July-Sep", quarter_extr == "Q4" ~ "Oct-Dec" )) %>% 
  mutate(quarter_extr = as.factor(quarter_extr)) %>% 
  mutate(quarter_extr = factor(quarter_extr, levels = c("Jan-Mar","Apr-June","July-Sep","Oct-Dec"))) %>% 
  mutate(pandemic_years = case_when(year == "2020" ~ "Pandemic", year == "2021" ~ "Pandemic", year == "2019" ~ "Pre-pandemic", year == "2018" ~ "Pre-pandemic"))






