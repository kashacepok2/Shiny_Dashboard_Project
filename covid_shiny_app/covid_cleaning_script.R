library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(sf)
library(rgdal)
library(leaflet)

beds <- read_csv("raw_data/by_board_of_treatment/beds_by_nhs_board_of_treatment_and_specialty.csv")

hospital_codes <- read_csv("raw_data/codes/hospital_codes.csv")

board_codes <- read_csv("raw_data/codes/health_board_codes.csv")

hospital_codes_clean <- hospital_codes %>%
  clean_names() %>% 
  select(location, hb, x_coordinate, y_coordinate)

board_codes_clean <- board_codes %>% 
  clean_names() %>% 
  select(hb, hb_name)

health_boards <- inner_join(hospital_codes_clean, board_codes_clean, by = "hb")

beds_clean <- beds %>% 
  select(- c("PercentageOccupancyQF",
             "AverageOccupiedBedsQF",
             "AverageAvailableStaffedBedsQF",
             "TotalOccupiedBeddaysQF",
             "AllStaffedBeddaysQF",
             "SpecialtyNameQF",
             "SpecialtyQF", 
             "LocationQF",
             "HBQF",
             "QuarterQF",
             "Specialty")) %>% 
  clean_names() %>%
  mutate(quarter = str_replace(quarter, "Q1", "-01-01")) %>% 
  mutate(quarter = str_replace(quarter, "Q2", "-04-01")) %>% 
  mutate(quarter = str_replace(quarter, "Q3", "-07-01")) %>% 
  mutate(quarter = str_replace(quarter, "Q4", "-10-01")) %>% 
  mutate(quarter = ymd(quarter)) %>% 
  inner_join(health_boards, by = "location") %>% 
  select(- c("hb.x", "hb.y")) %>% 
  mutate(hb_name = gsub("NHS ", "", hb_name)) %>% 
  filter(quarter >= "2018-01-01") %>% 
  rename(date = quarter) %>% 
  rename(percentage = percentage_occupancy) %>% 
  filter(specialty_name == "All Specialties")

ae_times <- read_csv("raw_data/by_board_of_treatment/monthly_ae_waitingtimes_202301.csv")

ae_times_clean <- ae_times %>% 
  select(- c("AttendanceGreater12hrsQF", "AttendanceGreater8hrsQF", "DischargeDestinationUnknownQF", "DischargeDestinationTransferQF", "DischargeDestinationResidenceQF", "DischargeDestinationOtherSpecialtyQF", "DischargeDestinationAdmissionToSameQF", "NumberMeetingTargetEpisodeQF", "NumberOfAttendancesEpisodeQF")) %>%
  clean_names() %>% 
  rename(hb = hbt) %>% 
  rename(location = treatment_location) %>% 
  inner_join(health_boards, by = "location") %>% 
  select(- c("hb.x", "hb.y")) %>% 
  mutate(month = ym(month)) %>% 
  rename(date = month) %>% 
  filter(date >= "2018-01-01") %>% 
  mutate(percentage = (number_meeting_target_aggregate/number_of_attendances_aggregate)*100)

scotland_hb <- clean_names(st_read("raw_data/health_board_mapping/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp"))

scotland_hb_xform <- st_transform(scotland_hb, 4326) %>% 
  st_simplify(dTolerance = 1000)

getcolour <- function(d) {
  case_when(
    d >= 95 ~ "#000000",
    d >= 90 ~ "#330000",
    d >= 85 ~ "#4d0000",
    d >= 80 ~ "#660000",
    d >= 75 ~ "#800000",
    d >= 70 ~ "#990000",
    d >= 65 ~ "#b30000",
    d >= 60 ~ "#cc0000",
    d >= 55 ~ "#e60000",
    d >= 50 ~ "#ff0000",
    d >= 45 ~ "#ff1a1a",
    d >= 40 ~ "#ff3333",
    d >= 35 ~ "#ff4d4d",
    d >= 30 ~ "#ff6666",
    d >= 25 ~ "#ff8080",
    d >= 20 ~ "#ff9999",
    d >= 15 ~ "#ffb3b3",
    d >= 10 ~ "#ffcccc",
    d >= 5 ~ "#ffe6e6",
    d < 5 ~ "#ffffff"
      
  )
}

