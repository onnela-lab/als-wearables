
#' @description 
#' This script takes file
#' - /participants_other_data/smart_wearables_start_end_dates_Y-m-d.csv
#' and cleans it so there is one row per person

rm(list = ls())
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(stringr)
library(here)
options(digits.secs = 3)


# ------------------------------------------------------------------------------

# read data 
start_end_dates_fname <- "smart_wearables_start_end_dates_2022-07-07.csv"
start_end_dates_path <- file.path(here(), "data_participants_other", start_end_dates_fname)
start_end_dates <- 
  fread(start_end_dates_path) %>%
  as.data.frame() %>%
  janitor::clean_names() %>% 
  select(record_id, redcap_event_name, wearable_type, date_wearable_begin, end_date_wearables, beiwe_id) %>%
  rename(subj_id = record_id,
         date_start = date_wearable_begin,
         date_end = end_date_wearables) 

# clean data 
start_end_dates_C <- 
  start_end_dates %>% 
  mutate(
    subj_id = gsub("701-","",subj_id),
    subj_id = as.numeric(subj_id),
    beiwe_id = str_trim(beiwe_id)
    ) %>%
  group_by(subj_id) %>%
  # to pull the proper rows (rows with data) for both start and end date
  summarise(
    wearable_type = max(wearable_type, na.rm = TRUE),
    date_start = max(date_start, na.rm = TRUE),
    date_ended = max(date_end, na.rm = TRUE),
    beiwe_id = max(beiwe_id, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    date_start = as.Date(date_start, format = "%m/%d/%Y"),
    date_ended = as.character(as.Date(date_ended, format = "%m/%d/%Y")),
    date_ended = as.Date(date_ended),
    wearable_type = case_when(
      wearable_type == 2 ~ "modus",
      wearable_type == 1 ~ "actigraph"),
    obs_duration = as.numeric(difftime(date_ended, date_start, units = "days")) + 1
  ) %>%
  as.data.frame()

# save to file 
out_path <- file.path(here(), "data_participants_other_processed", "smart_wearables_start_end_dates_clean.csv")
fwrite(start_end_dates_C, out_path)
