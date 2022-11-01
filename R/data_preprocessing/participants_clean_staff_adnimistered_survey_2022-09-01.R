#' @description 
#' This script cleans the file with staff administered FRS survey. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(janitor)
library(here)
options(digits.secs = 0)

# ------------------------------------------------------------------------------
# read data

# read file with mapping beiwe_id ~ subj_id
start_end_dates_fname = "smart_wearables_start_end_dates_clean.csv"
start_end_dates_path = file.path(here(), "data_participants_other_processed", start_end_dates_fname)
start_end_dates = 
  read_csv(start_end_dates_path) %>% 
  select(subj_id, beiwe_id)
head(start_end_dates)


# ------------------------------------------------------------------------------
# process data 

# read smart_wearables_clinicFRS data
staff_frs_fname <- "smart_wearables_clinicFRS_2022-07-07.csv"
staff_frs_path <- file.path(here(), "data_participants_other", "data_staff_administered_frs", staff_frs_fname)
staff_frs <- 
  read_csv(staff_frs_path) %>%
  janitor::clean_names() %>%
  mutate(
    subj_id = as.numeric(substr(record_id, 5, 7)),
    # note: depending on how the data looks like after reading it, one needs to either use 
    # "%m/%d/%y" (2 digits year notation) or "%m/%d/%Y" (4 digits year notation)
    # check it ahead to see if a change is needed
    local_time_date = as.Date(date_alsfrs, format = "%m/%d/%Y")
  ) %>%
  filter(alsfrs_complete == 2) %>%
  select(subj_id, local_time_date, redcap_event_name, frs_total_score) %>%
  left_join(start_end_dates) %>%
  select(subj_id, beiwe_id, everything()) %>% 
  as.data.frame()

# ------------------------------------------------------------------------------
# save data

datF <- staff_frs
datF

datF_path <- file.path(here(), 'data_participants_other_processed', 'staff_adnimistered_alsfrs_clean.csv')
fwrite(datF, datF_path); rm(datF_path)


# ------------------------------------------------------------------------------
# make extended version (each FRS question has its score)
# ------------------------------------------------------------------------------

staff_frs_ext <- 
  read_csv(staff_frs_path) %>%
  janitor::clean_names() %>%
  mutate(
    subj_id = as.numeric(substr(record_id, 5, 7)),
    # note: depending on how the data looks like after reading it, one needs to either use 
    # "%m/%d/%y" (2 digits year notation) or "%m/%d/%Y" (4 digits year notation)
    # check it ahead to see if a change is needed
    local_time_date = as.Date(date_alsfrs, format = "%m/%d/%Y")
  ) %>%
  filter(alsfrs_complete == 2) %>%
  select(subj_id, local_time_date, redcap_event_name, starts_with("frs")) %>%
  left_join(start_end_dates) %>%
  select(subj_id, beiwe_id, everything()) %>% 
  as.data.frame()


# ------------------------------------------------------------------------------
# save data

datF <- staff_frs_ext
datF

datF_path <- file.path(here(), 'data_participants_other_processed', 'staff_adnimistered_alsfrs_clean_ext.csv')
fwrite(datF, datF_path); rm(datF_path)










