#' @description 
#' Define a scope (date range) for Beiwe surveys. For each participants, it is 
#' defined as period of time until: 
#' 
#' max({last in-clinic ALSFRS-R assesment, end of wearables observation period}). 
#' 
#' Note: in-clinic ALSFRS-R baseline assesment happens 1-28 days before start of wearables observation period. 


rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)

# ------------------------------------------------------------------------------
# read and format data 
# ------------------------------------------------------------------------------

# read staff_adnimistered_alsfrs_clean.csv
inclinic_alsfrs_path <- file.path(here(), 'data_participants_other_processed', 'staff_adnimistered_alsfrs_clean.csv')
inclinic_alsfrs <- 
  fread(inclinic_alsfrs_path) %>% 
  as.data.frame() %>% 
  group_by(subj_id, beiwe_id) %>%
  summarise(
    inclinic_alsfrsr_date_min = min(local_time_date),
    inclinic_alsfrsr_date_max = max(local_time_date)
  ) %>%
  ungroup() %>%
  as.data.frame()

# read wearable start and end 
smart_wearables_start_end_path <- file.path(here(), 'data_participants_other_processed', 'smart_wearables_start_end_dates_clean.csv')
smart_wearables_start_end <- 
  fread(smart_wearables_start_end_path) %>% 
  as.data.frame() %>%
  select(subj_id, wearables_date_min = date_start, wearables_date_max = date_ended) %>%
  as.data.frame()


# ------------------------------------------------------------------------------
# combine to create Beiwe surveys range we should use  
# ------------------------------------------------------------------------------

beiwe_surveys_range_df <- 
  # study_sample_1 %>%
  # inner_join(inclinic_alsfrs) %>%
  inclinic_alsfrs %>%
  inner_join(smart_wearables_start_end) %>%
  rowwise() %>%
  mutate(
    beiwe_survey_end = max(c(wearables_date_max, wearables_date_max))
  ) %>%
  filter(!is.na(beiwe_id), beiwe_id != "") %>%
  as.data.frame() 

# ------------------------------------------------------------------------------
# save data 

dat_F <- 
  beiwe_surveys_range_df %>%
  select(
    subj_id, 
    beiwe_id, 
    beiwe_survey_end
  )
dat_F

dat_F_path <- file.path(here(), 'data_participants_other_processed', 'final_beiwe_survey_dates_range_1.csv')
fwrite(dat_F, dat_F_path); rm(dat_F_path)


