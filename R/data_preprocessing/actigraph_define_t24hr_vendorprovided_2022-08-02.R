#' @description 
#' This script processes ActiGraph vendor-provided daily measures to 
#' to keep only participant-days which are valid days.
#' 
#' Note that to run this file: 
#' - update the path to AG minute-level data to the one from the most recent
#'   AG data dump

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read ActiGraph data 
# ------------------------------------------------------------------------------

# update to the most recent part
dat_path <- file.path(here(), 'data_actigraph_other', '2022-07-01', 'subjectdaystats.csv')

# note: 
# subjectdaystats.csv has "DATE" -- Localized date/time (localized to the site location)
# epochsummarydata.csv has "TIMESTAMPTZ" -- Localized date/time (localized to the site location)
# => so our "local_time_date" based on "TIMESTAMPTZ" (created from pochsummarydata.csv) 
#    will be consistent with "DATE" here 
dat <- 
  fread(dat_path) %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  rename(
    subj_id = subject,
    local_time_date = date
  ) %>%
  mutate(
    local_time_date = as.Date(local_time_date)
  )


# ------------------------------------------------------------------------------
# read other data 
# ------------------------------------------------------------------------------

valid_day_flag_df_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_valid_day_flag.csv')
valid_day_flag_df <- 
  fread(valid_day_flag_df_path) %>%
  as.data.frame() %>%
  mutate(
    local_time_date = as.Date(local_time_date)
  ) %>%
  select(
    subj_id,
    beiwe_id,
    local_time_date,
    valid_day_flag
  )


# ------------------------------------------------------------------------------
# preprocessing 
# ------------------------------------------------------------------------------

# filter to keep valid days only 
dat2 <- 
  dat %>%
  left_join(valid_day_flag_df, by = c("subj_id", "local_time_date")) %>%
  mutate(valid_day_flag = ifelse(is.na(valid_day_flag), 0, valid_day_flag)) %>%
  filter(valid_day_flag == 1)

# reorder columns 
dat3 <-
  dat2 %>%
  select(
    subj_id, 
    beiwe_id,
    local_time_date,
    study_id = studyid,
    device_id = deviceid,
    everything()
  )


# ------------------------------------------------------------------------------
# save data  
# ------------------------------------------------------------------------------

datF <- dat3

out_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_t24hr_vendorprovided.csv')
fwrite(datF, out_path)

