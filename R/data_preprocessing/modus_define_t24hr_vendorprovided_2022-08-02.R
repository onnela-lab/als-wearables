#' @description 
#' This script processes Modus vendor-provided daily measures to 
#' to keep only participant-days which are valid days.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read Modus data 
# ------------------------------------------------------------------------------

# TODO: update the path for the most recent data drop
dat_path <- file.path(here(), 'data_modus_other', 'smart_wearables_modus_july.csv')
dat <- 
  fread(dat_path) %>%
  as.data.frame() %>%
  janitor::clean_names()
nrow(dat)


# ------------------------------------------------------------------------------
# read other data 
# ------------------------------------------------------------------------------

valid_day_flag_df_path <- file.path(here(), 'data_modus_processed', 'modus_valid_day_flag.csv')
valid_day_flag_df <- 
  fread(valid_day_flag_df_path) %>%
  as.data.frame() %>%
  mutate(
    local_time_date = as.Date(local_time_date)
  )
head(valid_day_flag_df)
mean(valid_day_flag_df$valid_day_flag)


# ------------------------------------------------------------------------------
# preprocessing
# ------------------------------------------------------------------------------

dat2 <- 
  dat %>%
  mutate(
    subj_id = as.numeric(substr(ssid, 5, 7)),
  ) %>%
  mutate(
    local_time_date = as.Date(date)
  )


# ------------------------------------------------------------------------------
# filter to keep only dates from valid day 
# ------------------------------------------------------------------------------

dat3 <- 
  dat2 %>%
  left_join(valid_day_flag_df, by = c("subj_id", "local_time_date")) %>%
  mutate(valid_day_flag = ifelse(is.na(valid_day_flag), 0, valid_day_flag)) %>%
  filter(valid_day_flag == 1)
nrow(dat3)

# subset data columns
dat4 <-
  dat3 %>%
  select(-c(project, ssid, date, sw4_id)) %>%
  select(
    subj_id, 
    beiwe_id, 
    local_time_date,
    valid_hr_flag_sum,
    valid_day_flag,
    everything()
  )


# ------------------------------------------------------------------------------
# save data
# ------------------------------------------------------------------------------

datF <- dat4
dim(datF)
# [1] 2947   24 # 2022-06-15

out_path <- file.path(here(), 'data_modus_processed', 'modus_t24hr_vendorprovided.csv')
fwrite(datF, out_path)

