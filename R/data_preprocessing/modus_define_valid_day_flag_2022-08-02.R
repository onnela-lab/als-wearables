#' @description 
#' This script derives "valid day" flag for Modus data.
#' 
#' Valid day flag =1 if expected to wear a sensor & had data indicating wear for
#' a pre-defined threshold time of a day, =0 otherwise.
#' 
#' The "pre-defined threshold time of a day" is defined as at least 8h 
#' (not necessarily consecutive), each with at least one step recorded. 
#' 
#' Note to update the Modus raw data path to the one from the most recent data dump.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read and combine raw Modus data
# ------------------------------------------------------------------------------

# read and combine modus minute-level data 
dat_dir <- file.path(here(), "data_modus_raw", "Raw Files 20220721", "mt-pharma-201119_SMART_ModusHealth_all_data_FINAL_07212022")
dat_fpaths <- list.files(dat_dir, full.names = TRUE)
dat_fpaths_l <- length(dat_fpaths)

# append files 
df_out <- data.table()
# for (i in 1 : 500){ # i <- 50
for (i in 1 : dat_fpaths_l){  # i <- 50
  print(i)
  # pull out first 1440 rows that contain information about minute-level binned data
  ans <- fread(dat_fpaths[i], nrows = 1440) 
  ans <- ans[, .(BINNED_STEPS_TIMESTAMP, BINNED_STEPS_STEPS_IN_BIN)]
  ans[, file_name:=basename(dat_fpaths[i])]
  df_out <- rbindlist(list(df_out, ans))
}
dat <- as.data.frame(df_out)

# check
dat_file_name_cnt <- 
  dat %>%
  select(file_name) %>%
  distinct() %>%
  pull(file_name)
dat_fpaths_l == length(dat_file_name_cnt)


# ------------------------------------------------------------------------------
# read other data
# ------------------------------------------------------------------------------

# data with device wear 
device_wear_dates_df_fname <- "smart_wearables_start_end_dates_clean.csv"
device_wear_dates_df_path <- file.path(here(), "data_participants_other_processed", device_wear_dates_df_fname)
device_wear_dates_df <- 
  fread(device_wear_dates_df_path) %>%
  as.data.frame() 
# check 
all(sort(unique(dat$subj_id)) %in% device_wear_dates_df$subj_id)


# ------------------------------------------------------------------------------
# preprocessing
# ------------------------------------------------------------------------------

dat2 <- 
  dat %>%
  janitor::clean_names() %>%
  rename(
    local_time = binned_steps_timestamp
  ) %>% 
  mutate(
    subj_id = as.numeric(substr(file_name, 23, 25)),
    local_time_date = substr(file_name, 27, 34),
    local_time_date = as.Date(local_time_date, "%Y%m%d"),
    local_time_hr = hour(local_time)
  )

# check
dat2_check <- 
  dat2 %>%
  group_by(
    subj_id, local_time
  ) %>%
  summarise(
    cnt = n()
  ) %>% 
  pull(cnt)
# if this is true, this means no duplicates related to tz etc 
all(dat2_check == 1)


# ------------------------------------------------------------------------------
# filter to keep only days where participants were supposed to be wearing wearable
# ------------------------------------------------------------------------------

dat3a <- 
  dat2 %>%
  left_join(device_wear_dates_df, by = "subj_id") 
# check
!any(is.na(dat3a$date_start))

dat3 <- 
  dat3a %>%
  filter(local_time_date >= date_start) %>%
  filter(local_time_date <= date_ended) 


# ------------------------------------------------------------------------------
# aggregate into hour, defile valid hour
# ------------------------------------------------------------------------------

dat4 <- 
  dat3 %>%
  group_by(
    subj_id, beiwe_id, local_time_date, local_time_hr
  ) %>%
  summarise(
    steps_in_bin = sum(binned_steps_steps_in_bin)
  ) %>%
  mutate(
    valid_hr_flag = ifelse(steps_in_bin > 0, 1, 0)
  ) %>%
  ungroup()

nrow(dat4)
nrow(dat4) / 24
mean(dat4$valid_hr_flag)


# ------------------------------------------------------------------------------
# aggregate into day, define valid day flag
# ------------------------------------------------------------------------------

dat5 <- 
  dat4 %>%
  group_by(
    subj_id, beiwe_id, local_time_date
  ) %>%
  summarise(
    valid_hr_flag_sum = sum(valid_hr_flag)
  ) %>%
  mutate(
    valid_day_flag = ifelse(valid_hr_flag_sum >= 8, 1, 0)
  ) %>%
  ungroup()

nrow(dat5)
mean(dat5$valid_day_flag)


# ------------------------------------------------------------------------------
# SAVE DATA -- PART 1 (day-level)
# ------------------------------------------------------------------------------

datF_A <- dat5
dim(datF_A)

out_path <- file.path(here(), 'data_modus_processed', 'modus_valid_day_flag.csv')
fwrite(datF_A, out_path); rm(out_path)


# ------------------------------------------------------------------------------
# SAVE DATA -- PART 2 (hour-level)
# ------------------------------------------------------------------------------

datF_B <- 
  dat4 %>%
  left_join((dat5 %>% select(subj_id, local_time_date,valid_day_flag)))
dim(datF_B)

out_path <- file.path(here(), 'data_modus_processed', 'modus_valid_hour_flag.csv')
fwrite(datF_B, out_path); rm(out_path)

