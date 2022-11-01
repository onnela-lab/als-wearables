#' @description 
#' This script derives "valid day" flag for ActiGraph (AG) data.
#' 
#' Valid day flag =1 if expected to wear a sensor & had worn a sensor 
#' for pre-defined threshold time of a day, =0 otherwise.
#' 
#' The "pre-defined threshold time of a day" is defined as at least 8h 
#' (not necesairly consecutive), each with all valid minutes. There are 2 reasons
#' we have seen that the minute may be invalid: 
#' 1. wear_flag = 0 (based on ActiGraph-derived wear flags using Choi algorithm); 
#' 2. there is no ActiGraph data (neither minute-level Activity Counts nor raw).
#' Otherwise, a minute is valid. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read ActiGraph minute-level data
# ------------------------------------------------------------------------------

# update to the most recent part
dat_path <- file.path(here(), 'data_actigraph_other', '2022-07-01', 'epochsummarydata.csv')
dat0 <- fread(dat_path)

# subset columns, rename columns
# format wear and awake flags to numeric
dat <- 
  dat0 %>%
  as.data.frame() %>% 
  janitor::clean_names() %>%
  select(
    subj_id = subject, 
    utc_time = timestamputc,
    local_time = timestamptz,
    ac = vectormagnitudecounts,
    wear_flag = wear
    ) %>%
  mutate(
    wear_flag = as.numeric(wear_flag)
  ) %>%
  as_tibble()


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
#  preprocessing steps
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# process to remove few duplicates due to daylight saving time change 
# (i.e., UTC time will be unique, but there will be duplicate minutes for one hour
# in local time 
# here, we arbitrarily discard minutes from "second" recording of hour 2:00-3:00am 
t1 <- Sys.time()
dat <- 
  dat %>%
  group_by(subj_id, local_time) %>%
  mutate(cnt = n()) %>%
  arrange(subj_id, utc_time) %>%
  filter(row_number() == 1) %>%
  ungroup()
t2 <- Sys.time()
t2 - t1
# Time difference of 2.197076 mins
nrow(dat)


# ------------------------------------------------------------------------------
# expand data frame so as for each subject-day there are 1440 entries,
# one for each minute (in case there are no data entries due to device docking)
# table with unique values of subject and day date 
local_time_df <- 
  dat %>% 
  mutate(local_time_date = as.Date(local_time)) %>%
  select(subj_id, local_time_date) %>%
  distinct() 
# define grid of data
local_time_grid_list <- list()
for (i in 1 : nrow(local_time_df)){ # i <- 1
  print(i)
  subj_id_i <- local_time_df$subj_id[i] 
  local_time_date_i <- local_time_df$local_time_date[i] 
  local_time_i <- seq(ymd_hms(paste0(local_time_date_i, " 00:00:00.000")), 
                      length.out = 1440, by = '1 min')
  out_df_i <- data.frame(local_time = local_time_i, subj_id = subj_id_i)
  local_time_grid_list[[i]] <- out_df_i
}
local_time_grid_df <- as.data.frame(data.table::rbindlist(local_time_grid_list))

# create data frame (expanded version)
# define valid minute
# define hour of a day variable 
dat2 <- 
  local_time_grid_df %>%
  left_join(dat, by = c("local_time", "subj_id")) %>%
  mutate(
    has_data_flag = ifelse(!is.na(ac), 1, 0),
    local_time_date = as.Date(local_time),
    local_time_hr = hour(local_time)
  )
nrow(dat2)
nrow(dat2) / 1440

# below should be true (i.e., if there is no data, then there is no wear/non-wear flag)
!any(dat2$has_data_flag == 0 & !is.na(dat2$wear_flag))


# ------------------------------------------------------------------------------
# filter to only keep days where is a day where subject was under observational 
# period according to metadata 
# ------------------------------------------------------------------------------

dat3a <- 
  dat2 %>%
  left_join(device_wear_dates_df, by = "subj_id") 
# check
!any(is.na(dat3a$date_start))

dat3 <- 
  dat3a %>%
  filter(local_time_date >= date_start) %>%
  filter(local_time_date <= date_ended) %>%
  mutate(
    valid_minute_flag = ifelse(has_data_flag == 1 & wear_flag == 1, 1, 0)
  ) 
nrow(dat3)
nrow(dat3) / 1440


# ------------------------------------------------------------------------------
# Define valid day flag, summarize number of valid minutes, 
# and invalid minutes
# ------------------------------------------------------------------------------

# aggregate on (subject, date, hour) level
dat4 <- 
  dat3 %>% 
  group_by(subj_id, beiwe_id, local_time_date, local_time_hr) %>%
  summarise(
    valid_minute_flag_sum = sum(valid_minute_flag, na.rm = TRUE),
    wear_flag_sum         = sum(wear_flag == 1, na.rm = TRUE),
    nonwear_flag_sum      = sum(wear_flag == 0, na.rm = TRUE),
    data_flag_sum         = sum(has_data_flag == 1, na.rm = TRUE),
    nodata_flag_sum       = sum(has_data_flag == 0, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    valid_hr_flag = ifelse(valid_minute_flag_sum == 60, 1, 0)
  ) %>%
  as.data.frame()

# aggregate on (subject, date) level
# define valid_day_flag (valid_hr_flag_sum >= 8)
dat5 <- 
  dat4 %>%
  group_by(subj_id, beiwe_id, local_time_date) %>%
  summarise(
    valid_minute_flag_sum = sum(valid_minute_flag_sum),
    wear_flag_sum         = sum(wear_flag_sum, na.rm = TRUE),
    nonwear_flag_sum      = sum(nonwear_flag_sum, na.rm = TRUE),
    data_flag_sum         = sum(data_flag_sum, na.rm = TRUE),
    nodata_flag_sum       = sum(nodata_flag_sum, na.rm = TRUE),
    valid_hr_flag_sum     = sum(valid_hr_flag, na.rm = TRUE)
  ) %>%
  select(
    subj_id, 
    beiwe_id,
    local_time_date,
    valid_minute_flag_sum,
    nonwear_flag_sum,
    nodata_flag_sum,
    valid_hr_flag_sum
  ) %>%
  mutate(valid_day_flag = ifelse(valid_hr_flag_sum >= 8, 1, 0)) %>%
  as.data.frame()


# ------------------------------------------------------------------------------
# SAVE DATA -- PART 1 (day-level)
# ------------------------------------------------------------------------------

datF <- dat5

out_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_valid_day_flag.csv')
fwrite(datF, out_path); rm(out_path)


# ------------------------------------------------------------------------------
# SAVE DATA -- PART 2 (hour-level)
# ------------------------------------------------------------------------------

datF_B <- 
  dat4 %>% 
  left_join(datF %>% select(subj_id, local_time_date, valid_day_flag)) %>%
  select(
    subj_id, 
    beiwe_id, 
    local_time_date, 
    local_time_hr,
    valid_hr_flag,
    valid_day_flag
  )
nrow(datF_B)

out_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_valid_hour_flag.csv')
fwrite(datF_B, out_path); rm(out_path)


# ------------------------------------------------------------------------------
# SAVE DATA -- PART 3 (minute-level)
# ------------------------------------------------------------------------------

datF_C <- 
  dat3 %>% 
  left_join(datF %>% select(subj_id, local_time_date, valid_day_flag)) %>%
  select(
    subj_id, 
    beiwe_id, 
    local_time_date, 
    local_time_hr,
    local_time,
    valid_minute_flag,
    valid_day_flag
  )
nrow(datF_C) / 1440

out_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_valid_minute_flag.csv')
fwrite(datF_C, out_path); rm(out_path)


