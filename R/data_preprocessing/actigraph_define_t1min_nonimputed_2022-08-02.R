#' @description 
#' This script derives a set of minute-level data. It consists of 
#' ActiLife-derived Activity Counts  minute-level data provided by the vendor,
#' but subset to keep only participant-days such as: 
#' - valid day

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
    ac = vectormagnitudecounts
    ) %>%
  as_tibble()
head(dat)
nrow(dat)

# ------------------------------------------------------------------------------
# read other data 
# ------------------------------------------------------------------------------

valid_day_flag_df_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_valid_day_flag.csv')
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

# process to remove few duplicates due to daylight saving time change 
# (i.e., UTC time will be unique, but there will be duplicate minutes for one hour
# in local time 
# here, we arbitrarily discard minutes from "second" recording of hour 2:00-3:00am 
dat2 <- 
  dat %>%
  group_by(subj_id, local_time) %>%
  mutate(cnt = n()) %>%
  arrange(subj_id, utc_time) %>%
  filter(row_number() == 1) %>%
  ungroup()
nrow(dat2)
nrow(dat2) / 1440


# ------------------------------------------------------------------------------
# expand data frame so as for each subject-day there are 1440 entries,
# one for each minute (in case there are no data entries due to device docking)
# table with unique values of subject and day date 
local_time_df <- 
  dat2 %>% 
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
# define local time date
dat3 <- 
  local_time_grid_df %>%
  left_join(dat2, by = c("local_time", "subj_id")) %>%
  mutate(local_time_date = as.Date(local_time))
nrow(dat3)
nrow(dat3) / 1440

# ------------------------------------------------------------------------------
# filter to keep valid days only
dat4 <- 
  dat3 %>%
  left_join(valid_day_flag_df, by = c("subj_id", "local_time_date")) %>%
  mutate(valid_day_flag = ifelse(is.na(valid_day_flag), 0, valid_day_flag)) %>%
  filter(valid_day_flag == 1)
nrow(dat4)


# ------------------------------------------------------------------------------
# format utc_time as character
dat5 <- 
  dat4 %>%
  mutate(
    utc_time_date = as.Date(utc_time),
    utc_time = as.character(utc_time)
  )


# ------------------------------------------------------------------------------
# define final data frame
# ------------------------------------------------------------------------------
datF <- dat5

datF <- 
  datF %>%
  select(
    subj_id, 
    beiwe_id, 
    local_time_date, 
    local_time,
    utc_time,
    utc_time_date,
    valid_day_flag,
    ac
    ) 
head(datF)

# checks
table(datF$valid_day_flag, useNA = "always")
nrow(datF) / 1440


# ------------------------------------------------------------------------------
# save data
# ------------------------------------------------------------------------------

out_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_t1min_nonimputed.csv')
fwrite(datF, out_path)

