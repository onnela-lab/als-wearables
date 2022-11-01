#' @description 
#' This script derives a set of investigator-derived daily measures,
#' based on ActiGraph AC minute-level data that were preprocessed 
#' (valid days only, ALS participants only), imputed.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read ActiGraph minute-level data (preprocessed, non-imputed)
# ------------------------------------------------------------------------------

# update to the most recent part
dat_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_t1min_imputed.csv')
dat <- 
  fread(dat_path) %>%
  as.data.frame() %>%
  mutate(
    local_time_date = as.Date(local_time_date)
  )


# ------------------------------------------------------------------------------
# compute daily measures
# ------------------------------------------------------------------------------

# data frame with unique values of subj_id and dates
subj_id_dates_df <- 
  dat %>% 
  select(subj_id, local_time_date) %>%
  distinct()


# ------------------------------------------------------------------------------
# function to compute daily summary statistics 
# ------------------------------------------------------------------------------

summarize_PA <- function(ac_i, valid_minute_i, sedentary_thresh = 1853){
  
  # compute PA volume statistics
  tac  <- sum(ac_i)
  tlac <- sum(log(1 + ac_i))
  ltac <- log(tac)
  
  # compute number of valid minutes
  valid_minutes_sum <- sum(valid_minute_i)
  
  # compute PA fragmentation metrics
  is_active_i <- (ac_i >= sedentary_thresh) * 1
  rle_out <- rle(is_active_i * 1)
  # vector of lengths of all active bouts
  actbout_len_vec  <- rle_out$lengths[which(rle_out$values == 1)]
  # vector of lengths of all non-active bouts
  nonactbout_len_vec <- rle_out$lengths[which(rle_out$values == 0)]
  if (length(actbout_len_vec) == 0){
    astp <- NA
    time_spent_active  <- 0
    no_of_active_bouts <- 0
    mean_active_bout   <- 0
  } else {
    astp <- 1/mean(actbout_len_vec)
    time_spent_active  <- sum(actbout_len_vec)/1
    no_of_active_bouts <- length(actbout_len_vec)/1
    mean_active_bout   <- mean(actbout_len_vec)
  }
  if (length(nonactbout_len_vec) == 0){
    satp <- NA
    time_spent_nonactive  <- 0
    no_of_nonactive_bouts <- 0
    mean_nonactive_bout   <- 0
  } else {
    satp <- 1/mean(nonactbout_len_vec)
    time_spent_nonactive  <- sum(nonactbout_len_vec)/1
    no_of_nonactive_bouts <- length(nonactbout_len_vec)/1
    mean_nonactive_bout   <- mean(nonactbout_len_vec)
  }
  
  # define output data frame
  out_i <- data.frame(
    subj_id = subj_id_i,
    local_time_date = local_time_date_i,
    valid_minutes_sum,
    tac, tlac, ltac,
    astp, satp,
    time_spent_active, time_spent_nonactive,
    no_of_active_bouts, no_of_nonactive_bouts,
    mean_active_bout, mean_nonactive_bout
  )
  return(out_i)
}


# ------------------------------------------------------------------------------
# compute daily measures
# ------------------------------------------------------------------------------

# replace NAs with 0s
table(is.na(dat$ac))
# dat$ac[is.na(dat$ac)] <- 0
# table(is.na(dat$ac))

# data frame to store iteration results 
out_df <- data.frame()

nrow(subj_id_dates_df)
for (i in 1 : nrow(subj_id_dates_df)){ # i <- 3
  print(i)
  
  # pull data specific to i-th user-day 
  subj_id_i <- subj_id_dates_df$subj_id[i]
  local_time_date_i <- subj_id_dates_df$local_time_date[i]
  dat_i <- 
    dat %>%
    filter(
      subj_id == subj_id_i,
      local_time_date == local_time_date_i
    ) %>%
    arrange(local_time)
  ac_i <- dat_i$ac
  valid_minute_i <- dat_i$valid_minute
  
  # compute statistics
  out_i <- summarize_PA(ac_i, valid_minute_i)
  
  # rbind additional meta data about the user
  out_i_other <- 
    dat_i %>%
    select(subj_id, beiwe_id) %>%
    distinct()
  out_i <- 
    out_i %>% 
    left_join(out_i_other, by = "subj_id") %>%
    select(subj_id, beiwe_id, everything())
  # append data to file
  out_df <- rbind(out_df, out_i)
}


# ------------------------------------------------------------------------------
# save data
# ------------------------------------------------------------------------------

path_tmp <- file.path(here(), 'data_actigraph_processed', 'actigraph_t24hr_investigatorderived_imputed.csv')
fwrite(out_df, path_tmp)






