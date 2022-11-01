#' @description 
#' This script derives a set of minute-level data. It consists of 
#' ActiLife-derived Activity Counts  minute-level data provided by the vendor,
#' but subset to keep only participant-days such as: 
#' - valid day
#' and further impute at the places of invalid minutes. 

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
dat_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_t1min_nonimputed.csv')
dat <- 
  fread(dat_path) %>%
  as.data.frame()
nrow(dat)

# ------------------------------------------------------------------------------
# impute non-valid minutes
# ------------------------------------------------------------------------------

# impute from subject-specific curve; 
# if not available (e.g., subject always takes of for night), impute from 
# population-level curve

# define placeholder for imputing data 
dat$ac_imp <- NA

# get population level average
ac_all <- dat$ac
ac_all[dat$valid_minute == 0] <- NA
ac_all_mat <- matrix(ac_all, byrow = TRUE, ncol = 1440)
mu_all <- apply(ac_all_mat, 2, mean, na.rm = TRUE)

# vector with all unique subj_id 
subj_id_vec <- sort(unique(dat$subj_id))

for (i in 1 : length(subj_id_vec)){ # i <- 2
  print(paste0("i = ", i))
  # pull data of subject specific to this loop iteration
  subj_id_i <- subj_id_vec[i]
  subj_id_i_idx <- which(dat$subj_id == subj_id_i)
  dat_i <- dat[subj_id_i_idx, ]
  ac_i <- dat_i$ac
  # replace with NA wherever invalid minute
  ac_i[dat_i$valid_minute == 0] <- NA
  ac_i_mat <- matrix(ac_i, byrow = TRUE, ncol = 1440)
  # define subject's all days mean curve
  mu_i <- apply(ac_i_mat, 2, mean, na.rm = TRUE)
  mu_i_cnt <- apply(ac_i_mat, 2, function(col_ij) {sum(!is.na(col_ij))})
  # in the subject's all days mean curve, 
  # replace remaining NAs or minutes with less than 5 days of observations, 
  # if any, with population-level curve
  mu_i_replace_mu_alll <- which(is.na(mu_i) | mu_i_cnt < 5)
  if (length(mu_i_replace_mu_alll) > 0){
    mu_i[mu_i_replace_mu_alll] <- mu_all[mu_i_replace_mu_alll]
  }
  # impute missing data in AC 
  ac_i_mat_imp <- ac_i_mat 
  for (j in 1 : nrow(ac_i_mat_imp)){
    row_i <- ac_i_mat_imp[j, ]
    row_i_na <- which(is.na(row_i))
    if (length(row_i_na) > 0){
      row_i[row_i_na] <- mu_i[row_i_na]
      ac_i_mat_imp[j, ] <- row_i
    }
  }
  # matrix to vector 
  ac_i_imp <- as.vector(t(ac_i_mat_imp))
  # replace in data the imputed values 
  dat$ac_imp[subj_id_i_idx] <- ac_i_imp
}

# check if data the same on places with valid minutes
all(dat$ac[dat$valid_minute == 1] == dat$ac_imp[dat$valid_minute == 1])
all(!is.na(dat$ac_imp))


# ------------------------------------------------------------------------------
# define final data frame
# ------------------------------------------------------------------------------

dat_F <- 
  dat %>%
  select(-ac) %>%
  rename(ac = ac_imp) %>%
  mutate(ac = round(ac, 3)) 

# check (should be 0)
sum(is.na(dat_F$ac))


# ------------------------------------------------------------------------------
# save data
# ------------------------------------------------------------------------------

out_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_t1min_imputed.csv')
fwrite(dat_F, out_path)








