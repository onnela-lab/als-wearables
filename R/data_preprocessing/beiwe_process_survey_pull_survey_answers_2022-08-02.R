
#' @description 
#' This script processes surveys from "survey_answers" Beiwe data stream. 
#' These will be later combined with surveys from "survey_timings" Beiwe data stream
#' (as neither stream is perfect, hence by combining two we assure we get all
#' data available).

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(here)
options(digits.secs = 3)

# source config file (not publically available) with hard-coded Beiwe ID 
# to fix one particular participant
source(file.path(here(), "R", "config.R"))

# define Beiwe raw data dir
BEIWE_RAW_DATA_DIR <- file.path(here(), "data_beiwe_raw")


# ------------------------------------------------------------------------------
# READ AND COMBINE DATA
# ------------------------------------------------------------------------------

#' For all surveys, assume the local time zone is "America/New_York". 
#' This may be imperfect (participants may travel etc.) but essentially does not
#' matter in practice as we only use date. 
tz <- "America/New_York"

# create empty list to store survey_answers responses
dat_all_list <- list()

beiwe_ids <- list.dirs(BEIWE_RAW_DATA_DIR, recursive = FALSE, full.names = FALSE)
user_dirs <- file.path(BEIWE_RAW_DATA_DIR, beiwe_ids)

for (i in 1 : length(beiwe_ids)){ # i <- 1
  beiwe_id <- beiwe_ids[i]
  user_dir <- user_dirs[i]
  print(paste0("i = ", i, ", beiwe_id = ", beiwe_id))
  survey_answers_path <- file.path(user_dir, "survey_answers")
  # if no survey_answers dir, go to the next user
  if (!dir.exists(survey_answers_path)) next
  survey_ids <- list.dirs(survey_answers_path, recursive = FALSE, full.names = FALSE)
  # if no question directories in survey_answers dir, go to the next user
  if (length(survey_ids) == 0) next 
  
  # iterate over question-specific directories 
  for (j in 1 : length(survey_ids)){ # j <- 1
    # get vector of response filenames whitin this survey_id directory 
    survey_id <- survey_ids[j]
    survey_id_dir <- file.path(survey_answers_path, survey_id)
    response_dat_fnames <- list.files(survey_id_dir)
    if (length(response_dat_fnames) == 0) next
    
    for (k in 1 : length(response_dat_fnames)){ # k <- 1
      response_dat_fname <- response_dat_fnames[k]
      # get UTC and local time of answers submission from the filename 
      utc_time <- ymd_hms(gsub("_", ":", str_sub(response_dat_fname, 1, 19)), tz = "UTC")
      local_time <- format(utc_time, tz = tz, usetz = TRUE, format = "%Y-%m-%d %H:%M:%S")
      # define data path
      dat_path <- file.path(survey_id_dir, response_dat_fname)
      # check if corrupted file and skip if yes 
      dat0 <- readLines(dat_path, n = 1)
      if(!dat0 == "question id,question type,question text,question answer options,answer") next 
      # read the answers data 
      dat <- 
        fread(dat_path) %>% 
        as.data.frame() %>%
        janitor::clean_names() %>%
        mutate(dat_idx = k) %>%
        mutate(dat_row_idx = row_number()) %>%
        mutate(beiwe_id = beiwe_id) %>%
        mutate(survey_id = survey_id) %>%
        # format to character to have the saved values presented more clearly 
        mutate(
          utc_time = as.character(utc_time),
          utc_time = ifelse(nchar(utc_time) == 10, paste0(utc_time, " 00:00:00"), str_sub(utc_time, 1, 19)),
          local_time = as.character(local_time),
          local_time = ifelse(nchar(local_time) == 10, paste0(local_time, " 00:00:00"), str_sub(local_time, 1, 19))
        )
      # append to data frame of all data 
      dat_all_list[[length(dat_all_list) + 1]] <- dat
      
    }
  }
}

# combine data into one df 
dat_all_df <- 
  rbindlist(dat_all_list) %>% 
  as.data.frame() %>%
  arrange(beiwe_id, utc_time, survey_id, dat_idx, dat_row_idx) %>%
  mutate(local_time_date = as.Date(ymd_hms(local_time))) %>%
  mutate(source = "survey_answers") %>%
  select(
    beiwe_id,
    utc_time,
    local_time,
    survey_id,
    question_id,
    question_type,
    question_text,
    question_answer_options,
    answer,
    dat_idx,
    dat_row_idx,
    source
  )

names(dat_all_df)
# [1] "beiwe_id"                "utc_time"                "local_time"             
# [4] "survey_id"               "question_id"             "question_type"          
# [7] "question_text"           "question_answer_options" "answer"                 
# [10] "dat_idx"                 "dat_row_idx"             "source"    


# ------------------------------------------------------------------------------
# Fix NA into "" to be consistent with "survey_answers" stream
# ------------------------------------------------------------------------------

# check number of NA across data frame columns
sapply(dat_all_df, function(vec) sum(is.na(vec)))

# replace NA with ""
dat_all_df <- dat_all_df %>% replace(is.na(.), "")

sapply(dat_all_df, function(vec) sum(is.na(vec)))


# ------------------------------------------------------------------------------
# Fix one particular subject (values hard-coded in /R/config.R file not publically available)
# ------------------------------------------------------------------------------

# increase dat_idx for BEIWE_ID_FIX1_TO by the largest dat_idx found for BEIWE_ID_FIX1_FROM
dat_idx_max_BEIWE_ID_FIX1_FROM <- dat_all_df %>% filter(beiwe_id == BEIWE_ID_FIX1_FROM) %>% pull(dat_idx) %>% max()
dat_all_df <- 
  dat_all_df %>%
  mutate(
    dat_idx = ifelse(beiwe_id == BEIWE_ID_FIX1_TO, dat_idx + dat_idx_max_BEIWE_ID_FIX1_FROM, dat_idx)
  )

# replace the beiwe ID from BEIWE_ID_FIX1_FROM to BEIWE_ID_FIX1_TO  
dat_all_df <- 
  dat_all_df %>%
  mutate(
    beiwe_id = ifelse(beiwe_id == BEIWE_ID_FIX1_FROM, BEIWE_ID_FIX1_TO, beiwe_id)
  )

# ------------------------------------------------------------------------------
# SAVE DATA 
# ------------------------------------------------------------------------------

# save processed data 
dat_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_survey_answers.rds")
saveRDS(dat_all_df, dat_path)






