
#' @description 
#' This script subsets  survey answers to keep ALSFRS-R and ROADS (complete answers to these) only.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library(here)
options(digits.secs = 3)


# ------------------------------------------------------------------------------
# READ DATA
# ------------------------------------------------------------------------------

# Beiwe surveys all 
survey_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly.csv")
survey_beiwe <- 
  fread(survey_beiwe_path) %>%
  as.data.frame() %>%
  mutate(local_time_date = as.Date(local_time_date))


# ------------------------------------------------------------------------------
# prepare clean -- Beiwe ROADS 
# ------------------------------------------------------------------------------

# define map between ROAD answer text and ROAD answer value
roads_answer_text_value_map <- data.frame(
  answer = c("Unable to perform", 
             "Abnormal: able to perform but with difficulty", 
             "Normal: able to perform without difficulty"),
  answer_value = c(0, 1, 2)
)

# read map between ROAD answer value and ROAD answer value normed
roads_answer_value_normed_map_path <- file.path(here(), "doc", "Surveys", "ROADS_answers_score_normed_map.csv")
roads_answer_value_normed_map <- 
  fread(roads_answer_value_normed_map_path) %>% 
  as.data.frame() %>%
  select(-rasch_logit_measure)

# map to answer value
roads_beiwe <- 
  survey_beiwe %>% 
  filter(survey_name == "ROADS") %>% 
  left_join(roads_answer_text_value_map, by = "answer") %>%
  filter(!is.na(answer_value)) %>%
  group_by(subj_id, beiwe_id, local_time_date) %>%
  summarise(
    answers_cnt = n(),
    answers_score = sum(answer_value)
  ) %>%
  filter(answers_cnt == 28) %>%
  ungroup() %>%
  left_join(roads_answer_value_normed_map, by = "answers_score") %>%
  select(
    subj_id, beiwe_id, local_time_date, everything()
  )


# ------------------------------------------------------------------------------
# prepare clean --  Beiwe ALSFRS-R 
# ------------------------------------------------------------------------------

# read pre-defined map of ALSFRS-R answer text and ALSFRS-R answer value
alsfrs_answer_map_path <- file.path(here(), "doc", "Surveys", "ALSFRSR_answer_value_map.csv")
alsfrs_answer_map <- read_csv(alsfrs_answer_map_path)

# make data used in model fit 
alsfrsr_beiwe0 <- 
  survey_beiwe %>% 
  filter(survey_name == "ALSFRS-R") %>%
  filter(!grepl("Do you have a g-tube or feeding tube that you use for", question_text)) %>%
  left_join(alsfrs_answer_map, by = c("question_id", "question_text", "answer")) 

alsfrsr_beiwe <- 
  alsfrsr_beiwe0 %>% 
  group_by(subj_id, beiwe_id, local_time_date) %>%
  summarise(
    answers_cnt = n(),
    answers_score = sum(answer_value)
  ) %>%
  filter(answers_cnt == 12) %>%
  ungroup() %>%
  select(
    subj_id, beiwe_id, local_time_date, everything()
  )


# ------------------------------------------------------------------------------
# save processed data 
# ------------------------------------------------------------------------------

roads_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_roads.csv")
fwrite(roads_beiwe, roads_beiwe_path)

alsfrsr_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrs.csv")
fwrite(alsfrsr_beiwe, alsfrsr_beiwe_path)









