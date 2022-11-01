#' @description 
#' This script defines final study sample (subj_id, beiwe_id).
#' 
#' Note: assume in the final study sample, we want participants who: 
#' (a) have at least 2 completely answered ROADS surveys, and 
#' (b) have at least 2 completely answered ALSFRS-R surveys, and 
#' (c) have any wearable data, and
#' (d) are ALS patients (exclude HC). 
#' 
#' The idea is to have this data set constructed at the end 
#' (after above preprocessing steps) and easily update if needed. 
#' This data set would be USED TO FILTER PARTICIPANTS FOR EACH DATA ANALYSIS. 


rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

# ActiGraph valid days flag 
actigraph_valid_day_df_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_valid_day_flag.csv')
actigraph_valid_day_df <- 
  fread(actigraph_valid_day_df_path) %>%
  as.data.frame()


# Modus valid days flag 
modus_valid_day_df_path <- file.path(here(), 'data_modus_processed', 'modus_valid_day_flag.csv')
modus_valid_day_df <- 
  fread(modus_valid_day_df_path) %>%
  as.data.frame()

# Beiwe surveys
roads_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_roads.csv")
roads_beiwe <- fread(roads_beiwe_path) %>% as.data.frame()

alsfrsr_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrs.csv")
alsfrsr_beiwe <- fread(alsfrsr_beiwe_path) %>% as.data.frame()

# ALS / HC map  
beiwe_ids_master_path <- file.path(here(), "data_participants_other", "@Beiwe_IDs_Master.csv")
beiwe_ids_master <- 
  fread(beiwe_ids_master_path) %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  filter(wearables != "no") %>%
  mutate(
    subj_id = as.numeric(substr(subject_id, 5, 8))
  ) %>%
  select(
    subj_id, als_hc_pls
  )


# ------------------------------------------------------------------------------
# make final study sample 1
# ------------------------------------------------------------------------------

# combine IDs from wearables
id_df_actigraph <- 
  actigraph_valid_day_df %>%
  select(subj_id, beiwe_id) %>%
  distinct() %>%
  mutate(
    wearable = "actigraph"
  )

id_df_modus <- 
  modus_valid_day_df %>%
  select(subj_id, beiwe_id) %>%
  distinct() %>%
  mutate(
    wearable = "modus"
  )

dat1 <- 
  id_df_actigraph %>%
  rbind(id_df_modus) 


# ------------------------------------------------------------------------------
# filter to keep those with at least two complete answers for each survey

beiwe_id_roads <- 
  roads_beiwe %>%
  group_by(beiwe_id) %>%
  summarise(complete_surveys_cnt = n())  %>%
  filter(complete_surveys_cnt >= 2) %>%
  pull(beiwe_id)

# checks 
length(beiwe_id_roads)
sort(beiwe_id_roads)

beiwe_id_alsfrs <- 
  alsfrsr_beiwe  %>%
  group_by(beiwe_id) %>%
  summarise(complete_surveys_cnt = n()) %>%
  filter(complete_surveys_cnt >= 2) %>%
  pull(beiwe_id)

# checks 
length(beiwe_id_alsfrs)
sort(beiwe_id_alsfrs)

# create vector with beiwe IDs which are intersection of those having 2 surveys for each 
# of the above
beiwe_id_roads %in% beiwe_id_alsfrs
beiwe_id_alsfrs %in% beiwe_id_roads

beiwe_id_roads_and_alsfrs <- intersect(beiwe_id_roads, beiwe_id_alsfrs)
# check
length(beiwe_id_roads)
length(beiwe_id_alsfrs)
length(beiwe_id_roads_and_alsfrs)

dat2 <- 
  dat1 %>%
  filter(beiwe_id %in% beiwe_id_roads_and_alsfrs)
dim(dat1)
dim(dat2)


# ------------------------------------------------------------------------------
# filter to keep only ALS (exclude HC)

# check
all(dat2$subj_id %in% beiwe_ids_master$subj_id)

subj_id_als <- 
  beiwe_ids_master %>%
  filter(als_hc_pls == "ALS") %>%
  pull(subj_id) 

dat3 <-
  dat2 %>%
  filter(subj_id %in% subj_id_als)
dim(dat2)
dim(dat3)

table(dat3$wearable)


# ------------------------------------------------------------------------------
# save data 

datF <- dat3

datF_path <- file.path(here(), 'data_participants_other_processed', 'final_study_sample_1.csv')
fwrite(datF, datF_path); rm(datF_path)



