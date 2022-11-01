#' @description 
#' This script summarizes Beiwe survey and wearable device compliance in a table.


rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)
options(dplyr.summarise.inform = FALSE)


# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

# sample participants
study_sample_1_path <- file.path(here(), 'data_participants_other_processed', 'final_study_sample_1.csv')
study_sample_1 <- fread(study_sample_1_path) %>% as.data.frame()
dim(study_sample_1)

#  final date range for Beiwe survey dates to be used
study_beiwe_survey_dates_range_1_path <- file.path(here(), 'data_participants_other_processed', 'final_beiwe_survey_dates_range_1.csv')
study_beiwe_survey_dates_range_1 <- fread(study_beiwe_survey_dates_range_1_path) %>% as.data.frame()
dim(study_beiwe_survey_dates_range_1)

# roads complete answers
roads_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_roads.csv")
roads_beiwe <- 
  fread(roads_beiwe_path) %>%
  as.data.frame() %>%
  inner_join(study_sample_1) %>%
  inner_join(study_beiwe_survey_dates_range_1) %>%
  filter(
    local_time_date <= beiwe_survey_end
  )
# checks
dim(roads_beiwe)
length(unique(roads_beiwe$subj_id))

# alsfrs-r complete answers
alsfrsr_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrs.csv")
alsfrsr_beiwe <- 
  fread(alsfrsr_beiwe_path) %>%
  as.data.frame() %>%
  inner_join(study_sample_1) %>%
  inner_join(study_beiwe_survey_dates_range_1) %>%
  filter(
    local_time_date <= beiwe_survey_end
  )


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# surveys compliance
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Beiwe survey: compliance
# ------------------------------------------------------------------------------

# make table that has additional wearable group "combined" 
wearable_fct_levels <- c("actigraph", "modus", "combined")
wearable_fct_labels <- c("ActiGraph", "Modus", "Combined")
survey_name_levels <- c("alsfrsr_beiwe", "roads_beiwe")
survey_name_labels <- c("ALSFRS-RSE", "ROADS")

# combine data from two sueveys
df1 <- 
  roads_beiwe %>% 
  select(subj_id, wearable, local_time_date) %>%
  mutate(name = "roads_beiwe")
df2 <- 
  alsfrsr_beiwe %>% 
  select(subj_id, wearable, local_time_date) %>%
  mutate(name = "alsfrsr_beiwe")

# table with counts 
tbl_df <- 
  df1 %>% 
  rbind(df2) %>%
  group_by(subj_id, wearable, name) %>%
  summarise(value = n()) %>%
  ungroup()
# append additional set of rows as "combined" group of participants, format
tbl_df <- 
  tbl_df %>% 
  rbind(tbl_df %>% mutate(wearable = "combined")) %>%
  mutate(wearable_fct = factor(wearable, levels = wearable_fct_levels, labels = wearable_fct_labels)) %>% 
  mutate(name_fct = factor(name, levels = survey_name_levels, labels = survey_name_labels))

tbl_df_survey_mean_sd <- 
  tbl_df %>%
  group_by(wearable_fct, name_fct) %>%
  summarise(
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_mean_f = sprintf("%.1f", value_mean),
    value_sd_f = sprintf("%.1f", value_sd),
    value_f = paste0(value_mean_f, " (", value_sd_f, ")")
  ) %>%
  select(wearable_fct, name_fct, value_f) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  mutate(name_fct = paste0(name_fct, " submissions (mean (SD))")) 

tbl_df_survey_median_min_max <- 
  tbl_df %>%
  group_by(wearable_fct, name_fct) %>%
  summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(wearable_fct, name_fct, value_f) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  mutate(name_fct = paste0(name_fct, " submissions (median [min, max])")) 

tbl_df_survey <- tbl_df_survey_mean_sd %>% rbind(tbl_df_survey_median_min_max)
tbl_df_survey <- tbl_df_survey[c(1,3,2,4), ]
tbl_df_survey

rm(tbl_df)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Wearables: compliance
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# read wearables valid day 
# ------------------------------------------------------------------------------

# actigraph
actigraph_valid_day_flag_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_valid_day_flag.csv')
actigraph_valid_day_flag <- 
  fread(actigraph_valid_day_flag_path) %>% 
  as.data.frame() %>%
  filter(subj_id %in% study_sample_1$subj_id)
dim(actigraph_valid_day_flag)

# modus 
modus_valid_day_flag_path <- file.path(here(), 'data_modus_processed', 'modus_valid_day_flag.csv')
modus_valid_day_flag <- 
  fread(modus_valid_day_flag_path) %>% 
  as.data.frame() %>%
  filter(subj_id %in% study_sample_1$subj_id)
dim(modus_valid_day_flag)


# ------------------------------------------------------------------------------
# read smart_wearables_start_end_dates_clean.csv
# ------------------------------------------------------------------------------

smart_wearables_start_end_path <- file.path(here(), 'data_participants_other_processed', 'smart_wearables_start_end_dates_clean.csv')
smart_wearables_start_end <- 
  fread(smart_wearables_start_end_path) %>% 
  as.data.frame() %>%
  select(-c(beiwe_id, wearable_type))


# ------------------------------------------------------------------------------
# aggregate
# ------------------------------------------------------------------------------

# prepare data to combine
tbl_actigraph <- 
  actigraph_valid_day_flag %>%
  select(
    subj_id,
    local_time_date,
    valid_hr_flag_sum,
    valid_day_flag
  ) %>%
  mutate(
    wearable = "actigraph"
  )

tbl_modus <- 
  modus_valid_day_flag %>%
  select(
    subj_id,
    local_time_date,
    valid_hr_flag_sum,
    valid_day_flag
  ) %>%
  mutate(
    wearable = "modus"
  )

# combine and aggregate
tbl_comb <- 
  tbl_actigraph %>%
  rbind(tbl_modus) %>%
  group_by(
    subj_id,
    wearable
  ) %>%
  summarise(
    valid_day_ndays = sum(valid_day_flag),
    avg_valid_hr_on_valid_day = mean(ifelse(valid_day_flag == 1, valid_hr_flag_sum, NA), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # 2022-05-26 @MK: the updated definition of compliance uses end - start date 
  left_join(smart_wearables_start_end) %>%
  rename(obs_ndays = obs_duration) %>%
  select(
    subj_id,
    wearable,
    obs_ndays,
    valid_day_ndays,
    avg_valid_hr_on_valid_day
  ) %>%
  pivot_longer(cols = c(obs_ndays, valid_day_ndays, avg_valid_hr_on_valid_day))


name_fct_levels <- c('obs_ndays', 'valid_day_ndays', 'avg_valid_hr_on_valid_day')
name_fct_labels <- c(
  'Days in observation period', 
  'Compliant days in observation period', 
  'Average # compliant hours on a compliant day'
)
wearable_fct_levels <- c("actigraph", "modus", "combined")
wearable_fct_labels <- c("ActiGraph", "Modus", "Combined")

# add definition of "all"
tbl_comb <- 
  tbl_comb %>% 
  rbind(tbl_comb %>% mutate(wearable = "combined")) %>%
  mutate(wearable_fct = factor(wearable, levels = wearable_fct_levels, labels = wearable_fct_labels)) %>% 
  mutate(name_fct = factor(name, levels = name_fct_levels, labels = name_fct_labels))

tbl_comb_wearable_compliance_mean_sd <- 
  tbl_comb %>%
  group_by(wearable_fct, name_fct) %>%
  summarise(
    value_mean = mean(value),
    value_sd = sd(value)
  ) %>%
  mutate(
    value_mean_f = sprintf("%.1f", value_mean),
    value_sd_f = sprintf("%.1f", value_sd),
    value_f = paste0(value_mean_f, " (", value_sd_f, ")")
  ) %>%
  select(wearable_fct, name_fct, value_f) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  mutate(name_fct = paste0(name_fct, " (mean (SD))")) 

tbl_comb_wearable_compliance_median_min_max <- 
  tbl_comb %>%
  group_by(wearable_fct, name_fct) %>%
  summarise(
    value_median = median(value),
    value_min = min(value),
    value_max = max(value)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(wearable_fct, name_fct, value_f) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  mutate(name_fct = paste0(name_fct, " (median [min, max])")) 

tbl_comb_wearable_compliance <- tbl_comb_wearable_compliance_mean_sd %>% rbind(tbl_comb_wearable_compliance_median_min_max)
tbl_comb_wearable_compliance <- tbl_comb_wearable_compliance[c(1,4,2,5,3,6), ]

rm(tbl_comb)


# ------------------------------------------------------------------------------
# create final table, save
# ------------------------------------------------------------------------------

dat_F <- rbind(tbl_df_survey, tbl_comb_wearable_compliance)
# exclude rows with mean, keep median onlys
dat_F <- dat_F %>% filter(!grepl('mean', name_fct))
dat_F

dat_F_path <- file.path(here(), 'results_tables', 'manuscript_1', "table_beiwe_survey_and_wearable_compliance.csv")
fwrite(dat_F, dat_F_path); rm(dat_F_path); rm(dat_F)


