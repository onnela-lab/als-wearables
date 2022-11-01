#' @description 
#' Generate table 1. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read sample subset
# ------------------------------------------------------------------------------

study_sample_1_path <- file.path(here(), 'data_participants_other_processed', 'final_study_sample_1.csv')
study_sample_1 <- fread(study_sample_1_path) %>% as.data.frame()
dim(study_sample_1)


# ------------------------------------------------------------------------------
# read participants characteristics
# ------------------------------------------------------------------------------

#' sex: 0 Female, 1 Male, 2 Prefer to not answer
#' ethnicity: 0 Hispanic or Latino, 1 NOT Hispanic or Latino, 2 Unknown / Not Reported
#' race: 
#' 0 American Indian/Alaska Native
#' 1 Asian
#' 2 Native Hawaiian or Other Pacifi c Islander
#' 3 Black or African American
#' 4 White
#' 5 More Than One Race
#' 6 Unknown / Not Reported
#' 7 Other
#' ios_android: 
#' 1 iOS
#' 2 Android

demog_df_path <- file.path(here(), 'data_participants_other', 'wearables_table1_2022-05-15.csv')
demog_df <- 
  fread(demog_df_path) %>% 
  as.data.frame() %>%
  mutate(
    subj_id = as.numeric(str_sub(record_id, 5, 7))
  ) %>%
  inner_join(study_sample_1) %>%
  select(
    subj_id,
    age,
    sex,
    ethnicity,
    race,
    ios_android,
    wearable
  ) %>%
  mutate(
    sex = recode(sex, '0'='Female', '1'='Male', '2'='Prefer to not answer'),
    ethnicity = recode(ethnicity, '0'='Hispanic or Latino', '1'= 'Not Hispanic or Latino', '2'= 'Unknown / Not Reported'),
    race = recode(race, '0'= 'American Indian/Alaska Native', '1'= 'Asian', '2'= 'Native Hawaiian or Other Pacific Islander', '3'= 'Black or African American', '4'= 'White', '5'= 'More Than One Race', '6'= 'Unknown / Not Reported', '7'= 'Other'),
    ios_android = recode(ios_android, '1'= 'iOS', '2'= 'Android')
  )
demog_df
dim(demog_df)


#------------------------------------------------------------------------------
# read surveys responses
# ------------------------------------------------------------------------------

# ALSFRS-R in-clinic 
alsfrs_clinic_path <- file.path(here(), "data_participants_other_processed", "staff_adnimistered_alsfrs_clean.csv")
alsfrs_clinic <- 
  fread(alsfrs_clinic_path) %>%
  as.data.frame()  %>%
  filter(subj_id %in% study_sample_1$subj_id) %>%
  mutate(local_time_date = as.Date(local_time_date)) 
head(alsfrs_clinic)
range(alsfrs_clinic$local_time_date)

# ALSFRS-R in-clinic, subset to keep first (baseline) data only
alsfrs_clinic_baseline <-
  alsfrs_clinic %>%
  group_by(subj_id, beiwe_id) %>%
  filter(local_time_date == min(local_time_date)) %>%
  ungroup() %>%
  filter(subj_id %in% study_sample_1$subj_id) %>%
  as.data.frame()

length(unique(alsfrs_clinic_baseline$subj_id))
length(unique(alsfrs_clinic_baseline$beiwe_id))


# ------------------------------------------------------------------------------
# Combine and make a table
# ------------------------------------------------------------------------------

table_df <- 
  alsfrs_clinic_baseline %>% 
  select(subj_id, clinic_alsfrs_baseline = frs_total_score, clinic_alsfrs_baseline_date = local_time_date) %>%
  inner_join(demog_df) 
dim(table_df)

# make table that has additional wearable group "combined" 
wearable_fct_levels <- c("actigraph", "modus", "combined")
wearable_fct_labels <- c("ActiGraph", "Modus", "Combined")
table_df <- 
  rbind(table_df, table_df %>% mutate(wearable = "combined")) %>%
  mutate(wearable_fct = factor(wearable, levels = wearable_fct_levels, labels = wearable_fct_labels))


# ------------------------------------------------------------------------------
# count 
tbl_cnt <- 
  table_df %>%
  group_by(wearable_fct) %>%
  summarise(cnt = n()) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "cnt") %>%
  mutate(var_name = "Count all (n)", .before = everything())


# ------------------------------------------------------------------------------
# age -- mean_sd
tbl_age_mean_sd <- 
  table_df %>%
  group_by(wearable_fct) %>%
  summarise(
    value_mean = mean(age),
    value_sd = sd(age)
  ) %>%
  mutate(
    value_mean_f = sprintf("%.1f", value_mean),
    value_sd_f = sprintf("%.1f", value_sd),
    value_f = paste0(value_mean_f, " (", value_sd_f, ")")
  ) %>%
  select(wearable_fct, value_f) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  mutate(var_name = "Age (mean (SD))", .before = everything())

# age -- median_min_max
tbl_age_median_min_max <- 
  table_df %>%
  group_by(wearable_fct) %>%
  summarise(
    value_median = median(age),
    value_min = min(age),
    value_max = max(age)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(wearable_fct, value_f) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  mutate(var_name = "Age (median [min, max])", .before = everything())


# ------------------------------------------------------------------------------
# sex 
sex_levels <- names(sort(table(table_df$sex), decreasing = TRUE))
tbl_sex <- 
  table_df %>%
  group_by(wearable_fct) %>%
  mutate(cnt_all = n()
  ) %>%
  group_by(wearable_fct, sex, cnt_all) %>%
  summarise(
    cnt = n()
  ) %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(wearable_fct, value_f, sex) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  rename(var_name = sex) %>%
  mutate(var_name = factor(var_name, levels = sex_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("Sex: ", var_name, " (n (%))"))
tbl_sex[is.na(tbl_sex)] <- "0 (0.0%)"


# ------------------------------------------------------------------------------
# ethnicity 
ethnicity_levels <- names(sort(table(table_df$ethnicity), decreasing = TRUE))
tbl_ethnicity <- 
  table_df %>%
  group_by(wearable_fct) %>%
  mutate(cnt_all = n()
  ) %>%
  group_by(wearable_fct, ethnicity, cnt_all) %>%
  summarise(
    cnt = n()
  ) %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(wearable_fct, value_f, ethnicity) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  rename(var_name = ethnicity) %>%
  mutate(var_name = factor(var_name, levels = ethnicity_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("Ethnicity: ", var_name, " (n (%))")) 
tbl_ethnicity[is.na(tbl_ethnicity)] <- "0 (0.0%)"


# ------------------------------------------------------------------------------
# race 
race_levels <- names(sort(table(table_df$race), decreasing = TRUE))
tbl_race <- 
  table_df %>%
  group_by(wearable_fct) %>%
  mutate(cnt_all = n()
  ) %>%
  group_by(wearable_fct, race, cnt_all) %>%
  summarise(
    cnt = n()
  ) %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(wearable_fct, value_f, race) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  rename(var_name = race) %>%
  mutate(var_name = factor(var_name, levels = race_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("Race: ", var_name, " (n (%))")) 
tbl_race[is.na(tbl_race)] <- "0 (0.0%)"


# ------------------------------------------------------------------------------
# ios_android 
ios_android_levels <- names(sort(table(table_df$ios_android), decreasing = TRUE))
tbl_ios_android <- 
  table_df %>%
  group_by(wearable_fct) %>%
  mutate(cnt_all = n()
  ) %>%
  group_by(wearable_fct, ios_android, cnt_all) %>%
  summarise(
    cnt = n()
  ) %>%
  mutate(
    cnt_pct = cnt / cnt_all * 100,
    value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
  ) %>%
  select(wearable_fct, value_f, ios_android) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  rename(var_name = ios_android) %>%
  mutate(var_name = factor(var_name, levels = ios_android_levels)) %>%
  arrange(var_name) %>%
  mutate(var_name = paste0("OS: ", var_name, " (n (%))")) 
tbl_ios_android[is.na(tbl_ios_android)] <- "0 (0.0%)"


# ------------------------------------------------------------------------------
# clinic_alsfrs_baseline -- mean_sd
tbl_clinic_alsfrs_baseline_mean_sd <- 
  table_df %>%
  group_by(wearable_fct) %>%
  summarise(
    value_mean = mean(clinic_alsfrs_baseline),
    value_sd = sd(clinic_alsfrs_baseline)
  ) %>%
  mutate(
    value_mean_f = sprintf("%.1f", value_mean),
    value_sd_f = sprintf("%.1f", value_sd),
    value_f = paste0(value_mean_f, " (", value_sd_f, ")")
  ) %>%
  select(wearable_fct, value_f) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  mutate(var_name = "Baseline staff ALSFRS-R (mean (SD))", .before = everything())

# clinic_alsfrs_baseline -- median_min_max
tbl_clinic_alsfrs_baseline_median_min_max <- 
  table_df %>%
  group_by(wearable_fct) %>%
  summarise(
    value_median = median(clinic_alsfrs_baseline),
    value_min = min(clinic_alsfrs_baseline),
    value_max = max(clinic_alsfrs_baseline)
  ) %>%
  mutate(
    value_f = paste0(sprintf("%.0f", value_median), " [", sprintf("%.0f", value_min), ", ", sprintf("%.0f", value_max), "]")
  ) %>%
  select(wearable_fct, value_f) %>%
  pivot_wider(names_from = "wearable_fct", values_from = "value_f") %>%
  mutate(var_name = "Baseline staff ALSFRS-R (median [min, max])", .before = everything())


# ------------------------------------------------------------------------------
# combine altogether 

tbl_out <- 
  tbl_cnt %>%
  rbind(tbl_age_mean_sd) %>%
  rbind(tbl_age_median_min_max) %>%
  rbind(tbl_sex) %>%
  rbind(tbl_ethnicity) %>%
  rbind(tbl_race) %>%
  rbind(tbl_ios_android) %>%
  rbind(tbl_clinic_alsfrs_baseline_mean_sd) %>%
  rbind(tbl_clinic_alsfrs_baseline_median_min_max)
tbl_out


# ------------------------------------------------------------------------------
# save to file
# ------------------------------------------------------------------------------

tbl_out_path <- file.path(here(), 'results_tables', 'manuscript_1', "table_1.csv")
fwrite(tbl_out, tbl_out_path); rm(tbl_out_path)


