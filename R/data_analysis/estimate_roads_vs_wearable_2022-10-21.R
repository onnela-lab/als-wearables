#' @description 
#' Estimates the LMM models: ROADS ~ measure.
                
rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(lme4)
library(lmerTest)
library(cowplot)
library(MuMIn)
library(e1071)    
library(here)
options(dplyr.summarise.inform = FALSE)
options(digits.secs = 0)
options(scipen = 999)

# source common ggplot2 theme
source(file.path(here(), "R", "data_analysis", "config_figures.R"))
# source file with functions and model variable names/labels used across >1 script
source(file.path(here(), "R", "data_analysis", "utils.R"))


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
  ) %>%
  rename(survey_local_time_date = local_time_date)
# checks
dim(roads_beiwe)
length(unique(roads_beiwe$subj_id))


# ------------------------------------------------------------------------------
# read wearable data daily measures, combine with survey
# ------------------------------------------------------------------------------

# - to EACH daily measurement, attach first ALL surveys for a participant define 
#   number of days difference between daily measurement date (wearbale_local_time_date) 
#   and survey date (survey_local_time_date)
# - compute days distance bw survey and wearable metrics
# - filter to keep only 1 (the closest) survey in time for each daily measurement
# - filter to keep only these which are not further from the closest survey than 7 days

days_diff_max <- 7

# Actigraph -- vendor-provided
ag_vendorprovided_path <- file.path(here(), "data_actigraph_processed", "actigraph_t24hr_vendorprovided.csv") 
ag_vendorprovided <- 
  fread(ag_vendorprovided_path) %>%
  as.data.frame()  %>%
  filter(subj_id %in% study_sample_1$subj_id) %>%
  mutate(local_time_date = as.Date(local_time_date)) %>%
  rename(wearbale_local_time_date = local_time_date) %>%
  left_join(roads_beiwe) %>%
  mutate(
    days_diff = abs(as.numeric(difftime(wearbale_local_time_date, survey_local_time_date, units = c("days"))))
  ) %>%
  group_by(beiwe_id, wearbale_local_time_date) %>%
  arrange(days_diff) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(days_diff <= days_diff_max) 


# Actigraph -- investigrator-derived, imputed
ag_investigatorderived_imp_path <- file.path(here(), "data_actigraph_processed", "actigraph_t24hr_investigatorderived_imputed.csv") 
ag_investigatorderived_imp <- 
  fread(ag_investigatorderived_imp_path) %>%
  as.data.frame()  %>%
  filter(subj_id %in% study_sample_1$subj_id) %>%
  mutate(local_time_date = as.Date(local_time_date)) %>%
  rename(wearbale_local_time_date = local_time_date) %>%
  left_join(roads_beiwe) %>%
  mutate(
    days_diff = abs(as.numeric(difftime(wearbale_local_time_date, survey_local_time_date, units = c("days"))))
  ) %>%
  group_by(beiwe_id, wearbale_local_time_date) %>%
  arrange(days_diff) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(days_diff <= days_diff_max) 


# Modus -- investigrator-derived
modus_vendorprovided_path <- file.path(here(), "data_modus_processed", "modus_t24hr_vendorprovided.csv") 
modus_vendorprovided <- 
  fread(modus_vendorprovided_path) %>%
  as.data.frame()  %>%
  filter(subj_id %in% study_sample_1$subj_id) %>%
  mutate(local_time_date = as.Date(local_time_date)) %>%
  rename(wearbale_local_time_date = local_time_date) %>%
  left_join(roads_beiwe) %>%
  mutate(
    days_diff = abs(as.numeric(difftime(wearbale_local_time_date, survey_local_time_date, units = c("days"))))
  ) %>%
  group_by(beiwe_id, wearbale_local_time_date) %>%
  arrange(days_diff) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(days_diff <= days_diff_max) 


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# util functions to run LMM models
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# function to fit models separately for each predictor listed in var_name_vec
get_out_df <- function(dat, var_name_vec, var_label_vec){
  pp <- length(var_name_vec)
  # objects to store model estimation results
  out_x_name <- rep(NA, pp)
  out_intcp_est <- rep(NA, pp)
  out_slope_est <- rep(NA, pp)
  out_intcp_pval <- rep(NA, pp)
  out_slope_pval <- rep(NA, pp)
  out_intcp_ci_lo <- rep(NA, pp)
  out_slope_ci_lo <- rep(NA, pp)
  out_intcp_ci_up <- rep(NA, pp)
  out_slope_ci_up <- rep(NA, pp)
  out_R2m <- rep(NA, pp)
  out_R2c <- rep(NA, pp)
  out_conv <- rep(NA, pp)
  # create beiwe factor
  beiwe_id_levels <- sort(unique(dat$beiwe_id))
  dat$beiwe_id_fct <- factor(dat$beiwe_id, levels = beiwe_id_levels)
  # iterate over different wearable factors
  for (i in 1 : pp){ # i <- 12
    print(paste0("i = ", i))
    x_name <- var_name_vec[i]
    # prepare model data frame
    mod_df <- 
      dat %>%
      rename_with(~ c("y"), all_of("answers_score_normed"))  %>%
      rename_with(~ c("x_orig"), all_of(x_name)) %>% 
      filter(!is.na(x_orig)) %>%
      mutate(x = as.numeric(scale(x_orig, center = TRUE, scale = TRUE))) %>%
      group_by(beiwe_id_fct, subj_id, survey_local_time_date, y) %>%
      summarise(
        x_mean = mean(x, na.rm = TRUE),
        x_sd = sd(x, na.rm = TRUE),
        x_n = sum(!is.na(x))
      ) %>%
      ungroup()
    # fit model
    mod_out <- lmer(y ~ x_mean + (1 + x_mean | beiwe_id_fct), data = mod_df, control = lmerControl(optimizer = "bobyqa"))
    # generate additional model information / pull model parameters
    mod_out_ci <- get_mod_ci(mod_out, mod_df)
    mod_out_conv   <- merMod_has_converged(mod_out)
    mod_out_s      <- summary(mod_out)
    mod_out_s_c    <- mod_out_s$coefficients %>% as.data.frame() %>% janitor::clean_names()
    mod_out_est    <- round(mod_out_s_c$estimate, 3)
    mod_out_pvalue <- round(mod_out_s_c$pr_t, 3)
    mod_out_r2     <- round(r.squaredGLMM(mod_out), 3)
    mod_out_ranef_skew  <- round(sapply(ranef(mod_out)$beiwe_id_fct, skewness), 3)
    # append model parameters
    out_x_name[i]     <- x_name
    out_intcp_est[i]  <- mod_out_est[1]
    out_slope_est[i]  <- mod_out_est[2]
    out_intcp_pval[i] <- mod_out_pvalue[1]
    out_slope_pval[i] <- mod_out_pvalue[2]
    out_intcp_ci_lo[i] <- mod_out_ci[1]
    out_slope_ci_lo[i] <- mod_out_ci[2]
    out_intcp_ci_up[i] <- mod_out_ci[3]
    out_slope_ci_up[i] <- mod_out_ci[4]
    out_R2m[i]        <- mod_out_r2[1]
    out_R2c[i]        <- mod_out_r2[2]
    out_conv[i]       <- mod_out_conv
  }
  # combine into model df
  out_df <- data.frame(
    # out_dataset_name = rep(dataset_name, pp),
    out_y_label = rep("ROADS", pp),
    out_x_label = var_label_vec,
    out_intcp_est,
    out_intcp_pval,
    out_slope_est,
    out_slope_pval,
    out_intcp_ci_lo,
    out_slope_ci_lo,
    out_intcp_ci_up,
    out_slope_ci_up,
    out_R2m,
    out_R2c,
    out_conv
  ) 
  names(out_df) <- gsub("out_", "", names(out_df))
  return(out_df)
}


# data frame to store the 
out_df_all <- data.frame()

# ------------------------------------------------------------------------------
# Actigraph -- Vendor-prov. 

names(ag_vendorprovided)
# dat <- ag_vendorprovided
var_name_vec  <- var_name_vec_actigraph_vendorprovided
var_label_vec <- var_label_vec_actigraph_vendorprovided
# add "wearfiltered" for all but sleep
var_name_vec[1 : 12] <- paste0("wearfiltered", var_name_vec[1 : 12])
out_df <- get_out_df(ag_vendorprovided, var_name_vec, var_label_vec)
out_df <- out_df %>% mutate(wearable_group = "ActiGraph", data_set = "Vendor-provided", .before = everything())
out_df_all <- rbind(out_df_all, out_df); rm(out_df)


# ------------------------------------------------------------------------------
# Actigraph -- investigator-derived (imputed)

names(ag_investigatorderived_imp)
var_name_vec <- var_name_vec_actigraph_investigatorderived
var_label_vec <- var_label_vec_actigraph_investigatorderived
out_df <- get_out_df(ag_investigatorderived_imp, var_name_vec, var_label_vec)
out_df <- out_df %>% mutate(wearable_group = "ActiGraph", data_set = "Investigator-derived", .before = everything())
out_df_all <- rbind(out_df_all, out_df); rm(out_df)

# ------------------------------------------------------------------------------
# Modus -- vendor-provided

names(modus_vendorprovided)
var_name_vec  <- var_name_vec_modus_vendorprovided
var_label_vec <- var_label_vec_modus_vendorprovided
out_df <- get_out_df(modus_vendorprovided, var_name_vec, var_label_vec)
out_df <- out_df %>% mutate(wearable_group = "Modus", data_set = "Vendor-provided", .before = everything())
out_df_all <- rbind(out_df_all, out_df); rm(out_df)


# ------------------------------------------------------------------------------
# save object to file 
# ------------------------------------------------------------------------------

out_path <- file.path(here(), 'results_tables', 'manuscript_1', "table_lmm_roads_vs_wearable.csv")
fwrite(out_df_all, out_path)

