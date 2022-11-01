#' @description 
#' Estimate the LMM models: measure ~ time.

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
options(digits.secs = 0)
options(scipen = 999)

# source common ggplot2 theme
source(file.path(here(), "R", "data_analysis", "config_figures.R"))
# source file with functions and model variable names/labels used across >1 script
source(file.path(here(), "R", "data_analysis", "utils.R"))


# ------------------------------------------------------------------------------
# read sample subset
# ------------------------------------------------------------------------------

study_sample_1_path <- file.path(here(), 'data_participants_other_processed', 'final_study_sample_1.csv')
study_sample_1 <- fread(study_sample_1_path) %>% as.data.frame()
dim(study_sample_1)


# ------------------------------------------------------------------------------
# read wearable data daily measures
# ------------------------------------------------------------------------------

# Actigraph -- vendor-provided
ag_vendorprovided_path <- file.path(here(), "data_actigraph_processed", "actigraph_t24hr_vendorprovided.csv") 
ag_vendorprovided <- 
  fread(ag_vendorprovided_path) %>%
  as.data.frame()  %>%
  filter(subj_id %in% study_sample_1$subj_id) %>%
  mutate(local_time_date = as.Date(local_time_date)) %>%
  group_by(subj_id) %>%
  mutate(
    day_relative = as.numeric(difftime(local_time_date, min(local_time_date), units = c("days"))),
    month_relative = day_relative/30.5
  ) %>%
  ungroup() 

# Actigraph -- investigator-derived, imputed
ag_investigatorderived_imp_path <- file.path(here(), "data_actigraph_processed", "actigraph_t24hr_investigatorderived_imputed.csv") 
ag_investigatorderived_imp <- 
  fread(ag_investigatorderived_imp_path) %>%
  as.data.frame()  %>%
  filter(subj_id %in% study_sample_1$subj_id) %>%
  mutate(local_time_date = as.Date(local_time_date)) %>%
  group_by(subj_id) %>%
  mutate(
    day_relative = as.numeric(difftime(local_time_date, min(local_time_date), units = c("days"))),
    month_relative = day_relative/30.5
  ) %>%
  ungroup() 

# Modus -- investigrator-derived
modus_vendorprovided_path <- file.path(here(), "data_modus_processed", "modus_t24hr_vendorprovided.csv") 
modus_vendorprovided <- 
  fread(modus_vendorprovided_path) %>%
  as.data.frame()  %>%
  filter(subj_id %in% study_sample_1$subj_id) %>%
  mutate(local_time_date = as.Date(local_time_date)) %>%
  group_by(subj_id) %>%
  mutate(
    day_relative = as.numeric(difftime(local_time_date, min(local_time_date), units = c("days"))),
    month_relative = day_relative/30.5
  ) %>%
  ungroup() 



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
  out_y_name <- rep(NA, pp)
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
  for (i in 1 : pp){ # i <- 4
    message(paste0("i = ", i))
    # define model outcome y
    y_name <- var_name_vec[i]
    mod_df <- dat
    mod_df$y <- mod_df %>% pull(y_name) 
    mod_df <- mod_df %>% filter(!is.na(y)) 
    # fit model
    mod_out <- lmer(y ~ month_relative + (1 + month_relative | beiwe_id_fct), data = mod_df, control = lmerControl(optimizer = "bobyqa"))
    # generate additional model information / pull model parameters
    mod_out_ci     <- get_mod_ci(mod_out, mod_df)
    mod_out_conv   <- merMod_has_converged(mod_out)
    mod_out_s      <- summary(mod_out)
    mod_out_sc     <- mod_out_s$coefficients %>% as.data.frame() %>% janitor::clean_names()
    mod_out_est    <- round(mod_out_sc$estimate, 3)
    mod_out_pvalue <- round(mod_out_sc$pr_t, 3)
    mod_out_r2     <- round(r.squaredGLMM(mod_out), 3)
    # mod_out_ranef_skew  <- round(sapply(ranef(mod_out)$beiwe_id_fct, skewness), 3)
    # append model parameters
    out_y_name[i]     <- y_name
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
    out_y_label = var_label_vec, 
    out_x_label = rep("Time [months]", pp),
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


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# run LMM models
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# data frame to store the 
out_df_all <- data.frame()

# ------------------------------------------------------------------------------
# Actigraph -- vendor-provided (wearfiltered)

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
# save 

dim(out_df_all)

out_path <- file.path(here(), 'results_tables', 'manuscript_1', "table_lmm_wearable_over_time.csv")
fwrite(out_df_all, out_path)

