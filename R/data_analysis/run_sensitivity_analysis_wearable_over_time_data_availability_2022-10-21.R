#' @description 
#' Run sensitivity analysis that observes changes in estimated 
#' (a) population-level intercept/slope
#' (b) conditional intercepts/slopes (population-level + individual-specific)
#' as we consider different scenarios of data availability. 

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


# ------------------------------------------------------------------------------
# read sample subset
# ------------------------------------------------------------------------------

study_sample_1_path <- file.path(here(), 'data_participants_other_processed', 'final_study_sample_1.csv')
study_sample_1 <- fread(study_sample_1_path) %>% as.data.frame()
dim(study_sample_1)


# ------------------------------------------------------------------------------
# read wearable data daily measures
# ------------------------------------------------------------------------------

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


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# run LMM models
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# use only one pair of wearables
var_name <- "tac"
var_label <- "Total activity counts"
dataset_name = "ActiGraph investigator-derived IMP"

dat <- ag_investigatorderived_imp

# create beiwe factor
beiwe_id_levels <- sort(unique(dat$beiwe_id))
dat$beiwe_id_fct <- factor(dat$beiwe_id, levels = beiwe_id_levels)

# define model outcome y
y_name <- var_name
dat$y <- dat %>% pull(y_name) 
dat <- dat %>% filter(!is.na(y)) 

# define weeks grid for different experiment setups 
dat$relative_week <- floor(dat$day_relative / 7)
rw_min <- min(dat$relative_week)
rw_max <- max(dat$relative_week)

# list with relative week subsets to be used 
setup_weeks_list <- list(
  c(rw_min : rw_max),
  as.numeric(sapply(seq(rw_min, rw_max, by = 4), function(val) val + c(0, 1))),
  as.numeric(sapply(seq(rw_min, rw_max, by = 6), function(val) val + c(0, 1))),
  as.numeric(sapply(seq(rw_min, rw_max, by = 8), function(val) val + c(0, 1))),
  as.numeric(sapply(seq(rw_min, rw_max, by = 10), function(val) val + c(0, 1))),
  seq(rw_min, rw_max, by = 3),
  seq(rw_min, rw_max, by = 5),
  seq(rw_min, rw_max, by = 7),
  seq(rw_min, rw_max, by = 9)
)

# list with relative week subsets to be used 
setup_label_vec <- c(
  "All D",
  # 2 weeks of data, X weeks of break
  "2wD+2wB",
  "2wD+4wB",
  "2wD+6wB",
  "2wD+8wB",
  # 1 week of data, X weeks of break
  "1wD+2wB",
  "1wD+4wB",
  "1wD+6wB",
  "1wD+8wB"
)

# create data frames to store the results 
ranef_slope_df_all <- data.frame()
ranef_interc_df_all <- data.frame()
betaest_interc_df_all <- data.frame()
betaest_slope_df_all <- data.frame()
# iterate over sensitivity analysis setups
for (i in 1 : length(setup_label_vec)){ # i <- 1
  print(paste0("i = ", i))
  # pull current setup weeks subset and label 
  setup_weeks <- setup_weeks_list[[i]]
  setup_label <- setup_label_vec[i]
  # subset data to keep specific weeks only
  mod_df <- dat %>% filter(relative_week %in% setup_weeks)
  # fit model
  mod_out <- lmer(y ~ month_relative + (1 + month_relative | beiwe_id_fct), data = mod_df, control = lmerControl(optimizer = "bobyqa"))
  # retrieve model estimates
  interc_popul <- summary(mod_out)$coef[1, 1]
  slope_popul  <- summary(mod_out)$coef[2, 1]
  interc_popul_pval <- summary(mod_out)$coef[1, 5]
  slope_popul_pval  <- summary(mod_out)$coef[2, 5]
  interc_popul_ci <- confint(mod_out, parm = "(Intercept)", method = "profile")
  slope_popul_ci  <- confint(mod_out, parm = "month_relative", method = "profile")
  # intercept
  ranef_interc_df <- 
    ranef(mod_out)$beiwe_id_fct %>% 
    janitor::clean_names() %>%
    select(interc_ranef = intercept) %>%
    mutate(interc_cond = interc_ranef + interc_popul) %>%
    rownames_to_column(var = "beiwe_id") %>%
    mutate(setup_label = setup_label)
  betaest_interc_df <- data.frame(
    interc_popul = interc_popul, 
    interc_popul_pval = interc_popul_pval,
    interc_popul_ci_lo = interc_popul_ci[1],
    interc_popul_ci_up = interc_popul_ci[2],
    setup_label = setup_label,
    data_points_cnt = nrow(mod_df)
  )
  # slope
  ranef_slope_df <- 
    ranef(mod_out)$beiwe_id_fct %>% 
    select(slope_ranef = month_relative) %>%
    mutate(slope_cond = slope_ranef + slope_popul) %>%
    rownames_to_column(var = "beiwe_id") %>%
    mutate(setup_label = setup_label)
  betaest_slope_df <- data.frame(
    slope_popul = slope_popul, 
    slope_popul_pval = slope_popul_pval,
    slope_popul_ci_lo = slope_popul_ci[1],
    slope_popul_ci_up = slope_popul_ci[2],
    setup_label = setup_label,
    data_points_cnt = nrow(mod_df)
  )
  # append 
  ranef_slope_df_all <- rbind(ranef_slope_df_all, ranef_slope_df)
  ranef_interc_df_all <- rbind(ranef_interc_df_all, ranef_interc_df)
  betaest_slope_df_all <- rbind(betaest_slope_df_all, betaest_slope_df)
  betaest_interc_df_all <- rbind(betaest_interc_df_all, betaest_interc_df)
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# table the results
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# format table

# to calculate change 
ref_val <- betaest_interc_df_all %>% filter(setup_label == "All D") %>% pull(interc_popul)
tbl_df_A <-
  betaest_interc_df_all %>% 
  mutate(
    pct_change = 100 * ((interc_popul - ref_val)/ref_val)
  ) %>%
  mutate(
    interc_popul_f = sprintf("%.0f", interc_popul),
    interc_popul_ci_lo_f = sprintf("%.0f", interc_popul_ci_lo),
    interc_popul_ci_up_f = sprintf("%.0f", interc_popul_ci_up),
    interc_popul_pval_f = sprintf("%.3f", interc_popul_pval),
    interc_pct_change = sprintf("%.1f", pct_change)
  ) %>%
  mutate(
    # interc_popul_f_F = paste0(interc_popul_f, " [", interc_popul_ci_lo_f, ", ", interc_popul_ci_up_f, "] (", interc_popul_pval_f, ")"),
    interc_popul_f_F = paste0(interc_popul_f, " [", interc_popul_ci_lo_f, ", ", interc_popul_ci_up_f, "]")
  ) %>%
  select(
    setup_label, 
    data_points_cnt,
    interc_popul_f_F,
    interc_pct_change
  ) 
tbl_df_A

ref_val <- betaest_slope_df_all %>% filter(setup_label == "All D") %>% pull(slope_popul)
tbl_df_B <-
  betaest_slope_df_all %>% 
  mutate(
    pct_change = 100 * ((slope_popul - ref_val)/ref_val)
  ) %>%
  mutate(
    slope_popul_f = sprintf("%.0f", slope_popul),
    slope_popul_ci_lo_f = sprintf("%.0f", slope_popul_ci_lo),
    slope_popul_ci_up_f = sprintf("%.0f", slope_popul_ci_up),
    slope_popul_pval_f = sprintf("%.3f", slope_popul_pval),
    slope_pct_change = sprintf("%.1f", pct_change)
  ) %>%
  mutate(
    slope_popul_f_F = paste0(slope_popul_f, " [", slope_popul_ci_lo_f, ", ", slope_popul_ci_up_f, "] (", slope_popul_pval_f, ")"),
  ) %>%
  select(
    setup_label, 
    slope_popul_f_F ,
    slope_pct_change
  ) 
tbl_df_B

tbl_df <- 
  tbl_df_A %>%
  left_join(tbl_df_B)
tbl_df


# ------------------------------------------------------------------------------
# save to file
# ------------------------------------------------------------------------------

out_path <- file.path(here(), 'results_tables', 'manuscript_1', "table_sensitivity_analysis_wearable_over_time_data_availability.csv")
fwrite(tbl_df, out_path); rm(out_path)


