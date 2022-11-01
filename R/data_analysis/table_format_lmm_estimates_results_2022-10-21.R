
#' @description 
#' This script takes the precomputed results (LMM model estimation) and format them 
#' into a nice table.


rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(dplyr.summarise.inform = FALSE)
options(digits.secs = 0)
options(scipen = 999)

# source common ggplot2 theme
source(file.path(here(), "R", "data_analysis", "config_figures.R"))
# source file with functions and model variable names/labels used across >1 script
source(file.path(here(), "R", "data_analysis", "utils.R"))


# ------------------------------------------------------------------------------
# read precomputed data
# ------------------------------------------------------------------------------

out_path <- file.path(here(), 'results_tables', 'manuscript_1', "table_lmm_wearable_over_time.csv")
wear_vs_time <- 
  fread(out_path) %>%
  as.data.frame() %>%
  mutate(model_no = row_number(), model_group = 1, .before = everything())

out_path <- file.path(here(), 'results_tables', 'manuscript_1', "table_lmm_alsfrsr_vs_wearable.csv")
alsfrsr_vs_wear <- 
  fread(out_path) %>%
  as.data.frame() %>%
  mutate(model_no = row_number(), model_group = 2, .before = everything())

out_path <- file.path(here(), 'results_tables', 'manuscript_1', "table_lmm_roads_vs_wearable.csv")
roads_vs_wear <- 
  fread(out_path) %>%
  as.data.frame() %>%
  mutate(model_no = row_number(), model_group = 3, .before = everything())

# combine all together
dat_all <- wear_vs_time %>% rbind(alsfrsr_vs_wear) %>% rbind(roads_vs_wear) 
table(dat_all$wearable_group)
table(dat_all$data_set)
# TODO check: this is good as long as we DO NOT have any (-1); both (1) and (0) are fine
table(dat_all$conv)
dim(dat_all)


# ------------------------------------------------------------------------------
# format: supplementary material
# ------------------------------------------------------------------------------

# function to format numeric value to a character
# rounds depending on the value to be formatted (large numbers rounded to 0 decimal
# places, abs()<10 numbers rounded to 3 decimal places)
format_num_to_char <-  function(val){
  val_abs <- abs(val)
  if (val_abs < 10){
    val_f = sprintf("%.3f", val)
  } else if (val_abs >= 10 & val_abs < 100) {
    val_f = sprintf("%.2f", val)
  } else if (val_abs >= 100 & val_abs < 1000){
    val_f = sprintf("%.1f", val)
  } else {
    val_f = sprintf("%.0f", val)
  }
  return(val_f)
}

wearable_group_levels <- c("AG", "M")
data_set_levels <- c("VP", "ID")

# format columns content
rm(dat_all_f)
dat_all_f0 <- 
  dat_all %>%
  mutate(
    # replace wearable group with abbreviations
    wearable_group = replace(wearable_group, wearable_group == "ActiGraph", "AG"),
    wearable_group = replace(wearable_group, wearable_group == "Modus", "M"),
    wearable_group_fct = factor(wearable_group, levels = wearable_group_levels),
    # replace wearable group with abbreviations
    data_set = replace(data_set, data_set == "Investigator-derived", "IDMs"),
    data_set = replace(data_set, data_set == "Vendor-provided", "VDMs"),
    data_set_fct = factor(data_set, levels = data_set_levels)
  ) %>%
  # arrange(model_group, wearable_group_fct, data_set_fct, desc(intcp_est)) %>%
  rowwise() %>%
  mutate_at(vars(intcp_est : R2c), format_num_to_char) %>%
  ungroup() %>%
  mutate(
    intcp_f = paste0(intcp_est, " [", intcp_ci_lo, ", ", intcp_ci_up, "]"),
    slope_f = paste0(slope_est, " [", slope_ci_lo, ", ", slope_ci_up, "] (", slope_pval, ")")
  ) 
dat_all_f <- 
  dat_all_f0 %>% 
  select(model_no, wearable_group, data_set, y_label, x_label, intcp_f, slope_f, R2m, R2c) %>% 
  as.data.frame() 

names(dat_all_f) <- c("No.", "Group", "Data set",
                      "Outcome", "Covariate", 
                      "Interc. est. [95% CI]", "Slope est. [95% CI] (p-val.)",
                      "R2m", "R2c")
dat_all_f
# View(dat_all_f)


# ------------------------------------------------------------------------------
# save to file
# ------------------------------------------------------------------------------

path_tmp <- file.path(here(), "results_tables", "manuscript_1", "table_formatted_lmm_estimates_results_all.csv")
fwrite(dat_all_f, path_tmp)


# ------------------------------------------------------------------------------
# check which have significant association with both (1) ROADS and (2) ALSFRS-R and 
# (3) have significant change over time
# ------------------------------------------------------------------------------

sign_sub <- 
  dat_all_f0 %>%
  # filter(model_group %in% c(2,3)) %>%
  filter(slope_pval < 0.05) %>%
  as.data.frame() %>%
  group_by(model_no) %>%
  filter(n() == 3) %>%
  as.data.frame() %>%
  filter(model_group == 1) %>%
  select(wearable_group, data_set, y_label)
sign_sub




