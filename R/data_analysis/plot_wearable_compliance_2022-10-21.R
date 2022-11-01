#' @description 
#' Plots wearable compliance patterns across individuals.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(cowplot)
library(here)
options(digits.secs = 0)

# source common ggplot2 theme
source(file.path(here(), "R", "data_analysis", "config_figures.R"))


# ------------------------------------------------------------------------------
# read sample subset
# ------------------------------------------------------------------------------

study_sample_1_path <- file.path(here(), 'data_participants_other_processed', 'final_study_sample_1.csv')
study_sample_1 <- fread(study_sample_1_path) %>% as.data.frame()
dim(study_sample_1)


# ------------------------------------------------------------------------------
# read wearables valid hour flag
# ------------------------------------------------------------------------------

actigraph_valid_hour_flag_path <- file.path(here(), 'data_actigraph_processed', 'actigraph_valid_hour_flag.csv')
actigraph_valid_hour_flag <- 
  fread(actigraph_valid_hour_flag_path) %>% 
  as.data.frame() %>%
  filter(subj_id %in% study_sample_1$subj_id) %>%
  mutate(
    subj_id_fct = paste0("ID ", subj_id)
  )
dim(actigraph_valid_hour_flag)

modus_valid_hour_flag_path <- file.path(here(), 'data_modus_processed', 'modus_valid_hour_flag.csv')
modus_valid_hour_flag <- 
  fread(modus_valid_hour_flag_path) %>% 
  as.data.frame() %>% 
  filter(subj_id %in% study_sample_1$subj_id) %>%
  mutate(
    subj_id_fct = paste0("ID ", subj_id)
  )
dim(modus_valid_hour_flag)


# ------------------------------------------------------------------------------
# PLOT: compliance across hours, valid only
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# actigraph

x_breaks <- seq(0, 24, by = 4)
y_breaks <- seq(0, 1, by = 0.2)

x_limits <- c(NA, 24)
y_limits <- c(0, 1)

plt_df <- 
  actigraph_valid_hour_flag %>%
  filter(valid_day_flag == 1) %>%
  group_by(subj_id, subj_id_fct, local_time_hr) %>%
  summarise(
    valid_hr_flag_mean = mean(valid_hr_flag)
  )

plt1 <- 
  ggplot(plt_df, aes(x = local_time_hr, y = valid_hr_flag_mean)) + 
  geom_bar(stat = "identity", alpha = 0.5, color = "black", fill = "grey50", size = 0.3) + 
  facet_wrap(~ subj_id, ncol = 5) + 
  scale_x_continuous(breaks = x_breaks, limits = x_limits) + 
  scale_y_continuous(breaks = y_breaks, limits = y_limits) + 
  labs(
    x = "Hour of a day",
    y = "Proportion of observation days with valid hour",
    title = "ActiGraph"
  )
plt1


# ------------------------------------------------------------------------------
# modus

plt_df <- 
  modus_valid_hour_flag %>%
  filter(valid_day_flag == 1) %>%
  group_by(subj_id, subj_id_fct, local_time_hr) %>%
  summarise(
    valid_hr_flag_mean = mean(valid_hr_flag)
  )

plt2 <- 
  ggplot(plt_df, aes(x = local_time_hr, y = valid_hr_flag_mean)) + 
  geom_bar(stat = "identity", alpha = 0.5, color = "black", fill = "grey50", size = 0.3) + 
  facet_wrap(~ subj_id, ncol = 5) + 
  scale_x_continuous(breaks = x_breaks, limits = x_limits) + 
  scale_y_continuous(breaks = y_breaks, limits = y_limits) + 
  labs(
    x = "Hour of a day",
    y = "Proportion of observation days with valid hour",
    title = "Modus"
  )
plt2


# ------------------------------------------------------------------------------
# combine plots and save

plt_list <- list(plt1, plt2)
plt <- plot_grid(plotlist = plt_list, ncol = 1, align = "hv", byrow = FALSE)
# plt

plt_fpath <- file.path(here(), "results_figures", "manuscript_1", "wearable_compliance_proportion_of_days_with_valid_hour.jpeg")
# dpi affects the size of the outcome file; for example, changing from default dpi = 300
# to dpi = 150 changed the JPEG file size to go from 1.5 MB to 500 KB
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 12,
          dpi = 150)


# ------------------------------------------------------------------------------
# PLOT: compliance across days of monitoring
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# actigraph

x_breaks <- seq(0, 200, by = 50)
y_breaks <- seq(0, 24, by = 4)

x_limits <- c(0, 200)
y_limits <- c(0, 24)

plt_df <- 
  actigraph_valid_hour_flag %>%
  # filter(valid_day_flag == 1) %>%
  group_by(subj_id, subj_id_fct) %>%
  mutate(
    day_rel = as.numeric(difftime(local_time_date, min(local_time_date), units = c("days")))
  ) %>%
  ungroup() %>% 
  group_by(subj_id, subj_id_fct, day_rel) %>%
  summarise(
    valid_hr_flag_sum = sum(valid_hr_flag)
  ) %>%
  as.data.frame()

plt1 <- 
  ggplot(plt_df, aes(x = day_rel, y = valid_hr_flag_sum)) + 
  geom_point(size = 0.8, alpha = 0.5) + 
  geom_hline(yintercept = 8, linetype = 2, size = 0.3, alpha = 0.6) + 
  facet_wrap(~ subj_id, ncol = 5) + 
  scale_x_continuous(breaks = x_breaks, limits = x_limits) +
  scale_y_continuous(breaks = y_breaks, limits = y_limits) + 
  labs(
    x = "Observation day",
    y = "Number of valid hours",
    title = "ActiGraph"
  ) 
plt1


# ------------------------------------------------------------------------------
# modus

plt_df <- 
  modus_valid_hour_flag %>%
  # filter(valid_day_flag == 1) %>%
  group_by(subj_id, subj_id_fct) %>%
  mutate(
    day_rel = as.numeric(difftime(local_time_date, min(local_time_date), units = c("days")))
  ) %>%
  ungroup() %>% 
  group_by(subj_id, subj_id_fct, day_rel) %>%
  summarise(
    valid_hr_flag_sum = sum(valid_hr_flag)
  ) %>%
  as.data.frame()

plt2 <- 
  ggplot(plt_df, aes(x = day_rel, y = valid_hr_flag_sum)) + 
  geom_point(size = 0.8, alpha = 0.5) + 
  geom_hline(yintercept = 8, linetype = 2, size = 0.3, alpha = 0.6) + 
  facet_wrap(~ subj_id, ncol = 5) + 
  scale_x_continuous(breaks = x_breaks, limits = x_limits) +
  scale_y_continuous(breaks = y_breaks, limits = y_limits) + 
  labs(
    x = "Observation day",
    y = "Number of valid hours",
    title = "Modus"
  ) 
plt2


# ------------------------------------------------------------------------------
# combine plots and save

plt_list <- list(plt1, plt2)
plt <- plot_grid(plotlist = plt_list, ncol = 1, align = "hv", byrow = FALSE)
# plt

plt_fpath <- file.path(here(), "results_figures", "manuscript_1", "wearable_compliance_valid_hours_across_days.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 12,
          dpi = 150)


# ------------------------------------------------------------------------------
# total compliant days

# wear compliance actigraph 
compliance_wear_calc_ag <-
  actigraph_valid_hour_flag %>%
  group_by(beiwe_id,local_time_date) %>% 
  filter(local_time_hr == 0)
sum(compliance_wear_calc_ag$valid_day_flag) / nrow(compliance_wear_calc_ag)

# wear compliance modus 
compliance_wear_calc_m<-
  modus_valid_hour_flag %>% 
  group_by(beiwe_id,local_time_date) %>% 
  filter(local_time_hr == 0)

sum(compliance_wear_calc_m$valid_day_flag) / nrow(compliance_wear_calc_m)






