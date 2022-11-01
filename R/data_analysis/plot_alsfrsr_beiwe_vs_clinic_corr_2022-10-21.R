#' @description 
#' Plot correlation between ALSFRS Beiwe and staff at 3 different time points

rm(list = ls())
library(tidyverse)
library(lubridate)
library(janitor)
library(cowplot)
library(data.table)
library(here)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

# final sample of participants
study_sample_1_path <- file.path(here(), 'data_participants_other_processed', 'final_study_sample_1.csv')
study_sample_1 <- fread(study_sample_1_path) %>% as.data.frame()
dim(study_sample_1)


#  final date range for Beiwe survey dates to be used
study_beiwe_survey_dates_range_1_path <- file.path(here(), 'data_participants_other_processed', 'final_beiwe_survey_dates_range_1.csv')
study_beiwe_survey_dates_range_1 <- fread(study_beiwe_survey_dates_range_1_path) %>% as.data.frame()
dim(study_beiwe_survey_dates_range_1)


# beiwe alsfrsr complete answers
alsfrsr_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_alsfrs.csv")
alsfrsr_beiwe <- 
  fread(alsfrsr_beiwe_path) %>%
  as.data.frame() %>%
  inner_join(study_sample_1) %>%
  inner_join(study_beiwe_survey_dates_range_1) %>%
  filter(
    local_time_date <= beiwe_survey_end
  ) %>%
  select(
    subj_id, beiwe_id, wearable, local_time_date, answers_score
  ) %>%
  select(
    subj_id,
    beiwe_id, 
    local_time_date_beiwe = local_time_date, 
    value_beiwe = answers_score
  )
# checks
dim(alsfrsr_beiwe)
length(unique(alsfrsr_beiwe$subj_id))


# in clinic alsfrsr 
alsfrsr_clinic_path <- file.path(here(), "data_participants_other_processed", "staff_adnimistered_alsfrs_clean.csv")
alsfrsr_clinic <- 
  fread(alsfrsr_clinic_path) %>%
  as.data.frame()  %>%
  inner_join(study_sample_1) %>%
  filter(subj_id %in% study_sample_1$subj_id) %>%
  select(
    subj_id,
    beiwe_id,
    redcap_event_name,
    local_time_date_clinic = local_time_date,
    value_clinic = frs_total_score
  )
dim(alsfrsr_clinic)
range(alsfrsr_clinic$local_time_date)
length(unique(alsfrsr_clinic$subj_id))


# ------------------------------------------------------------------------------
# match
# ------------------------------------------------------------------------------

redcap_event_levels <- c(
  "screeningbaseline_arm_1",
  "month_3_arm_1",
  "month_6_arm_1"
)
redcap_event_labels <- c(
  "Baseline",
  "Month 3",
  "Month 6"
)
plt_title_labels <- c(
  "a",
  "b",
  "c"
)

plt_list <- list()
for (i in 1 : length(redcap_event_levels)){ # i <- 1
  message(paste0("i = ", i))
  # data subset
  alsfrsr_clinic_sub <- 
    alsfrsr_clinic %>%
    filter(redcap_event_name == redcap_event_levels[i]) %>%
    left_join(alsfrsr_beiwe, by = c("subj_id", "beiwe_id")) %>%
    mutate(
      days_diff = as.numeric(difftime(local_time_date_beiwe, local_time_date_clinic, units = c("days"))),
      days_diff_abs = abs(days_diff)
    ) %>%
    group_by(subj_id, beiwe_id) %>%
    filter(days_diff_abs == min(days_diff_abs)) %>%
    filter(days_diff == min(days_diff)) %>%
    as.data.frame() %>%
    arrange(days_diff)
  # subset to only keep the beiwe survey that is no more than 28 days away
  plt_df <- 
    alsfrsr_clinic_sub %>%
    filter(days_diff_abs <= 28)
  message(paste0("nrow(alsfrsr_clinic_sub): ", nrow(alsfrsr_clinic_sub), ", nrow(plt_df): ", nrow(plt_df)))
  # compute correlation
  corr_val <- cor(plt_df$value_clinic, plt_df$value_beiwe)
  # compute mean of the two
  mean_vals <- round(c(mean(plt_df$value_clinic), mean(plt_df$value_beiwe)), 1)
  names(mean_vals) <- paste0(redcap_event_labels[i], "_", c("value_clinic", "value_beiwe"))
  print(mean_vals)
  corr_val_label = paste0(redcap_event_labels[i], "\ncorr = ", sprintf("%.2f", corr_val))
  axis_breaks <- seq(10, 50, by = 10)
  axis_limits <- c(10, 50)
  # axis_limits <- seq(10, 50, by = 10)
  # generate plot
  plt <- 
    ggplot(plt_df, aes(x = value_clinic, y = value_beiwe)) + 
    geom_smooth(method = "lm", se = FALSE, linetype = 2, color = "blue", size = 0.8) + 
    geom_abline(intercept = 0, slope = 1, linetype = 2, color = "grey40", size = 0.6) +
    geom_point(alpha = 0.7) + 
    annotate(geom = "text", x = 13, y = 45, 
             label = corr_val_label, color = "blue", 
             hjust = 0) + 
    scale_x_continuous(breaks = axis_breaks, limits = axis_limits) + 
    scale_y_continuous(breaks = axis_breaks, limits = axis_limits) + 
    labs(
      x = "ALSFRS-R (staff-administered)",
      y = "ALSFRS-R (self-entry)",
      title = plt_title_labels[i] 
    ) 
  plt_list[[length(plt_list) + 1]] <- plt
}


# ------------------------------------------------------------------------------
# combine plots and save

plt <- plot_grid(plotlist = plt_list, ncol = 3, align = "hv", byrow = TRUE)
plt
plt_fpath <- file.path(here(), "results_figures", "manuscript_1", "survey_alsfrs_beiwe_vs_clinic_corr_no_dashed_ctr_line.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 3 * 3.5, base_height = 3.5, dpi = 150)




