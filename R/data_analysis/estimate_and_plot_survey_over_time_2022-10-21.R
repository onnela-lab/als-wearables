#' @description 
#' This script uses linear mixed models (LMM) to estimate change of survey outcomes 
#' over time.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(lme4)
library(lmerTest)
library(cowplot)
library(here)
library(broom.mixed)
library(MuMIn)
options(digits.secs = 0)
options(scipen = 999)

# source common ggplot2 theme
source(file.path(here(), "R", "data_analysis", "config_figures.R"))
# source other functions  used across >1 sctips
source(file.path(here(), "R", "data_analysis", "utils.R"))


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# final participants sample
study_sample_1_path <- file.path(here(), 'data_participants_other_processed', 'final_study_sample_1.csv')
study_sample_1 <- fread(study_sample_1_path) %>% as.data.frame()
dim(study_sample_1)


#  final date range for Beiwe survey dates to be used
study_beiwe_survey_dates_range_1_path <- file.path(here(), 'data_participants_other_processed', 'final_beiwe_survey_dates_range_1.csv')
study_beiwe_survey_dates_range_1 <- fread(study_beiwe_survey_dates_range_1_path) %>% as.data.frame()
dim(study_beiwe_survey_dates_range_1)


# beiwe roads complete answers
roads_beiwe_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly_complete_roads.csv")
roads_beiwe <- 
  fread(roads_beiwe_path) %>%
  as.data.frame() %>%
  inner_join(study_sample_1) %>%
  inner_join(study_beiwe_survey_dates_range_1) %>%
  filter(
    local_time_date <= beiwe_survey_end
  )  %>%
  select(-answers_score) %>%
  select(
    subj_id, beiwe_id, wearable, local_time_date, answers_score = answers_score_normed
  ) %>%
  mutate(
    survey_type = "beiwe",
    survey_name = "roads"
  )
# checks
dim(roads_beiwe)
length(unique(roads_beiwe$subj_id))


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
  mutate(
    survey_type = "beiwe",
    survey_name = "alsfrsr"
  )
# checks
dim(alsfrsr_beiwe)
length(unique(alsfrsr_beiwe$subj_id))

# in-clinic alsfrs 
alsfrs_clinic_path <- file.path(here(), "data_participants_other_processed", "staff_adnimistered_alsfrs_clean.csv")
alsfrs_clinic <- 
  fread(alsfrs_clinic_path) %>%
  as.data.frame()  %>%
  inner_join(study_sample_1)  %>%
  filter(redcap_event_name %in% c("screeningbaseline_arm_1", "month_3_arm_1", "month_6_arm_1")) %>%
  select(
    subj_id, beiwe_id, wearable, local_time_date, answers_score = frs_total_score
  ) %>%
  mutate(
    survey_type = "in-clinic",
    survey_name = "alsfrsr"
  )
#checks
dim(alsfrs_clinic)
head(alsfrs_clinic)
length(unique(alsfrs_clinic$subj_id))


# combine all surveys together 
surveys_df_all <- 
  roads_beiwe %>% 
  rbind(alsfrsr_beiwe) %>% 
  rbind(alsfrs_clinic) %>%
  group_by(beiwe_id) %>%
  mutate(
    day_relative = as.numeric(difftime(local_time_date, min(local_time_date), units = c("days"))),
    month_relative = day_relative/30.5
  ) %>%
  ungroup()
  
#checks
dim(surveys_df_all)
head(surveys_df_all)
length(unique(surveys_df_all$subj_id))
table(surveys_df_all$survey_type)
table(surveys_df_all$survey_name)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# run LMM models
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Beiwe ALSFRS

# without wearable indicator
alsfrsr_beiwe_mod_df <- surveys_df_all %>% filter(survey_type == "beiwe", survey_name == "alsfrsr") 
alsfrsr_beiwe_mod <- lmer(answers_score ~ month_relative  + (1 + month_relative | subj_id), data = alsfrsr_beiwe_mod_df, control = lmerControl(optimizer = "bobyqa"))
alsfrsr_beiwe_mod_s <- summary(alsfrsr_beiwe_mod)
alsfrsr_beiwe_mod_sc <- alsfrsr_beiwe_mod_s
print(alsfrsr_beiwe_mod_sc)
merMod_has_converged(alsfrsr_beiwe_mod)

# with wearable indicator
alsfrsr_beiwe_wi_mod_df <- surveys_df_all %>% filter(survey_type == "beiwe", survey_name == "alsfrsr") 
alsfrsr_beiwe_wi_mod <- lmer(answers_score ~ month_relative * wearable + (1 + month_relative | subj_id), data = alsfrsr_beiwe_wi_mod_df, control = lmerControl(optimizer = "bobyqa"))
alsfrsr_beiwe_wi_mod_s <- summary(alsfrsr_beiwe_wi_mod)
alsfrsr_beiwe_wi_mod_sc <- alsfrsr_beiwe_wi_mod_s
print(alsfrsr_beiwe_wi_mod_sc)
merMod_has_converged(alsfrsr_beiwe_wi_mod)


# ------------------------------------------------------------------------------
# Beiwe ROADS

# without wearable indicator
roads_beiwe_mod_df <- surveys_df_all %>% filter(survey_type == "beiwe", survey_name == "roads") 
roads_beiwe_mod <- lmer(answers_score ~ month_relative  + (1 + month_relative | subj_id), data = roads_beiwe_mod_df, control = lmerControl(optimizer = "bobyqa"))
roads_beiwe_mod_s <- summary(roads_beiwe_mod)
roads_beiwe_mod_sc <- roads_beiwe_mod_s
print(roads_beiwe_mod_sc)
merMod_has_converged(roads_beiwe_mod)

# with wearable indicator
roads_beiwe_wi_mod_df <- surveys_df_all %>% filter(survey_type == "beiwe", survey_name == "roads") 
roads_beiwe_wi_mod <- lmer(answers_score ~ month_relative * wearable + (1 + month_relative | subj_id), data = roads_beiwe_wi_mod_df, control = lmerControl(optimizer = "bobyqa"))
roads_beiwe_wi_mod_s <- summary(roads_beiwe_wi_mod)
roads_beiwe_wi_mod_sc <- roads_beiwe_wi_mod_s
print(roads_beiwe_wi_mod_sc)
merMod_has_converged(roads_beiwe_wi_mod)


# ------------------------------------------------------------------------------
# In-clinic ALSFRS

# without wearable indicator
alsfrsr_inclinic_mod_df <- surveys_df_all %>% filter(survey_type == "in-clinic", survey_name == "alsfrsr") 
alsfrsr_inclinic_mod <- lmer(answers_score ~ month_relative  + (1 + month_relative | subj_id), data = alsfrsr_inclinic_mod_df, control = lmerControl(optimizer = "bobyqa"))
alsfrsr_inclinic_mod_s <- summary(alsfrsr_inclinic_mod)
alsfrsr_inclinic_mod_sc <- alsfrsr_inclinic_mod_s$coefficients
print(alsfrsr_inclinic_mod_sc)
merMod_has_converged(alsfrsr_inclinic_mod)


# ------------------------------------------------------------------------------
# Beiwe AND in-clinic ALSFRS

# without wearable indicator
alsfrsr_beiweandinclinic_mod_df <- surveys_df_all %>% filter(survey_name == "alsfrsr") 
alsfrsr_beiweandinclinic_mod <- lmer(answers_score ~ month_relative  * survey_type + (1 + month_relative | subj_id), data = alsfrsr_beiweandinclinic_mod_df, control = lmerControl(optimizer = "bobyqa"))
alsfrsr_beiweandinclinic_mod_s <- summary(alsfrsr_beiweandinclinic_mod)
alsfrsr_beiweandinclinic_mod_sc <- alsfrsr_beiweandinclinic_mod_s$coefficients
print(alsfrsr_beiweandinclinic_mod_sc)
merMod_has_converged(alsfrsr_beiweandinclinic_mod)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Summarize LMM coefficients in a table
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

format_coef <- function(mod){
  mod_r2  <- r.squaredGLMM(mod)
  out_ci_df <- as.data.frame(confint(mod)) %>% rownames_to_column(var = "term") 
  out <- 
    summary(mod)$coef %>% 
    as.data.frame() %>%
    clean_names() %>%
    rownames_to_column(var = "term") %>%
    select(-c(df)) %>% 
    left_join(out_ci_df) %>%
    janitor::clean_names()
  out$r2m <- c(mod_r2[1], rep(NA, nrow(out) - 1))
  out$r2c <- c(mod_r2[2], rep(NA, nrow(out) - 1))
  # this function is from utils.R
  out$conv <- c(merMod_has_converged(mod), rep(NA, nrow(out) - 1)) 
  out <- 
    out %>%
    mutate_if(is.numeric, round, 3)
  out
}


# ------------------------------------------------------------------------------
# generate and append model table summaries 

mod_tbl_all <- data.frame()

# Beiwe ALSFRS 
mod_tbl <- 
  format_coef(alsfrsr_beiwe_mod) %>% 
  mutate(
    model_no = c(1, rep(NA, nrow(.) - 1)), 
    outcome = c("Beiwe ALSFRS-R", rep(NA, nrow(.) - 1)), 
    .before = everything())
mod_tbl_all <- rbind(mod_tbl_all, mod_tbl)


# Beiwe ROADS 
mod_tbl <- 
  format_coef(roads_beiwe_mod) %>% 
  mutate(
    model_no = c(2, rep(NA, nrow(.) - 1)), 
    outcome = c("Beiwe ROADS", rep(NA, nrow(.) - 1)),  
    .before = everything()
)
mod_tbl_all <- rbind(mod_tbl_all, mod_tbl)


# In-clinic ALSFRS 
mod_tbl <- 
  format_coef(alsfrsr_inclinic_mod) %>% 
  mutate(
    model_no = c(3, rep(NA, nrow(.) - 1)), 
    outcome = c("In-clinic ALSFRS-R", rep(NA, nrow(.) - 1)),  
    .before = everything()
  )
mod_tbl_all <- rbind(mod_tbl_all, mod_tbl)


# In-clinic + Beiwe ALSFRS 
mod_tbl <- 
  format_coef(alsfrsr_beiweandinclinic_mod) %>% 
  mutate(
    model_no = c(4, rep(NA, nrow(.) - 1)), 
    outcome = c("(Beiwe, in-clinic) ALSFRS-R", rep(NA, nrow(.) - 1)),  
    .before = everything()
  )
mod_tbl_all <- rbind(mod_tbl_all, mod_tbl)


# Beiwe ALSFRS -- with wearable group indicator
mod_tbl <- 
  format_coef(alsfrsr_beiwe_wi_mod) %>% 
  mutate(
    model_no = c(5, rep(NA, nrow(.) - 1)), 
    outcome = c("Beiwe ALSFRS-R", rep(NA, nrow(.) - 1)),  
    .before = everything())
mod_tbl_all <- rbind(mod_tbl_all, mod_tbl)


# Beiwe ROADS -- with wearable group indicator
mod_tbl <- 
  format_coef(roads_beiwe_wi_mod) %>% 
  mutate(
    model_no = c(6, rep(NA, nrow(.) - 1)), 
    outcome = c("Beiwe ROADS", rep(NA, nrow(.) - 1)),  
    .before = everything()
  )
mod_tbl_all <- rbind(mod_tbl_all, mod_tbl)

# format 
mod_tbl_all_f <-
  mod_tbl_all %>%
  mutate(
    estimate_f = sprintf("%.3f", estimate),
    x2_5_percent_f = sprintf("%.3f", x2_5_percent),
    x97_5_percent_f = sprintf("%.3f", x97_5_percent),
    pr_t_f = sprintf("%.3f", pr_t),
    term_est_f = paste0(estimate_f, " [", x2_5_percent_f, ", ", x97_5_percent_f, "] (", pr_t_f, ")")
  ) %>%
  select(
    model_no, 
    outcome, 
    term,
    term_est_f,
    r2m,   
    r2c
  )
names(mod_tbl_all_f) <- c("No.", "Outcome", "Term", "Est. [95% CI] (p-val.)", "R2m", "R2c")
mod_tbl_all_f

# other formatting
mod_tbl_all_f$Term <- gsub("\\(Intercept\\)", "Intercept", mod_tbl_all_f$Term)
mod_tbl_all_f$Term <- gsub("survey_type", "", mod_tbl_all_f$Term)
mod_tbl_all_f$Term <- gsub("month_relative", "Time [months]", mod_tbl_all_f$Term)
mod_tbl_all_f$Term <- gsub("wearablemodus", "Modus", mod_tbl_all_f$Term)

# ------------------------------------------------------------------------------
# save table summaries

# save to file
out_path <- file.path(here(), 'results_tables', 'manuscript_1', "table_lmm_estimate_survey_over_time.csv")
fwrite(mod_tbl_all_f, out_path); rm(out_path)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Plot lmm fits
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

subj_id_levels <- sort(unique(surveys_df_all$subj_id))
# month_rel_max <- max(surveys_df_all$month_relative)
month_rel_max <- 6.2

wearable_levels <- c("actigraph", "modus")
wearable_labels <- c("ActiGraph", "Modus")

survey_type_levels <- c("beiwe", "in-clinic")
survey_type_labels <- c("Self-entry", "Staff")

plt_y_limits_alsfrs <- c(10, 50)
plt_y_limits_roads <- c(50, 135) 


# -----------------------------------------------------------------------------
# Beiwe ALSFRS -- without wearable indicator

plt_df <- alsfrsr_beiwe_mod_df 
plt_df$mu <- getME(alsfrsr_beiwe_mod, "mu")
plt_df$subj_id_fct <- factor(plt_df$subj_id, levels = subj_id_levels)
plt_df_group <- expand.grid(
    month_relative = c(0, month_rel_max)
  )
plt_df_group$mu <- predict(alsfrsr_beiwe_mod, newdata = plt_df_group, re.form = NA)
plt_x_breaks <- seq(0, month_rel_max, by = 1)
alsfrsr_beiwe_plt <- 
  ggplot() + 
  geom_line(data = plt_df, aes(x = month_relative, y = mu, color = subj_id_fct, group = subj_id_fct), 
            size = 0.3, linetype = 2) +
  geom_point(data = plt_df, aes(x = month_relative, y = answers_score, color = subj_id_fct, group = subj_id_fct),
             size = 0.8) +
  geom_line(data = plt_df_group, aes(x = month_relative, y = mu), 
            size = 1, color = "black", alpha = 0.9) +
  scale_x_continuous(breaks = plt_x_breaks) +
  scale_y_continuous(limits = plt_y_limits_alsfrs) +
  labs(x = "Time [months]", y = "ALSFRS-RSE", linetype = "Group", title = "a") + 
  guides(colour = "none") + 
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = alpha('white', 0.6), color = NA))


# -----------------------------------------------------------------------------
# Beiwe ROADS -- without wearable indicator

plt_df <- roads_beiwe_mod_df 
plt_df$mu <- getME(roads_beiwe_mod, "mu")
plt_df$subj_id_fct <- factor(plt_df$subj_id, levels = subj_id_levels)
plt_df_group <- expand.grid(
  month_relative = c(0, month_rel_max)
)
plt_df_group$mu <- predict(roads_beiwe_mod, newdata = plt_df_group, re.form = NA)
plt_x_breaks <- seq(0, month_rel_max, by = 1)
roads_beiwe_plt <- 
  ggplot() + 
  geom_line(data = plt_df, aes(x = month_relative, y = mu, color = subj_id_fct, group = subj_id_fct), 
            size = 0.3, linetype = 2) +
  geom_point(data = plt_df, aes(x = month_relative, y = answers_score, color = subj_id_fct, group = subj_id_fct),
             size = 0.8) +
  geom_line(data = plt_df_group, aes(x = month_relative, y = mu), 
            size = 1, color = "black", alpha = 0.9) +
  scale_x_continuous(breaks = plt_x_breaks) +
  scale_y_continuous(limits = plt_y_limits_roads) +
  labs(x = "Time [months]", y = "ROADS", linetype = "Group", title = "b") + 
  guides(colour = "none") + 
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = alpha('white', 0.6), color = NA))


# -----------------------------------------------------------------------------
# In-clinic ALSFRS -- without wearable indicator

plt_df <- alsfrsr_inclinic_mod_df
plt_df$mu <- getME(alsfrsr_inclinic_mod, "mu")
plt_df$subj_id_fct <- factor(plt_df$subj_id, levels = subj_id_levels)
plt_df_group <- expand.grid(
  month_relative = c(0, month_rel_max)
)
plt_df_group$mu <- predict(alsfrsr_inclinic_mod, newdata = plt_df_group, re.form = NA)
plt_x_breaks <- seq(0, month_rel_max, by = 1)
alsfrsr_inclinic_plt <- 
  ggplot() + 
  geom_line(data = plt_df, aes(x = month_relative, y = mu, color = subj_id_fct, group = subj_id_fct), 
            size = 0.3, linetype = 2) +
  geom_point(data = plt_df, aes(x = month_relative, y = answers_score, color = subj_id_fct, group = subj_id_fct),
             size = 0.8) +
  geom_line(data = plt_df_group, aes(x = month_relative, y = mu), 
            size = 1, color = "black", alpha = 0.9) +
  scale_x_continuous(breaks = plt_x_breaks) +
  scale_y_continuous(limits = plt_y_limits_alsfrs) +
  labs(x = "Time [months]", y = "ALSFRS-R", linetype = "Group", title = "c") + 
  guides(colour = "none") + 
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = alpha('white', 0.6), color = NA))


# ------------------------------------------------------------------------------
# Beiwe AND in-clinic ALSFRS

# without wearable indicator
plt_df <- alsfrsr_beiweandinclinic_mod_df
plt_df$mu <- getME(alsfrsr_beiweandinclinic_mod, "mu")
plt_df$subj_id_fct <- factor(plt_df$subj_id, levels = subj_id_levels)
plt_df$plt_group = paste0(plt_df$subj_id, "_", plt_df$survey_type)
plt_df_group <- expand.grid(
  survey_type = unique(plt_df$survey_type),
  month_relative = c(0, max(plt_df$month_relative))
)
plt_df_group$mu <- predict(alsfrsr_beiweandinclinic_mod, newdata = plt_df_group, re.form = NA)
plt_df_group$survey_type_fct <- factor(plt_df_group$survey_type, levels = survey_type_levels, labels = survey_type_labels)
plt_df$survey_type_fct <- factor(plt_df$survey_type, levels = survey_type_levels, labels = survey_type_labels)
plt_x_breaks <- seq(0, month_rel_max, by = 1)
alsfrsr_beiweandinclinic_plt <- 
  ggplot() + 
  geom_line(data = plt_df, aes(x = month_relative, y = mu, color = subj_id_fct, group = plt_group, linetype = survey_type_fct), 
            size = 0.3, alpha = 0.6) +
  geom_point(data = plt_df, aes(x = month_relative, y = answers_score, color = subj_id_fct, group = subj_id_fct, shape = survey_type_fct),
             size = 1.3) +
  geom_line(data = plt_df_group, aes(x = month_relative, y = mu, group = survey_type_fct, linetype = survey_type_fct), 
            size = 1, color = "black", alpha = 0.9) +
  scale_x_continuous(breaks = plt_x_breaks) +
  scale_y_continuous(limits = plt_y_limits_alsfrs) +
  labs(x = "Time [months]", y = "ALSFRS-R, ALSFRS-RSE", 
       linetype = "Administration", shape = "Administration", title = "d") + 
  guides(colour = "none") + 
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = alpha('white', 0.6), color = NA))



# -----------------------------------------------------------------------------
# Beiwe ALSFRS -- with wearable indicator

plt_df <- alsfrsr_beiwe_mod_df
plt_df$mu <- getME(alsfrsr_beiwe_wi_mod, "mu")
plt_df$subj_id_fct <- factor(plt_df$subj_id, levels = subj_id_levels)
plt_df_group <- expand.grid(
  wearable = unique(plt_df$wearable),
  month_relative = c(0, max(plt_df$month_relative))
)
plt_df_group$mu <- predict(alsfrsr_beiwe_wi_mod, newdata = plt_df_group, re.form = NA)
plt_df_group$wearable_fct <- factor(plt_df_group$wearable, levels = wearable_levels, labels = wearable_labels)
plt_x_breaks <- seq(0, month_rel_max, by = 1)
alsfrsr_beiwe_wi_plt <- 
  ggplot() + 
  geom_line(data = plt_df, aes(x = month_relative, y = mu, color = subj_id_fct, group = subj_id_fct), 
            size = 0.3, linetype = 2) +
  geom_point(data = plt_df, aes(x = month_relative, y = answers_score, color = subj_id_fct, group = subj_id_fct),
             size = 0.8) +
  geom_line(data = plt_df_group, aes(x = month_relative, y = mu, group = wearable_fct, linetype = wearable_fct), 
            size = 1, color = "black", alpha = 0.9) +
  scale_x_continuous(breaks = plt_x_breaks) +
  scale_y_continuous(limits = plt_y_limits_alsfrs) +
  labs(x = "Time [months]", y = "ALSFRS-RSE", linetype = "Group", title = "a") + 
  guides(colour = "none") + 
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = alpha('white', 0.6), color = NA))


# -----------------------------------------------------------------------------
# Beiwe ROADS -- with wearable indicator

plt_df <- roads_beiwe_mod_df
plt_df$mu <- getME(roads_beiwe_wi_mod, "mu")
plt_df$subj_id_fct <- factor(plt_df$subj_id, levels = subj_id_levels)
plt_df_group <- expand.grid(
  wearable = unique(plt_df$wearable),
  month_relative = c(0, max(plt_df$month_relative))
)
plt_df_group$mu <- predict(roads_beiwe_wi_mod, newdata = plt_df_group, re.form = NA)
plt_df_group$wearable_fct <- factor(plt_df_group$wearable, levels = wearable_levels, labels = wearable_labels)
plt_x_breaks <- seq(0, month_rel_max, by = 1)
roads_beiwe_wi_plt <- 
  ggplot() + 
  geom_line(data = plt_df, aes(x = month_relative, y = mu, color = subj_id_fct, group = subj_id_fct), 
            size = 0.3, linetype = 2) +
  geom_point(data = plt_df, aes(x = month_relative, y = answers_score, color = subj_id_fct, group = subj_id_fct),
             size = 0.8) +
  geom_line(data = plt_df_group, aes(x = month_relative, y = mu, group = wearable_fct, linetype = wearable_fct), 
            size = 1, color = "black", alpha = 0.9) +
  scale_x_continuous(breaks = plt_x_breaks) +
  scale_y_continuous(limits = plt_y_limits_roads) +
  labs(x = "Time [months]", y = "ROADS", linetype = "Group", title = "b") + 
  guides(colour = "none") + 
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = alpha('white', 0.6), color = NA))


# ------------------------------------------------------------------------------
# save plots

# figure 1 
plt_list <- list(
  alsfrsr_beiwe_plt, 
  roads_beiwe_plt,
  alsfrsr_inclinic_plt,
  alsfrsr_beiweandinclinic_plt
  )
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "hv", byrow = TRUE)
plt_fpath <- file.path(here(), "results_figures", "manuscript_1", "lmm_survey_over_time_set1.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 4.5 * 2, base_height = 4.5 * 2, dpi = 150)

# figure 2 
plt_list <- list(
  alsfrsr_beiwe_wi_plt, 
  roads_beiwe_wi_plt
  )
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "hv", byrow = TRUE)
plt_fpath <- file.path(here(), "results_figures", "manuscript_1", "lmm_survey_over_time_set2.jpeg")
# save_plot(filename = plt_fpath, plot = plt, base_width = 3.8 * 3, base_height = 3.8, dpi = 150)
save_plot(filename = plt_fpath, plot = plt, base_width = 4.5 * 2, base_height = 4.5, dpi = 150)


