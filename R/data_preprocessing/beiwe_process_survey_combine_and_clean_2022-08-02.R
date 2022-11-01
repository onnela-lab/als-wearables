
#' @description 
#' This script combines surveys from "survey_answers" and "survey_timings" 
#' Beiwe data streams (after their initial preprocessing done earlier
#' in other scripts).
#' Then some preprocessing cleaning is done to assure only one final answer 
#' per survey question is kept for participant for a day. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library(here)
options(digits.secs = 3)


# ------------------------------------------------------------------------------
# READ DATA
# ------------------------------------------------------------------------------

answers_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_survey_answers.rds")
answers <- readRDS(answers_path)

timings_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_survey_timings.rds")
timings <- readRDS(timings_path)

# check these two data frames have the same column names
names(answers) == names(timings)

# read mapping for survey names
survey_id_map_path <- file.path(here(), "data_participants_other", "subject ids.xlsx")
survey_id_map <- read_excel(survey_id_map_path, sheet = "survey ID-name map")

# read pre-defined map of ALSFRS-R answer text and ALSFRS-R answer value
ALSFRSR_answer_map_path <- file.path(here(), "doc", "Surveys", "ALSFRSR_answer_value_map.csv")
ALSFRSR_answer_map <- read_csv(ALSFRSR_answer_map_path)

# read Beiwe_ID to get whether HC or ALS
Beiwe_IDs_Master_path <- file.path(here(), "data_participants_other", "@Beiwe_IDs_Master.csv")
Beiwe_IDs_Master <- 
  fread(Beiwe_IDs_Master_path) %>% 
  as.data.frame() %>% 
  janitor::clean_names() 

# read file with mapping beiwe_id ~ subj_id
start_end_dates_fname = "smart_wearables_start_end_dates_clean.csv"
start_end_dates_path = file.path(here(), "data_participants_other_processed", start_end_dates_fname)
start_end_dates = 
  fread(start_end_dates_path) %>% 
  as.data.frame() %>% 
  janitor::clean_names() 


# ------------------------------------------------------------------------------
# get final answers
# ------------------------------------------------------------------------------

#' From Zachary Clement: 
#' (1) if their question type looks like this, it was made on an Android device. 
#'     Radio button questions are the only ones with the integer instead of text
#'     (Also, avoiding any possible text outputs)
#' (2) if a semicolon appears in an answer choice option,
#'     our regexp sub/split operation would think there are
#'     way more answers than there really are. 
#'     We will pull from the responses from iPhone users and switch semicolons 
#'     within an answer to commas. 
#' (3) Android users have "; " seperating answer choices. iPhone users have ";" separating choices.

# get all answer choices text options now that Android and iPhone have the same ones 
radio_answer_choices_list <- 
  answers %>%
  as.data.frame() %>%
  filter(question_type == "radio_button") %>%
  select(question_id, question_answer_options) %>%
  distinct() %>%
  # there is one case where for one question_id there are two question_answer_options,
  # one of which is with some random stuff inside 
  mutate(qao_nchar = nchar(question_answer_options)) %>%
  group_by(question_id) %>%
  arrange(qao_nchar) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-qao_nchar) %>%
  as.data.frame()
  
# function to preprocess answers 
process_answer <- function(question_type, question_id, answer){
  # if their question type looks like this, it was made on an Android device
  question_type_android <- "Radio Button Question"
  answer_android <- as.character(0 : 100)
  if (!(question_type == question_type_android & answer %in% answer_android)){
    return(answer)
  } else {
    # preprocess question answer options
    # get question_answer_options from iPhone answers list
    qao_vec <- radio_answer_choices_list[radio_answer_choices_list$question_id == question_id, "question_answer_options"]
    # remove square brackets at the beginning and at the end
    qao_vec <- gsub("^\\[|\\]$", "", qao_vec)
    # replace " ;" with ";" in question_answer_options (rare cases)
    qao_vec <- gsub(" ;", ";", qao_vec)
    # split by semicolon immediately followed by another string
    qao_vec <- strsplit(qao_vec, ";(?!\\s)", perl = TRUE)[[1]]
    # note the answers values start at 0
    qao_vec_idx <- as.numeric(answer) + 1 
    out <- qao_vec[qao_vec_idx]
    return(out)
  }
}

answers_F <-
  answers %>%
  mutate(local_time_date = as.Date(local_time)) %>% 
  group_by(beiwe_id, local_time_date, survey_id, question_id) %>%
  # choose the last answer for a given question_id (if multiple present)
  arrange(dat_idx, dat_row_idx) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(
    -dat_idx,
    -dat_row_idx
  ) %>% 
  rowwise() %>%
  mutate(answer = process_answer(question_type, question_id, answer)) %>%
  ungroup()

timings_F <- 
  timings %>%
  mutate(local_time_date = as.Date(local_time)) %>% 
  group_by(beiwe_id, local_time_date, survey_id, question_id) %>%
  # choose the last answer for a given question_id (if multiple present)
  arrange(dat_idx, dat_row_idx) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(
    -dat_idx,
    -dat_row_idx
  )

# check if all column names agree
all(names(timings_F) == names(answers_F))

# combine the data 
df_rbind <- 
  rbind(answers_F, timings_F) %>%
  group_by(beiwe_id, local_time_date, survey_id, question_id) %>%
  mutate(cnt = n_distinct(answer)) 

# define subset where either 
# (a) no duplicates from answers and timings
# (b) duplicates present, but they do agree in answer (pick up one only then)
df_rbind_1 <- 
  df_rbind %>% 
  filter(cnt == 1) %>%
  filter(row_number() == 1) %>%
  ungroup()

# define subset where duplicates present and answers do not answer
df_rbind_2 <- 
  df_rbind %>% 
  filter(cnt > 1) %>%
  mutate(
    is_valid = ifelse(is.na(answer), 0, 1),
    is_valid = ifelse(answer %in% c("", "NO_ANSWER_SELECTED", "NOT_PRESENTED"), 0, is_valid)
  ) %>%
  arrange(is_valid, utc_time) %>%
  filter(row_number() == n())  %>%
  ungroup() %>%
  select(-is_valid)

# combine
dat <- 
  rbind(df_rbind_1, df_rbind_2) %>%
  select(- cnt)


# ------------------------------------------------------------------------------
# STEP: clean "question_text"
# ------------------------------------------------------------------------------

sngl_quot_rx = "[ʻʼʽ٬‘’‚‛՚︐]"
# dbl_quot_rx = "[«»““”„‟≪≫《》〝〞〟\＂″‶]"

dat$question_text <- sapply(dat$question_text, function(val){
  val <- gsub(pattern = ":\\s+", ": ", val)
  val <- gsub(sngl_quot_rx, "'", `Encoding<-`(val, "UTF8"))
  val
})

# CHECK ------------------------------------------------------------------------
# check if there are multiple distinct question_text within question_id
dat_tmp <- 
  dat %>% 
  group_by(question_id) %>%
  summarise(cnt_distinct = n_distinct(question_text)) %>%
  ungroup() %>%
  filter(cnt_distinct > 1) %>%
  arrange(desc(cnt_distinct)) 
dat_tmp_agg <- 
  dat %>% 
  inner_join(dat_tmp, by = "question_id") %>%
  group_by(survey_id, question_id, question_text) %>%
  summarise(cnt = n()) %>%
  inner_join(survey_id_map, by = "survey_id") %>% 
  ungroup()  %>%
  select(survey_name, question_id, question_text, cnt) %>% 
  arrange(desc(question_id))
# dat_tmp_agg
vec_tmp <- dat_tmp_agg %>% pull(question_text)
names(vec_tmp) <- NULL
vec_tmp
# [1] "Did a health professional ever tell you that you might have ALS or Lou Gehrig's disease?"                                                                            
# [2] "Did a health professional ever tell you that you might have PLS or ALS (Lou Gehrig's disease)?"                                                                      
# [3] "(Only answer if you do have a feeding tube/G-tube and you use it more than 50% of the time) Cutting food and handling utensils"                                      
# [4] "Cutting food and handling utensils/g-tube"                                                                                                                           
# [5] "(Only answer if you do NOT have a feeding tube/G-tube or if you do have a feeding tube but you use it less than 50% of the time)  Cutting food and handling utensils"
# [6] "Cutting food and handling utensils"      


# ------------------------------------------------------------------------------
# STEP: clean "question_answer_options"
# ------------------------------------------------------------------------------

dat$question_answer_options <- sapply(dat$question_answer_options, function(val){
  val <- gsub(sngl_quot_rx, "'", `Encoding<-`(val, "UTF8"))
  val <- gsub(pattern = ";\\s+", ";", val)
  val <- gsub(pattern = "None;3 Some", "None;Some", val)
  # val <- gsub(pattern = "n’t", "n't", val)
  val
})

# CHECK ------------------------------------------------------------------------
dat %>% 
  select(question_id, question_text, question_answer_options) %>%
  distinct() %>%
  group_by(question_id, question_text) %>%
  filter(n() > 1) %>%
  ungroup()

# ------------------------------------------------------------------------------
# STEP: clean "answer"
# ------------------------------------------------------------------------------

dat$answer <- sapply(dat$answer, function(val){
  val <- gsub(sngl_quot_rx, "'", `Encoding<-`(val, "UTF8"))
  val <- gsub(pattern = "3 Some", "Some", val)
  val
})


# ------------------------------------------------------------------------------
# STEP: add "survey_name" 
# ------------------------------------------------------------------------------

dat <- 
  dat %>% 
  inner_join(survey_id_map, by = "survey_id")


# -----------------------------------------------------------------------------
# STEP: remove invalid answers
# ------------------------------------------------------------------------------

dat <- 
  dat %>% 
  filter(!(answer %in% c("", "NO_ANSWER_SELECTED", "NOT_PRESENTED")))


# ------------------------------------------------------------------------------
# STEP: reorder columns
# ------------------------------------------------------------------------------

names(dat) 
length(names(dat))
dat <- 
  dat %>%
  select(
    beiwe_id,
    # timestamp, 
    utc_time,
    local_time,
    survey_id,
    survey_name,
    question_id,
    everything()
  ) 


# ------------------------------------------------------------------------------
# STEP: arrange
# ------------------------------------------------------------------------------

dat <- 
  dat %>%
  arrange(
    beiwe_id,
    local_time
  )


# ------------------------------------------------------------------------------
# STEP: filter out healthy controls
# ------------------------------------------------------------------------------

Beiwe_IDs_Master_path <- file.path(here(), "data_participants_other", "@Beiwe_IDs_Master.csv")
Beiwe_IDs_Master <- 
  fread(Beiwe_IDs_Master_path) %>% 
  as.data.frame() %>% 
  janitor::clean_names() 
HC_beiwe_id <- 
  Beiwe_IDs_Master %>%
  filter(wearables != "no", als_hc_pls == "HC") %>%
  pull(beiwe_id)

dat <- dat %>% filter(!(beiwe_id %in% HC_beiwe_id))


# ------------------------------------------------------------------------------
# STEP: add subj_id
# ------------------------------------------------------------------------------

subj_id_beiwe_id_map <- 
  start_end_dates %>%
  select(beiwe_id, subj_id)

dat <- 
  dat %>% 
  left_join(subj_id_beiwe_id_map)  %>% select(
  subj_id,
  beiwe_id,
  everything()
)

# TODO CHECK !!
# both below should be TRUE
!any(is.na(dat$subj_id))
!any(is.na(dat$beiwe_id))


# ------------------------------------------------------------------------------
# save processed data 
# ------------------------------------------------------------------------------

# save processed data 
dat_path <- file.path(here(), "data_beiwe_processed", "surveys", "survey_data_finalansweronly.csv")
fwrite(dat, dat_path)


