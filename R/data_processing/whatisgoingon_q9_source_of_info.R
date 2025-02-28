################## data processing for Question 9 ##################
#
# This script cleans up survey data for 
#     ---- QUESTION 9 ----
#     "What are the main ways you learn about kelp forest-related issues?"
#    it combines code from 'whatisgoingon_summary' and 
#    'explore_survey_dat_SENqs' so that processing is 
#    consistent for Mary's SEN analysis / Gabby's SNA.
#    It then saves question-specific data frames where
#    cleaned up answers to questions are saved with
#    the response ID.
#
# Date written: 2/28/2025
# author: Gabby Yang, compiled by Mary Fisher
#
#####################################################################



# set up  -----------------------------------------------------------------

library(readr)
library(tidyverse)
library(magrittr)

# survey data -------------------------------------------------------------

dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
  clean_names() %>%
  slice(-c(1:2))

colnames(dat_survey)

## remove anyone who said they are not involved in kelp-related issues
question_1_to_rmv <- dat_survey %>% 
  select(response_id, q1) %>%
  filter(q1 == "I have no involvement in kelp forest-related issues") %>% 
  pull(response_id)

length(question_1_to_rmv) # 23
length(question_1_to_rmv)/length(dat_survey$response_id) # 9.5%

dat_survey %<>% filter(!(response_id %in% question_1_to_rmv))

## remove anyone who works outside of california
question_5_to_rmv <- dat_survey %>% 
  select(response_id, starts_with("q5")) %>%
  pivot_longer(cols = -c(response_id), names_to = "tmp", values_to = "county") %>%
  dplyr::select(-tmp) %>%
  filter(county %in% c("Baja California","Ensenada, Mexico","Other","Other:","none")) %>%
  pull(response_id)

length(question_5_to_rmv) # 3
length(question_5_to_rmv)/length(dat_survey$response_id) # 3%

dat_survey %<>% filter(!(response_id %in% question_5_to_rmv))



# Question 9: How do you learn about kelp-related issues? -------------------

## info source q was number 9
info.df <- dat_survey %>% 
  dplyr::select(response_id, starts_with("q9")) %>%
  filter(!if_all(starts_with("q9"), is.na))

## reformat and remove NAs
info.df %<>% 
  pivot_longer(cols = -c(response_id), names_to = "tmp", values_to = "info_type") %>%
  dplyr::select(-tmp) %>% filter(!is.na(info_type))

## take a look
info.df %>%
  group_by(info_type) %>% summarise(n_respondents=length(unique(response_id))) %>%
  ggplot(aes(x=n_respondents, y=fct_reorder(info_type, n_respondents))) +
  geom_col() + labs(y="What are the main ways \nyou learn about kelp forest-related issues?") +
  theme_bw()

## some clean up of 'other' responses
info.df %<>%
  mutate(info_type = case_when(
    info_type== "50 years experience, air lift opperator, active now" ~ "I directly observe conditions",
    info_type == "I actively do kelp restoration" ~ "I directly observe conditions",
    info_type == "Research assistant" ~ "I collect or analyze data",
    TRUE ~ info_type)) %>%
  filter(!grepl("I wish there were", info_type)) %>%
  filter(info_type != "Other:")

## 'other' responses that could mean two info sources
info.df %<>%
  filter(info_type != "I study my restoration projects") %>%
  bind_rows(
    data.frame(response_id= rep(filter(info.df, info_type == "I study my restoration projects")$response_id,2),
               info_type=c("I collect or analyze data","I directly observe conditions"))
  ) %>%
  filter(info_type != "Kelp restoration groups") %>%
  bind_rows(
    data.frame(response_id= rep(filter(info.df, info_type == "Kelp restoration groups")$response_id,2),
               info_type=c("I talk to managers or regulators","I talk to scientists"))
  )

# explore -----------------------------------------------------------------

## check out info sources again
info.df %>%
  group_by(info_type) %>% summarise(n_respondents=length(unique(response_id))) %>%
  ggplot(aes(x=n_respondents, y=fct_reorder(info_type, n_respondents))) +
  geom_col() + labs(y="What are the main ways \nyou learn about kelp forest-related issues?") +
  theme_bw()

## how many answers did individuals select?
info.df %>%
  group_by(response_id) %>% summarise(n_sources=length(unique(info_type))) %>%
  ggplot(aes(x=n_sources)) +
  geom_bar() + labs(y='n_respondents', caption="What are the main ways \nyou learn about kelp forest-related issues?") +
  theme_bw()


# save -------------------------------------------------------------------

write_csv(info.df, here('confidential_data','processed','cleaned_responseID_by_info_source_q9.csv'))
























