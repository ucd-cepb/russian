################## data processing for Question 4 ##################
#
# This script cleans up survey data for 
#     ---- QUESTION 4 ----
#     "Which words or phrases best describe your involvement in kelp forest-related issues?
#      (please check all that apply)?"
#    It then saves question-specific data frames where
#    cleaned up answers to questions are saved with
#    the response ID.
#
# Date written: 9/25/2025
# author: Gabby Yang, compiled by Mary Fisher
#
#####################################################################



# set up  -----------------------------------------------------------------

library(readr)
library(tidyverse)
library(magrittr)
library(janitor)
library(here)

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



# Question 4: How are you involved in kelp-related issues? -------------------

## info source q was number 9
act.df <- dat_survey %>% 
  dplyr::select(response_id, starts_with("q4")) %>%
  filter(!if_all(starts_with("q4"), is.na))

## reformat and remove NAs
act.df %<>% 
  pivot_longer(cols = -c(response_id), names_to = "tmp", values_to = "activity") %>%
  dplyr::select(-tmp) %>% filter(!is.na(activity))

## clean up "Other:" as an extra row
act.df %<>% filter(activity!='Other:')

## create a list of answers given to respondents:
opts <- c('Business operations','Recreation or tourism','Student','Planning or permitting','Research','Harvesting or fishing or growing','People management','Advocacy or outreach','Environmental management')

## take a look
act.df %>%
  group_by(activity) %>% summarise(n_respondents=length(unique(response_id))) %>%
  ggplot(aes(x=n_respondents, y=fct_reorder(activity, n_respondents))) +
  geom_col() + labs(y="Which words or phrases best describe\n your involvement in kelp forest-related issues?") +
  theme_bw()

## add categories for respondents based on "other" responses (mary translated response IDs from gabby into activities)
act.df %<>% bind_rows(
  filter(act.df, activity %in% c('RC volunteer Diver','Volunteer','purple urchin removal', 'all sizes, Caspar, Calif.','Urchin Culling',
                      'Science-to-policy; policy development','ex situ preservation of kelp gametophytes (biobanking)',
                      'Volunteering in kelp restoration','kelp forest surveys, culling urchins, outreach docent')) %>%
    mutate(activity='Environmental management'),
  
  filter(act.df, activity %in% c('Informing management and policy','Science-to-policy; policy development')) %>%
    mutate(activity= "People management"),
  
  filter(act.df, activity %in% c('Citizen science','Surveys as citizen  scientist','Data collection','Involvement as related to sea otter research')) %>%
    mutate(activity= "Research"),
  
  filter(act.df, activity %in% c('Donations','Teaching','supporter')) %>%
    mutate(activity= "Advocacy or outreach"),  
  
  filter(act.df, activity %in% c('Film, photography, media','Media - film')) %>%
    mutate(activity= "Recreation or tourism")
    )


# explore -----------------------------------------------------------------

## check out activities again, but without 'other' responses
act.df %>% filter(activity %in% opts) %>%
  group_by(activity) %>% summarise(n_respondents=length(unique(response_id))) %>%
  ggplot(aes(x=n_respondents, y=fct_reorder(activity, n_respondents))) +
  geom_col() + labs(y="Which words or phrases best describe\n your involvement in kelp forest-related issues?") +
  theme_bw()


## how many answers did individuals select?
act.df %>% filter(activity %in% opts) %>%
  group_by(response_id) %>% summarise(n_sources=length(unique(activity))) %>%
  ggplot(aes(x=n_sources)) +
  geom_bar() + labs(y='n_respondents', caption="Which words or phrases best describe\n your involvement in kelp forest-related issues?") +
  theme_bw()


# save -------------------------------------------------------------------

## with 'other' responses
write_csv(act.df, here('confidential_data','processed','cleaned_responseID_by_activity_q4.csv'))

## without 'other' responses
act.df %>% filter(activity %in% opts) %>%
write_csv(here('confidential_data','processed','cleaned_responseID_by_activity_q4_noOther.csv'))























