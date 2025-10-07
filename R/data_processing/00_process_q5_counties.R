################## data processing for Question 5 ##################
#
# This script cleans up survey data for 
#     ---- QUESTION 5 ----
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



# Question 5: Where do you work on kelp-related issues? -------------------

## counties q was number 5
county.df <- dat_survey %>% 
  select(response_id, starts_with("q5"))

## re-format
county.df %<>%
  pivot_longer(cols = -c(response_id), names_to = "tmp", values_to = "county") %>%
  dplyr::select(-tmp)

## remove NAs
county.df %<>% filter(!is.na(county))

## check out counties
county.df %>%
  ggplot(aes(x=county)) +
  geom_bar() +
  theme_bw() + coord_flip()

## clean up a bit: fix spelling and too-specific answer
county.df %<>% 
  mutate(county = ifelse(county == "Del Notre", "Del Norte",county)) %>%
  mutate(county = ifelse(county=='all counties with emphasis on Humboldt and Northern California', '(all counties)', county)) 

## did anyone list out all counties instead of choosing 'all'? if so, replace their answers with (all counties) for consistency
all_counties <- county.df %>% group_by(response_id) %>% summarise(nc=length(unique(county))) %>%
  ungroup() %>%
  filter(nc > 14)
all_counties  # 2 respondents

county.df %<>% filter(!(response_id %in% all_counties$response_id)) %>%
  bind_rows(
    county.df %>% filter(response_id %in% all_counties$response_id) %>%
      dplyr::select(-county) %>% distinct() %>%
      mutate(county='(all counties)')
  )

# Add region --------------------------------------------------------------
county.df %<>%
  mutate(region = case_when(county %in% c("Ventura",
                                          "San Diego",
                                          "Orange",
                                          "Los Angeles") ~ "south",
                            county %in% c("Santa Barbara",
                                          "San Luis Obispo", 
                                          "Monterey",
                                          "Santa Cruz") ~ "central",
                            county %in% c("Sonoma",
                                          "San Mateo",
                                          "San Francisco",
                                          "Mendocino",
                                          "Marin",
                                          "Humboldt",
                                          "Del Norte") ~ "north",
                            county == "(all counties)" ~ "all counties"))

county.df



# explore -----------------------------------------------------------------

## check out counties again
county.df %>%
  ggplot(aes(x=county)) +
  geom_bar() +
  theme_bw() + coord_flip()

## how many counties do individuals work across?
county.df %>%
  filter(county != "(all counties)") %>%
  group_by(response_id) %>% summarise(nc=length(unique(county))) %>%
  bind_rows( 
    county.df %>% filter(county == "(all counties)") %>% mutate(nc=14) %>% dplyr::select(response_id, nc)
    ) %>%
  ggplot(aes(x=nc)) +
  geom_histogram() +
  scale_x_continuous(breaks=seq(1,14,by=1), name='Counties per individual') +
  theme_bw() 

## how many regions do individuals work across?
county.df %>%
  filter(county != "(all counties)") %>%
  group_by(response_id) %>% summarise(nr=length(unique(region))) %>%
  bind_rows( 
    county.df %>% filter(county == "(all counties)") %>% mutate(nr=3) %>% dplyr::select(response_id, nr)
  ) %>%
  ggplot(aes(x=nr)) +
  geom_histogram() +
  scale_x_continuous(breaks=seq(1,14,by=1), name='Regions (out of 3) per individual') +
  theme_bw() 


# save -------------------------------------------------------------------

write_csv(county.df, here('confidential_data','processed','cleaned_responseID_by_county.csv'))
























