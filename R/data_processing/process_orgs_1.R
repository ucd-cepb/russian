############### Process survey data on ego organization ############### 
#
# Adjust survey responses to the question:  in survey data frame for richer info on 
#   which organizations survey respondents work for. This includes (1)
#   adding organizations for respondents who answered q2 as "Yes, 
#   I am involved on behalf of several organizations or groups,"
#   and (2) processing organization names for individuals who did not 
#   provide alters (and therefore were excluded from the first pass of)
#   processing social network data from the survey).
#
# For now, this script only completes this process for individuals
#   who responded to QUESTION 9 (info source) by saying that they 
#   directly observed kelp. 
#
# Mary Fisher
#
#######################################################################


# Set up  -----------------------------------------------------------------

library(readr)
library(tidyverse) 
library(dplyr) 
library(tidyr) 
library(here)
library(magrittr)
library(janitor)
#
source('R/subfxn/clean_org_names.R')

# Data --------------------------------------------------------------------

## this is the survey data
dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
  clean_names() %>%
  slice(-c(1:2))

## these are the questions with org info
question_3 <- dat_survey %>%
  dplyr::select(response_id, recipient_last_name, recipient_first_name, email, starts_with('q3')) %>% 
  pivot_longer(starts_with('q3'), values_to='org_name',names_to='org_level')


# Add missing -------------------------------------------------------------
## manually add in orgs
## from Gabby's script whatisgoingon_summary.R
##    section # Question 4.1 Heatmap.  on 7/3/2025; Gabby's
##    script version from  Jun 4, 2025 russian/commit/7d19a0ff2edb2352d5ee726d74138ae2f0a0f4a6
question_3 %<>%
  mutate(org_name = case_when(
    ## from Gabby whatisgoingon_summary.R Feb 2025
    response_id == "R_7zc4m6dh9wc0YdK" & org_level=='q3_individual_1' ~ "California State Polytechnic University Humboldt",
    response_id == "R_1LLVDCfsYqnIq9H" & org_level=='q3_individual_1'  ~ "Get Inspired",
    response_id == "R_7JW5UqsrkFfRVRL" & org_level=='q3_individual_1'  ~ "California State University Long Beach",
    response_id == "R_7N1Uusw5TPiIBZ4" & org_level=='q3_individual_1'  ~ "Reef Check",
    response_id == "R_5dnbJSY7qhuMdsB" & org_level=='q3_individual_1'  ~ "University of California Santa Barbara",
    response_id == "R_38BizK7TnctWB0Z" & org_level=='q3_individual_1'  ~ "Moss Landing Marine Laboratories",
    response_id == "R_6U4QGgoI5tIAT6r" & org_level=='q3_individual_1'  ~ "Reef Check",
    response_id == "R_7DeczPHJmSjx6Tj" & org_level=='q3_individual_1'  ~ "Reef Check",
    response_id == "R_1NqY8R9wV0B745S" & org_level=='q3_individual_1'  ~ "Strategic Earth Consulting",
    response_id == "R_1OwaKPJzCpBm5QR" & org_level=='q3_individual_1'  ~ "California State Polytechnic University Humboldt",
    response_id == "R_7oHQgfbxkVtFRfP" & org_level=='q3_individual_1'  ~ "City College of San Francisco",
    response_id == "R_3jTauwChLV63T1f" & org_level=='q3_individual_1'  ~ "Giant Giant Kelp Restoration Project",
    response_id == "R_5p0hFxU61rtshee" & org_level=='q3_individual_1'  ~ "Giant Giant Kelp Restoration Project",
    response_id == "R_5DbUjzy5UEB6Bmv" & org_level=='q3_individual_1'  ~ "Giant Giant Kelp Restoration Project",
    response_id == "R_1A45wn3R1qfgQo1" & org_level=='q3_individual_1'  ~ "Individual",
    response_id == "R_1DIA6BHO7pKYU8x" & org_level=='q3_individual_1'  ~ "Individual",
    response_id == "R_3REDy37W41gd9F7" & org_level=='q3_individual_1'  ~ "Individual",
    ## added by Mary on 7/3/2025. note that some of these didn't provide alters, so they won't be in the social network.
    response_id == "R_5pzEeA0Y9QJuUVY" & org_level=='q3_individual_1'  ~ "University of California Davis - Bodega Marine Laboratory",
    response_id == "R_1ulIyefXrmNqx1f" & org_level=='q3_individual_1'  ~ "California Sea Grant",
    response_id == "R_3cBAzkvBbrEzWDq" & org_level=='q3_individual_1'  ~ "California Fish and Game Commission",
    response_id == "R_7ekodqIQhL8kW9Y" & org_level=='q3_individual_1'  ~ "California State Polytechnic University Humboldt",
    response_id == "R_5crCJOcIX1ZVu5X" & org_level=='q3_individual_1'  ~ "University of California San Diego - Scripps Institution of Oceanography",
    response_id == "R_7uHWBgcPD0hY0pj" & org_level=='q3_individual_1'  ~ "California Ocean Protection Council",  # finished=False
    response_id == "R_3KxOe3DfFTAjUg0" & org_level=='q3_individual_1'  ~ "California Department of Fish and Wildlife", # finished=False
    response_id == "R_1giJDgseUTPJq9R" & org_level=='q3_individual_1'  ~ "Comunidad y Biodiversidad",
    response_id == "R_6D5kmxtODIPb1hT" & org_level=='q3_individual_1'  ~ "University of California Santa Cruz", # finished=False
    response_id == "R_7OdkuIOsnSx3lGH" & org_level=='q3_individual_1'  ~ "University of California Merced", # finished=False
    response_id %in% c("R_6mLbnsU7eGt3zQ7","R_5OluY6cgVytnMXy") & org_level=='q3_individual_1'  ~ "California Department of Fish and Wildlife", # finished=False
    response_id == "R_3P6kxzidTnvjaEb" & org_level=='q3_individual_1'  ~ "Ocean Science Trust", # finished=False
    response_id == "R_6LMd7dTdf6Rfhim" & org_level=='q3_several_1'  ~ "Commercial Fishermen of Santa Barbara",
    response_id == "R_6LMd7dTdf6Rfhim" & org_level=='q3_several_2'  ~ "The Good Captain Co.",
    response_id == "R_1oSFZfE9boXZM7N" & org_level=='q3_individual_1'  ~ " Universidad Autónoma de Baja California", # finished=False
    TRUE ~ org_name))


## get rid of NA rows.
question_3 %<>% filter(!is.na(org_name) & org_name != 'x')


## who did we lose? use this to iteritively adjust code above.
lost_responses <- anti_join(dplyr::select(dat_survey, response_id, recipient_last_name, recipient_first_name, email,recipient_email,finished, 
                                          starts_with('q3'), starts_with('q11')), question_3) %>%
  filter(is.na(recipient_last_name) | !(recipient_last_name %in% question_3$recipient_last_name))



# Adjust organization names -----------------------------------------------

## use custom R function to adjust the names of organizations.










