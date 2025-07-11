############### Process survey data on alter organization ############### 
#
# Adjust the names of alter organizations.
#   
#
# For now, this script only completes this process for individuals
#   who responded to QUESTION 9 (info source) by saying that they 
#   directly observed kelp. *AND* only looks at collaborative ties.
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
source(here('R/subfxn/clean_org_names.R'))
#

# Data --------------------------------------------------------------------

## this is the survey data
dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
  clean_names() %>%
  slice(-c(1:2))

##   this is the cleaned up data set on the answer to the question: What are the main ways you learn about kelp forest-related issues?
info <- read_csv(here('confidential_data', 'processed','cleaned_responseID_by_info_source_q9.csv'))

## these are the questions with collaborator org info
question_11 <- dat_survey %>%
  dplyr::select(response_id, recipient_last_name, recipient_first_name, email, starts_with('q11')) %>% 
  pivot_longer(starts_with('q11'), values_to='org_name',names_to='org_level')
question_11 %<>% filter(!is.na(org_name))


# Adjust organization names -----------------------------------------------

## use custom R function to adjust the names of organizations.

question_11 %<>% mutate(clean_org_name=clean_org_names(org_name, collab=TRUE))

## make decisions on "NA" answers
View(filter(question_11, is.na(clean_org_name)))

## check answers that didn't change
View(filter(question_11, org_name==clean_org_name))
## to change round 1...
# UCSB (Ospina, J. Smith)
# SAFE
# none
# UCLA; Kyle Cavanuagh
# 	
# UC Davis (Baskett et al)
# Dandy Fish Company (fish processor)
# Morgan and Ian (North Coast Reef Check)
# Opc SeaGrant cdfw + KRMP
# Many many others
# And many others..
# Trinidad Rancheria
# CCCoP
# Greater Farallones Association kelp team (Rietta Hohman)
# SJSU Moss Landing Marine Labs - Scott Hamiltom
# Office of Habitat Conservation - Natalie C-Manning and Julia Royster
