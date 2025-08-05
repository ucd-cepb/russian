############### Process survey data on alter organization ############### 
#
# Adjust the names of alter organizations.
#   
#
# For now, this script only looks at collaborative ties -
#  q11: In the past year, which organizations and individual operators have you 
#  **worked directly with** on kelp forest-related projects or programs?
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
source(here('R/subfxn/expand_org_names.R'))
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


# Build Function clean_org_names ------------------------------------------
## create custom R function to adjust the names of organizations.

question_11 %<>% mutate(clean_org_name=clean_org_names(org_name, collab=TRUE))

## make decisions on "NA" answers
View(filter(question_11, is.na(clean_org_name)))

## check answers that didn't change
View(filter(question_11, org_name==clean_org_name))
## to change round 1...
# UCSB (Ospina, J. Smith) +
# SAFE +
# none +
# UCLA; Kyle Cavanuagh +
# UC Davis (Baskett et al) +
# Dandy Fish Company (fish processor) +
# Morgan and Ian (North Coast Reef Check) +
# Opc SeaGrant cdfw + KRMP
# Many many others +
# And many others.. +
# CCCoP
# Greater Farallones Association kelp team (Rietta Hohman) +
# SJSU Moss Landing Marine Labs - Scott Hamiltom +
# Office of Habitat Conservation - Natalie C-Manning and Julia Royster +

question_11 %<>% mutate(clean_org_name=clean_org_names(org_name, collab=TRUE))

## make decisions on "NA" answers. 
View(filter(question_11, is.na(clean_org_name)))

## check answers that didn't change
View(filter(question_11, org_name==clean_org_name))
#The Giant Giant Kelp restoration project
#	Other credible kelp scientists
#kelp farmers
#Ocean Protection Council; Greater Farallones Assoc.


question_11 %<>% mutate(clean_org_name=clean_org_names(org_name, collab=TRUE))

## make decisions on "NA" answers. 
View(filter(question_11, is.na(clean_org_name)))

## check answers that didn't change
View(filter(question_11, org_name==clean_org_name))
#The Giant Giant Kelp restoration project
#	Other credible kelp scientists
#kelp farmers
#Ocean Protection Council; Greater Farallones Assoc.
#	Commercial Divers
# Recreational Divers


# Use Function clean_org_names to Adjust organization names ---------------
question_11 %<>% mutate(clean_org_name=clean_org_names(org_name, collab=TRUE))
## check output
filter(question_11, is.na(clean_org_name))
length(unique(question_11$org_name)); length(unique(question_11$clean_org_name))
sum(grepl("-",question_11$clean_org_name))  # how many have project/group names included?
sum(grepl(":",question_11$clean_org_name))  # how many have individual names included?
sum(grepl(",",question_11$clean_org_name))  # how many do we have to split into multiple rows? *(next section)* -- ten!

# Split multiple orgs -----------------------------------------------------
## where collaboratives were named (e.g., PISCO, DISES) or where someone listed multiple orgs or 
##   individuals in the same response, the clean_org_names function will separate the 
##   separate org entries by a comma

q11 <- question_11 %>%
  select(-org_name) %>% rename(org_name=clean_org_name)

q11 %<>% separate(org_name, into=c('org_name','org.2','org.3','org.4','org.5','org.6','org.7','org.8','org.9','org.10'),sep=",")

## check cases where we have separate org entries
View(q11 %>% filter(!is.na(org.2)))


## make new rows for each of these. carry over the org level. 
q11 %<>% filter(is.na(org.2)) %>%
  select(-starts_with('org.')) %>%
  bind_rows(
    q11 %>% filter(!is.na(org.2)) %>%
      pivot_longer(c('org_name',starts_with('org.')), names_to='tmp', values_to='tmp_org_name') %>%
      mutate(org_name=ifelse(!is.na(tmp_org_name), tmp_org_name,NA)) %>%
      select(-tmp,-tmp_org_name) %>% filter(!is.na(org_name))
  )


## remove NAs (from running the clean_org_names function)
q11 %<>% filter(!is.na(org_name))



# Save --------------------------------------------------------------------

## rename column headers
q11 %<>% rename(type=org_level)
q11 %<>% rename(alter=org_name)

## confidential version
q11 %>% 
  write_csv(here('confidential_data','processed',paste0('processed_by_responseID_q11_collabs_4sen_',Sys.Date(),'.csv')))

## non-confidential version
q11 %>% 
  select(response_id,type,alter) %>%
  write_csv(here('data','sen',paste0('processed_by_responseID_q11_collabs_4sen_',Sys.Date(),'.csv')))







