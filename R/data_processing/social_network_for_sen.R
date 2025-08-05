############### Create social network for kelp SEN ############### 
#
# Take the output from the script `process_orgs_3.R` and make sure 
#    that organization names cross-ref each other, between 
#    egos and alters. 
# Save out an edgelist and an adjacency matrix for the kelp SEN.
# The first pass looks only at alters for those individuals who are
#    counted as "systematic direct observers" in the kelp SEN (social
#    layer 1)
#
#
# 7/23/2025 - Mary Fisher
# Last edited: 7/23/2025
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



# Data --------------------------------------------------------------------

## output from script 3: ego and alter orgs processed
dat <- read_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_updateINDupdateONE_4sen_2025-08-04.csv')))

## survey data - grab just response IDs, names, and emails
survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv')) %>%
  clean_names() %>% 
  slice(-c(1:2))

snames <- survey %>% select(response_id, recipient_first_name, recipient_last_name, email)

##  [[  save out surveys that have emails but no name  ]]
# snames %>%
#   filter(!is.na(email) & is.na(recipient_first_name)) %>%
#   left_join(survey %>% select(response_id,email,starts_with('q3'))) %>%
#   write_csv(here('confidential_data','raw','survey_recipient_emails_missing_names.csv'))

# survey %>% select(response_id,recipient_first_name,recipient_last_name,email,starts_with('q3')) %>%
#   write_csv(here('confidential_data','raw','survey_recipient_emails_names.csv'))

## bind rows with names added based on emails.
snames %<>% 
  filter((!is.na(recipient_first_name) & !is.na(email)) | is.na(email)) %>%
  bind_rows(read_csv(here('confidential_data','raw','survey_recipient_emails_missing_names_KEY.csv')))
snames %<>% filter(!is.na(recipient_first_name)) %>% select(response_id,recipient_first_name,recipient_last_name)

##  [[  save out all alters  ]]
# dat %>% select(alter) %>% distinct() %>%
#   write_csv(here('data','sen','alter_list.csv'))



# First pass: only respondents 'on the water' -----------------------------
ndat <- read_csv(here('../','california-kelp-SEN','data','kelp_sites','kelp_sites_confidential','final_assignments','areas_by_responseID_DataRes_STRICT_simplified.csv'))
sen_ids <- ndat %>% select(response_id) %>% distinct() %>% pull(response_id)
length(unique(sen_ids)) #113

dat %<>% filter(response_id %in% sen_ids)

# Create survey respondent ID key -----------------------------------------

## check for duplicate name - id matches. grab the id for the survey that the individual *finished*
dup <- snames %>% group_by(recipient_first_name, recipient_last_name) %>% summarise(n=length(unique(response_id))) %>% filter(n>1)
dup %<>% filter(!is.na(recipient_first_name)) %>%
  left_join(select(snames, recipient_first_name, recipient_last_name, response_id)) %>%
  left_join(survey %>% select(response_id,finished,starts_with('q3'), starts_with('q11')))
to_rmv <- dup %>% filter(finished=='False' & response_id!='R_6mLbnsU7eGt3zQ7')

snames %<>% mutate(recipient_last_name=ifelse(recipient_last_name=='Pomerory', 'Pomeroy', recipient_last_name))
snames %<>% filter(!(response_id %in% to_rmv$response_id))



# Social ties between survey respondents: Find ----------------------------

## break names out of alters column
datl <- dat %>% separate(alter, into=c('alter_org','alter_ind'), sep=':', remove=FALSE)
alti <- datl %>% filter(!is.na(alter_ind))
dim(datl) ; dim(alti)

alti %<>% select(response_id,alter,alter_org,alter_ind) %>% 
  mutate(alter_ind=str_trim(alter_ind)) %>%
  separate(alter_ind, into=c('alter_first_name','alter_last_name'), sep=' ')

## match!  (with all lowercase to avoid capitalization differences) 
alti %<>% 
  mutate_at(c('alter_first_name','alter_last_name'), str_to_lower) %>%
  left_join(snames %>%
              mutate_at(c('recipient_first_name','recipient_last_name'), str_to_lower) %>%
              rename(alter_id=response_id), by=c('alter_first_name'='recipient_first_name','alter_last_name'='recipient_last_name'))

## revisit match: hyphenated / composite last names, shortened first names
alti2 <- alti %>% filter(alter_last_name %in% c('murphy-cannella','giraldo') |
                           alter_first_name %in% c('josh','matthew','dan'))
alti2 %<>% mutate(alter_last_name=case_when(
  alter_last_name=='murphy-cannella' ~ 'murphy',
  alter_last_name=='giraldo' ~ 'giraldo ospina',
  .default=alter_last_name
)) %>%
  mutate(alter_first_name=case_when(
    alter_first_name=='josh' ~ 'joshua',
    alter_first_name=='matthew' ~ 'matt',
    alter_first_name=='dan' ~ 'daniel',
    .default=alter_first_name
  )) %>%
  dplyr::select(-alter_id) %>%
  left_join(snames %>%
              mutate_at(c('recipient_first_name','recipient_last_name'), str_to_lower) %>%
              rename(alter_id=response_id), by=c('alter_first_name'='recipient_first_name','alter_last_name'='recipient_last_name'))
alti2 %<>% filter(!is.na(alter_id))

## add revisited matches back into data frame
alti %<>% anti_join(alti2, by=c('response_id','alter','alter_org')) %>%
  bind_rows(alti2)
rm(alti2)


# Social ties between survey respondents: Expand --------------------------
## link survey respondents to all organizations that individual alters 'work on behalf of.' ##

## grab all affiliations for alters in our survey data set
alti %<>% filter(!is.na(alter_id))
alter_info <- alti %>% dplyr::select(alter_id,alter_org, alter) %>%
  left_join(dat %>% dplyr::select(response_id,org_name,multi_org) %>% distinct(), by=c('alter_id'='response_id'))

## how many alters are affiliated with multiple organizations? 
sum(!is.na(alter_info$multi_org)) ## 25

## how many alters are affiliated with one organization, and have a primary organization that doesn't match the alter info?
length(filter(alter_info, alter_org != org_name) %>% pull(alter_id)) ## 6. all but one are different naming for the same org, my use of projects within orgs.

## create a new data frame to expand on alter org info. data frame has: orig_alter | alter
new_alter_info <- alter_info %>% filter(alter_org != org_name & is.na(multi_org))
new_alter_info %<>% filter(alter_org=='Giant Giant Kelp Restoration Project - Caspar Cove Project') %>%
  bind_rows(new_alter_info %>% filter(alter_org=='Giant Giant Kelp Restoration Project - Caspar Cove Project') %>%
              mutate(alter_org=org_name))

new_alter_info %<>% dplyr::select(alter,alter_org) %>% separate(alter, into=c('tmp','alter_name'), sep=':',remove=FALSE) %>%
  dplyr::select(-tmp) %>%
  mutate(new_alter=case_when(grepl('Giant Giant',alter_org) ~ alter_org,
                             grepl('Alliance',alter_org) ~ paste0(alter_org,': ',alter_name),
                             .default=NA))

new_alter_info2 <- alter_info %>% filter(!is.na(multi_org))

new_alter_info2 %<>% separate(multi_org, into=c('org1','org2','org3','org4','org5','org6'), sep=',') %>%
  dplyr::select(-org1) %>% pivot_longer(starts_with('org'), names_to='org_level',values_to='org_name') %>%
  filter(!is.na(org_name)) %>%
  separate(alter, into=c('tmp','alter_name'), sep=':',remove=FALSE) %>%
  dplyr::select(-tmp) %>%
  unite('new_alter',org_name,alter_name,sep=':')
new_alter_info2 %<>% distinct()
new_alter_info2 %<>% mutate(new_alter=str_replace(new_alter,' Cordell Bank','Cordell Bank'))
new_alter_info2 %<>% dplyr::select(alter,new_alter)


## replace the alters in our input data set!
dat %<>% anti_join(new_alter_info, by='alter') %>%
  bind_rows(new_alter_info %>% left_join(dat, by='alter') %>%
              dplyr::select(-alter) %>% rename(alter=new_alter))

dat %<>% anti_join(new_alter_info2, by='alter') %>%
  bind_rows(new_alter_info2 %>% left_join(dat, by='alter') %>%
              dplyr::select(-alter) %>% rename(alter=new_alter))

## Check if it worked!
dat %<>% dplyr::select(-alter_name)
View(dat) ## search for "Jon" and "Carr"



# Cross-check org names ---------------------------------------------------
## Revise organization names that don't match between alter / ego data.
## for the SEN, this includes org names that I made extra specific to help
## connect people to kelp administrative areas.
dat_out <- dat

all_egos <- unique(dat %>% dplyr::select(response_id,multi_org) %>%
                     separate(multi_org, into=c('org1','org2','org3','org4','org5','org6'), sep=',') %>% 
                     pivot_longer(starts_with('org'), names_to='org_level',values_to='org_name') %>%
                     filter(!is.na(org_name)) %>%
                     pull(org_name))

all_alters <- dat %>% dplyr::select(response_id,alter) %>%
                       separate(alter,into=c('alter','alter_ind'), sep=':')

all_alters %<>% filter(!(alter %in% c('Commercial Diver','Artist','Photographer'))) %>% bind_rows(
  all_alters %>% filter(alter %in% c('Commercial Diver','Artist','Photographer')) %>%
    unite('alter',alter, alter_ind, sep=':'))

all_alters <- unique(filter(all_alters,!is.na(alter)) %>% pull(alter))

all_egos
all_alters


## there are multiple project / lab names in the same organization.
all_orgs <- data.frame(egos=all_egos) %>%
  separate(egos, into=c('org_name','ego_subgroup'), sep=' - ') %>%
  mutate(ego=1) %>%
  bind_rows(
    data.frame(alters=all_alters) %>%
      separate(alters, into=c('org_name','alter_subgroup'), sep=' - ') %>%
      mutate(alter=1)
  ) %>%
    group_by(org_name) %>% summarise(ego=sum(ego,na.rm=TRUE),alter=sum(alter,na.rm=TRUE),
                                     ego_sub=paste0(unique(ego_subgroup),collapse=','),
                                     alter_sub=paste0(unique(alter_subgroup),collapse=','))

## save this to create a manual key
write_csv(all_orgs,here('data','sen','sn_match_ego_alter_organizations.csv'))
# 
# data.frame(org_name=unique(c(all_egos,all_alters))) %>%
#   write_csv(here('data','sen','sn_match_ego_alter_organizations_KEY.csv'))  ## this will overwrite existing key!!

# Create social network ---------------------------------------------------

# First Pass: Data Res collabs only ------------------------------------
## read in names key


## apply re-naming to 'dat' dataframe.


## save as data frame


## save as matrix

#zeros v. missing data


# Second Pass: all collaborations -----------------------------------------






