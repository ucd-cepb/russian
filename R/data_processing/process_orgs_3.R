############### Process survey data on ego organization ############### 
#
# Adjust processed social network data frame for overlap in  
#   which organizations survey respondents *work on behalf of,*
#   and which they *work directly with.* 
# For "rules" on how this is done, check file: doc >> METHODS_survey_data_processing.docx
#
# To make my life easier, for individuals who work on behalf of two or more organizations,
#   I only cleaned up data for those who directly observe conditions.
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




# Data --------------------------------------------------------------------

## this is the survey data
dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
  clean_names() %>%
  slice(-c(1:2))

colnames(dat_survey)

## this is the cleaned up data on organizations that people work on behalf of
q3 <- read_csv(here('data','sen',paste0('processed_by_responseID_orgs_4sen_2025-08-04.csv')))

## this is the cleaned up data on organizationst that people work directly with
q11 <- read_csv(here('confidential_data','processed',paste0('processed_by_responseID_q11_collabs_4sen_2025-08-04.csv')))

## helpful Qs in the survey data
qs_of_interest <- make_clean_names(c('response_id','status',"recipient_last_name","recipient_first_name",
                                     "q3_individual_1",
                                     colnames(dat_survey)[grepl('q4',colnames(dat_survey))],
                                     colnames(dat_survey)[grepl('q9',colnames(dat_survey))]))

## combine q3 and q11 by response ID
head(q3)
head(q11)

## save!!
q3q11 <- q3 %>% left_join(q11, by='response_id')
write_csv(q3q11, here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_4sen_',Sys.Date(),'.csv')))

## reformat. we want each ego to be matched with every alter so we can compare all pairs.
orgdat <- q3q11 %>% select(-org_name,-multi_org) %>%   ## this is either q3_individual_1 or q3_several_1
  pivot_longer(cols=starts_with('q3'), names_to='ego_level',values_to='ego') %>%  
  filter(!is.na(ego))

## empty df for tracking sample sizes
qc_df <- data.frame(q3=as.character(),
                    category=as.character(),
                    n_alters=as.character(),
                    rc_or_g2kr=as.numeric(),
                    n=as.numeric())

# INDIVIDUAL: Reef Check or G2KR ------------------------------------------
View( filter(orgdat, grepl('Individual',ego)) )

orgdat0 <- orgdat %>% filter(grepl('Individual',ego) & ego_level=="q3_individual_1")
orgdat0_n <- length(unique(orgdat0$response_id))

orgdat0 %<>% select(response_id) %>% distinct() %>% left_join(orgdat)

## filter for only volunteer / cit sci 
filtdat0 <- orgdat0 %>%
  mutate(rc_or_g2kr=ifelse(alter %in% c("Reef Check","Giant Giant Kelp Restoration Project"), 1, 0)) %>%
  group_by(response_id) %>%
  summarise(rc_or_g2kr=sum(rc_or_g2kr), n_alter=length(unique(alter))) %>%
  mutate(q3='none',
         category='vol or citsci') 

## create corrected data frame
tofix <- filtdat0 %>% filter(rc_or_g2kr > 0, n_alter > 1) %>%  ## all n_alter > 1 when rc_or_g2kr > 0
  select(response_id) %>% left_join(orgdat0)
tofix

update <- tofix %>%
  filter(type=='q11_1') %>%
  select(response_id, alter) %>%
  rename(ego=alter) %>% 
  left_join(tofix %>% filter(type != 'q11_1') %>% select(-ego))


## update the data frame for individuals involved on behalf of one organization
orgdat0 %<>% anti_join(tofix) %>%
  bind_rows(update)

## check
orgdat0

## record sample sizes for QC
qc_df %<>% bind_rows(
  filtdat0 %>% mutate(n_alters=ifelse(n_alter > 1, "2-plus","1")) %>%
    group_by(q3, category, rc_or_g2kr,n_alters) %>%
    summarise(n=length(unique(response_id)))
)


# Save VERSION: Changes to here -------------------------------------------

# this is for when we assign individuals to administrative areas. Egos are changed only for respondents
#    who work as an individual
q3q11 %>%
  filter(!(response_id %in% orgdat0$response_id)) %>%
  bind_rows(orgdat0 %>%
              pivot_wider(names_from='ego_level',values_from='ego') %>%
              mutate(org_name=q3_individual_1,multi_org=NA)) %>%
write_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_updateIND_4sen_',Sys.Date(),'.csv')))

# 
# 
q3q11 %>%
  filter(!(response_id %in% orgdat0$response_id)) %>%
  bind_rows(orgdat0 %>%
              pivot_wider(names_from='ego_level',values_from='ego') %>%
              mutate(org_name=q3_individual_1,multi_org=NA)) %>%
  select(response_id, org_name, starts_with('q3'), multi_org, type, alter) %>%
  write_csv(here('data','sen',paste0('processed_by_responseID_q3orgs_q11collabs_updateIND_4sen_',Sys.Date(),'.csv')))



# ONE ORG: Exact Ego-Alter match ----------------------------------
orgdat1 <- filter(orgdat, ego_level=='q3_individual_1' & !(response_id %in% orgdat0$response_id))
orgdat1_n <- length(unique(orgdat1$response_id))

## exact matches
View(orgdat1 %>% filter(ego==alter))

## filter for exact matches
filtdat1 <- orgdat1 %>% filter(ego==alter) %>%
  group_by(response_id) %>%
  summarise(rc_or_g2kr=ifelse(ego %in% c("Reef Check","Giant Giant Kelp Restoration Project"), 1, 0)) %>%
  left_join(orgdat, by='response_id') %>%
  group_by(response_id, rc_or_g2kr) %>%
  summarise(n_alter=length(unique(alter))) %>%
  mutate(q3='one',
         category='exact match') 


## create corrected data frame, only for those who have an alter other than the one that matches their ego org
tofix <- filtdat1 %>% filter(n_alter > 1) %>%
  select(response_id) %>% left_join(orgdat1)

update <- filter(tofix, ego!=alter)


## update the data frame for individuals involved on behalf of one organization
orgdat1 %<>% anti_join(tofix) %>%
  bind_rows(update)

## check
orgdat1 %>% filter(ego==alter) %>%
  group_by(response_id) %>% summarise(n_alter=length(unique(alter)))  # all n_alter should = 1

## record sample sizes for QC
qc_df %<>% bind_rows(
  filtdat1 %>% mutate(n_alters=ifelse(n_alter > 1, "2-plus","1")) %>%
    group_by(q3, category, rc_or_g2kr,n_alters) %>%
      summarise(n=length(unique(response_id)))
)
## check new assignments
View(filtdat1 %>% filter(n_alter > 1) %>% left_join(orgdat1))

# Save VERSION: Changes to here -------------------------------------------

# this is for when we assign individuals to administrative areas. Egos are changed only for respondents
#    who work as an individual
q3q11 %>%
  filter(!(response_id %in% orgdat0$response_id)) %>%
  filter(!(response_id %in% orgdat1$response_id)) %>%
  bind_rows(orgdat0 %>%
              pivot_wider(names_from='ego_level',values_from='ego') %>%
              mutate(org_name=q3_individual_1,multi_org=NA)) %>%
  bind_rows(orgdat1 %>%
              pivot_wider(names_from='ego_level',values_from='ego') %>%
              mutate(org_name=q3_individual_1,multi_org=NA)) %>%
  write_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_updateINDupdateONE_4sen_',Sys.Date(),'.csv')))


q3q11 %>%
  filter(!(response_id %in% orgdat0$response_id)) %>%
  filter(!(response_id %in% orgdat1$response_id)) %>%
  bind_rows(orgdat0 %>%
              pivot_wider(names_from='ego_level',values_from='ego') %>%
              mutate(org_name=q3_individual_1,multi_org=NA)) %>%
  bind_rows(orgdat1 %>%
              pivot_wider(names_from='ego_level',values_from='ego') %>%
              mutate(org_name=q3_individual_1,multi_org=NA)) %>%
  write_csv(here('../california-kelp-SEN','data','survey','confidential',paste0('processed_by_responseID_q3orgs_q11collabs_updateINDupdateONE_4sen_',Sys.Date(),'.csv')))


#################################### stopped here 7/21 ####################################
# 2+ ORGS: Exact Ego-Alter Match ------------------------------------------

## grab respondents who work on behalf of multiple orgs
orgdat2 <- filter(orgdat, ego_level=='q3_several_1') %>%
  select(response_id) %>% distinct() %>% left_join(orgdat)
orgdat2_n <- length(unique(orgdat2$response_id))  # 63

## exact matches
View(orgdat2 %>% filter(ego==alter) %>% 
       mutate(category=ifelse(ego_level=='q3_several_1', 'exact-first ego','exact-other ego')))


## filter for exact matches
filtdat2 <- orgdat2 %>% filter(ego==alter) %>% 
  mutate(category=ifelse(ego_level=='q3_several_1', 'exact-first ego','exact-other ego')) %>%
  select(response_id, category) %>% distinct() %>%
  left_join(orgdat2,by='response_id') %>%
  mutate(rc_or_g2kr=ifelse(ego %in% c("Reef Check","Giant Giant Kelp Restoration Project"), 1, 0)) %>%
  group_by(response_id, category) %>%
  summarise(n_alter=length(unique(alter)),rc_or_g2kr=sum(rc_or_g2kr),
            n_ego=length(unique(ego))) %>%
  mutate(q3='two') 
with(filtdat2, table(category))
# exact-first ego exact-other ego 
# 17              23
#   

## remove alter IF first ego listed is also an exact match for an alter
tofix_alter <- filter(filtdat2, category=='exact-first ego') %>%
  select(response_id) %>% distinct() %>% left_join(orgdat2)

## remove ego IF second or later ego listed is also an exact match for an alter
tofix_ego <- filter(filtdat2, category=='exact-other ego') %>%
  select(response_id) %>% distinct() %>% left_join(orgdat2)

## how many have both?
sum(unique(tofix_alter$response_id) %in% unique(tofix_ego$response_id))

## adjust to create three data frames for changing stuff
tofix_both <- filter(tofix_alter, response_id %in% tofix_ego$response_id)
tofix_alter %<>% filter(!response_id %in% tofix_both$response_id)
tofix_ego %<>% filter(!response_id %in% tofix_both$response_id)

## create output data frame
orgdat2_out <- filter(orgdat2, !(response_id %in% filtdat2$response_id))


# 2+ ORGS: Exact Ego-Alter Match change ALTER -----------------------------
filtdat2a <- filter(filtdat2, response_id %in% tofix_alter$response_id)
filtdat2a

## put the two with 1 alter back into the data set (don't remove any alters, to avoid filtering them out of the social network)
update_alter <- tofix_alter %>% filter(response_id %in% filter(filtdat2a, n_alter==1)$response_id)
update_alter  # both alters are reef check, respondents involved in reef check and other vol activities


tofix_alter %<>% anti_join(update_alter)

update_alter %<>% bind_rows(
  tofix_alter %>% filter(response_id=='R_5i7YPwDuFEqceK9' & alter != 'Reef Check')
) %>% bind_rows(
  tofix_alter %>% filter(response_id=='R_50f07NmTN6tehhY' & alter != 'University of California Davis')
)

# View(update_alter)

# Save VERSION: Changes to here -------------------------------------------

## this is for when we assign individuals to administrative areas. Egos are changed only for respondents
##    who work as an individual
# write_csv(here('_updateINDfilterALTER'))

# 2+ ORGS: Exact Ego-Alter Match change EGO -----------------------------

## split 'both' into respondents who exclusively listed rc / g2kr as both ego and alter
# tofix_both_rc <- filter(tofix_both, )

## manually update
# update_both <- filter(tofix_both, response_id=='R_1AGjFLTVbTA4HTj') %>%
#   filter(!alter %in% c('University of California Santa Cruz','The Nature Conservancy')) %>%
#   filter(ego != 'California Department of Fish and Wildlife') %>%
#   bind_rows(
#     
#   )



# Save --------------------------------------------------------------------


# write_csv(here('_updateINDfilterALTERfilterEGO'))



