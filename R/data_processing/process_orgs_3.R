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
q3 <- read_csv(here('data','sen',paste0('processed_by_responseID_orgs_4sen_2025-07-18.csv')))

## this is the cleaned up data on organizationst that people work directly with
q11 <- read_csv(here('confidential_data','processed',paste0('processed_by_responseID_q11_collabs_4sen_2025-07-14.csv')))

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
# q3q11 %>%
#   filter(!(response_id %in% orgdat0$response_id)) %>%
#   bind_rows(orgdat0 %>%
#               pivot_wider(names_from='ego_level',values_from='ego') %>%
#               mutate(org_name=q3_individual_1,multi_org=NA)) %>%
# write_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_updateIND_4sen_',Sys.Date(),'.csv')))
# 
# 
# 
# q3q11 %>%
#   filter(!(response_id %in% orgdat0$response_id)) %>%
#   bind_rows(orgdat0 %>%
#               pivot_wider(names_from='ego_level',values_from='ego') %>%
#               mutate(org_name=q3_individual_1,multi_org=NA)) %>%
#   select(response_id, org_name, starts_with('q3'), multi_org, type, alter) %>%
#   write_csv(here('data','sen',paste0('processed_by_responseID_q3orgs_q11collabs_updateIND_4sen_',Sys.Date(),'.csv')))



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


#################################### stopped here 7/17 ####################################
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

View(update_alter)

# Save VERSION: Changes to here -------------------------------------------

## this is for when we assign individuals to administrative areas. Egos are changed only for respondents
##    who work as an individual
write_csv(here('_updateINDfilterALTER'))

# 2+ ORGS: Exact Ego-Alter Match change EGO -----------------------------

## split 'both' into respondents who exclusively listed rc / g2kr as both ego and alter
tofix_both_rc <- filter(tofix_both, )

## manually update
update_both <- filter(tofix_both, response_id=='R_1AGjFLTVbTA4HTj') %>%
  filter(!alter %in% c('University of California Santa Cruz','The Nature Conservancy')) %>%
  filter(ego != 'California Department of Fish and Wildlife') %>%
  bind_rows(
    
  )



# Save --------------------------------------------------------------------


write_csv(here('_updateINDfilterALTERfilterEGO'))











# Retired code ------------------------------------------------------------

## there seems to be quite a lot of overlap between "work on behalf of" and "collaborate with."
## for people who said that they work on behalf of multiple organizations, check to see if any 
## "work on behalf of" organizations are listed as "collaborate with" as well. Unless the person
## identifies a specific individual that they collaborate with, 

## note that this is for building the social network part of the SEN! for assigning individuals
## to sub-county administrative areas, use all "work on behalf of" organizations.

## read in data
sn <- read_csv(here('confidential_data', 'processed','cleaned_social_network_with_org_allDOs_2025-04-30.csv'))

## full survey data set - helpful for investigating individual responses.
dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
   clean_names() %>%
   slice(-c(1:2))

my_grepl <- function(x,y){
  out <- c()
  for(i in 1:length(x)){out[i] <- grepl(x[i],y[i])}
  return(out)
}

## grab collaborative ties and split alters into ORGANIZATION - GROUP : PERSON
sn_collab <- filter(sn, edge_type=="collaboration")
sn_collab %<>% separate(alter, into=c('alter','alter_name'), sep=': ')
sn_collab %<>% separate(alter, into=c('alter','alter_grp1','alter_grp2'), sep='- ')

sn_collab %<>% dplyr::select(response_id,starts_with('alter')) %>% distinct()
head(sn_collab)
sn_collab %<>% mutate(collab=1)  # this is for joining later in the script


## now bring in all organizations each individual worked on behalf of
multis_check <- sn %>% select(response_id,org_name,multi_org) %>% distinct()
multis_check %<>% separate(multi_org,into=c('morg1','morg2','morg3','morg4','morg5'), sep=', ') # split one column into many (one org per column)

## each organization gets its own row
multis_check %<>% 
   pivot_longer(col=c('org_name', starts_with('morg')), names_to='org_level',values_to='org_name') %>%
   filter(!is.na(org_name))

## we don't have to worry about individuals
multis_check %<>% filter(org_name != 'Individual')

## split organization names into org and group
multis_check %<>% separate(org_name, into=c('org_name','org_grp1','org_grp2'), sep='- ')

## join alters to 'multis_check' data frame, by organization name
multis_check %<>% left_join(sn_collab, by=c('response_id','org_name'='alter'))

## remove organizations that we want to **keep** as **'work on behalf of'**
## (1) no collaborations within the organization someone works on behalf of
multis_edit <- multis_check %>%
   filter(collab==1)
dim(multis_check); dim(multis_edit)
# [1] 243   9
# [1] 111   9

## (2) when someone listed the same org for "work on behalf of" and "collaborate with", but they specified a different individual they work with.
multis_edit %<>%
   filter(is.na(alter_name))
dim(multis_edit)
# [1] 59  9

## (3) when someone works on behalf of a different group or project team than their alter, within the same org
multis_edit %<>% filter(is.na(org_grp1) | (alter_grp1 == org_grp1) | grepl('Surfrider',org_name))
dim(multis_edit)
# [1] 58  9


## how many people listed as a collaborator their primary "work on behalf of" organization
multis_edit %>% filter(org_level=='org_name') %>% group_by(org_name) %>% summarise(n=length(unique(response_id)))
# org_name                                        n
# 1 Coastal Conservation Association California     1
# 2 Get Inspired                                    1
# 3 Greater Farallones Association                  1
# 4 InterTribal Sinkyone Wilderness Council         1
# 5 Reef Check                                     26
# 6 University of California Berkeley               1
# 7 University of California Davis                  2
# 8 University of California Santa Cruz             1


## !!!! DECISION: Remove Reef Check as a collaborator, for people who have Reef Check as their primary "work on behalf of" organization
multis_edit %<>% mutate(rmv_collab=ifelse(org_name=='Reef Check' & collab==1, 1, 0))
## !!!! DECISION: Do some ground truthing of individuals who listed as a collaborator their primary "work on behalf of" organization. 
##      where are they listed on the org website as employed by them / serving on an advisory board?
##      unless it's clearly a volunteer organization, remove the affiliation. otherwise, remove the collaboration. 

multis_ground <- multis_edit %>% filter(org_level=='org_name' & rmv_collab==0) 
View(filter(dat_survey, response_id %in% multis_ground$response_id))

multis_edit %<>% mutate(rmv_collab=case_when(
   response_id=='R_1LLVDCfsYqnIq9H' & org_level=='org_name' ~ 1, 
   response_id=='R_1AGjFLTVbTA4HTj' & org_level=='org_name' ~ 1,
   response_id=='R_3eavCPS10YuL9kK' & org_level=='org_name' ~ 1,
   response_id=='R_6aIROXxP5g9ceLh' & org_level=='org_name' ~ 1,
   response_id=='R_6GbV3QDZn2JJCiR' & org_level=='org_name' ~ 1,
   response_id=='R_50f07NmTN6tehhY' & org_level=='org_name' ~ 1,
   response_id=='R_7GVc2EVkNJq5OuE' & org_level=='org_name' ~ 1,
   .default=rmv_collab
)) %>%
   mutate(alter_grp1=ifelse(response_id=='R_37vKJWxcl9TGyAh' & org_name=='University of California Davis', 'Kelp Restoration as an Integrated Socio-Ecological System',alter_grp1))

multis_edit %<>% filter((is.na(alter_grp1) & is.na(org_grp1)) | alter_grp1 == org_grp1)
## how many people listed multiple organizations as both 'work on behalf of' and 'collaborators'?
multis_edit %>% group_by(response_id) %>% summarise(n=length(unique(org_name))) %>% filter(n>1) %>%
   left_join(multis_edit)

# response_id           n
# 1 R_1AGjFLTVbTA4HTj     3
# 2 R_1GBVWsZocl0VprB     2
# 3 R_1QxOFb6rcRAJfNj     2
# 4 R_3eavCPS10YuL9kK     2
# 5 R_3ruJtqqgLFhsPQJ     2
# 6 R_5i7YPwDuFEqceK9     2
# 7 R_7GVc2EVkNJq5OuE     2
# 8 R_7TCeiJcmXqj5D3j     2
# 9 R_7upUSPV19KS9FVh     2


## check out the full list of affiliations for respondents who have a rmv_collab==0. Are there any that we don't want to remove the affiliation for?
multis_ground <- multis_edit %>% filter(rmv_collab==0)
multis_ground %<>%
   bind_rows(multis_check %>% filter(response_id %in% multis_edit$response_id) %>%
                anti_join(select(multis_ground, response_id, org_level)))
multis_ground %<>% mutate(org_level=factor(org_level, levels=c('org_name','morg1','morg2','morg3'))) %>% arrange(response_id,org_level)


multis_edit %<>% mutate(rmv_collab=case_when(
   org_name=='Kelp Restoration and Management Plan Community Working Group' ~ 1, 
   response_id=='R_1GBVWsZocl0VprB' & org_name=='Ocean Rainforest' ~ 1,
   response_id=='R_1QxOFb6rcRAJfNj' ~ 1,
   response_id=='R_1kGTi1GqQ0cAh7l' & org_grp1=='Sunflower Star Laboratory' ~ 1,
   response_id=='R_3eavCPS10YuL9kK' & org_level=='morg1' ~ 1,
   response_id=='R_3ruJtqqgLFhsPQJ' & org_name=='Monterey Bay Aquarium' ~ 1,
   response_id=='R_5i7YPwDuFEqceK9' & org_name=="Watermen's Alliance" ~ 1,
   response_id=='R_5vldi5R3qII19F6' & org_name=='University of California San Diego ' ~ 1,
   response_id=='R_7upUSPV19KS9FVh' & org_name=='Moss Landing Marine Laboratories ' ~ 1,
   response_id=='R_3iPGxjt8gAiTFfa' & org_name=="Watermen's Alliance" ~ 1,
   response_id=='R_7TCeiJcmXqj5D3j' ~ 1,   ## NMSs
   .default=rmv_collab))
multis_edit %<>% mutate(rmv_affiliation=case_when(
   response_id=='R_3QfvHYy3RtQ0Y3p' & org_name=='Greater Farallones National Marine Sanctuary' ~ 1,
   response_id=='R_3qIDU2i8sWSWYdH' & org_name=='The Nature Conservancy' ~ 1,
   response_id=='R_5R8mhuPcpBgJCfg' & org_name=='The Nature Conservancy' ~ 1,
   response_id=='R_7GVc2EVkNJq5OuE' & org_name=='University of California Davis' ~ 1,
   response_id=='R_7L1g8rvgxtmyocF' & org_name=='University of California Santa Barbara' ~ 1,
   response_id=='R_1AGjFLTVbTA4HTj' & org_name=='The Nature Conservancy' ~ 1,
   response_id=='R_1AGjFLTVbTA4HTj' & org_name=='California Department of Fish and Wildlife' ~ 1,
   .default=0))

filter(multis_edit, rmv_collab==0 & rmv_affiliation==0)

## now fix the sn data frame using multis_edit

sn_out <- sn



## First, the collaborations to remove
sn_out %<>% separate(alter, into=c('alter_org','alter_name'), sep=': ', remove=FALSE)
sn_out %<>% separate(alter_org, into=c('alter_org','alter_grp1','alter_grp2'), sep='- ')

sn_out %<>% anti_join(filter(multis_edit, rmv_collab==1) %>% mutate(alter_org=org_name) %>% select(response_id,alter_org, alter_grp1, alter_grp2, alter_name))
dim(sn)
# [1] 1008    7
dim(sn_out)
# [1] 955  11
sn_out %<>% select(-alter_org,-alter_grp1,-alter_grp2,-alter_name)

## Next, the affiliations to remove
to_rmv <- multis_edit %>% filter(rmv_affiliation==1) ## thankfully, don't have to worry about re-uniting the groups / project teams with the org name.
sn_out %<>% separate(multi_org,into=c('morg1','morg2','morg3','morg4','morg5'), sep=', ') # split one column into many (one org per column)
sn_out %<>% pivot_longer(col=c('org_name', starts_with('morg')), names_to='org_level',values_to='org_name') %>%
   filter(!is.na(org_name))

sn_out %<>% anti_join(select(to_rmv, response_id, org_level, org_name))

## Check a few
sn_out %>% filter(response_id %in% to_rmv$response_id) %>% select(response_id, org_level, org_name) %>% distinct()
to_rmv # looks good!


sn_out %>% 
   select(response_id, org_level, org_name, type, edge_type,alter) %>%
   write_csv(here('confidential_data', 'processed','cleaned_social_network_with_org_allDOs_fixMultiOrgCollab_2025-06-07.csv'))

sn_out %>% 
   select(response_id, org_level, org_name, type, edge_type,alter) %>%
   write_csv(here('../','california-kelp-SEN','data','survey','confidential','cleaned_social_network_with_org_allDOs_fixMultiOrgCollab_2025-06-07.csv'))


