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
d.in <- '2025-09-23'
d.out <- d.in
#
write_out <- FALSE

# Data --------------------------------------------------------------------

## this is the survey data
dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
  clean_names() %>%
  slice(-c(1:2))

colnames(dat_survey)

## this is the cleaned up data on organizations that people work on behalf of
q3 <- read_csv(here('data','sen',paste0('processed_by_responseID_orgs_4sen_',d.in,'.csv')))

## this is the cleaned up data on organizationst that people work directly with
q11 <- read_csv(here('confidential_data','processed',paste0('processed_by_responseID_q11_collabs_4sen_',d.in,'.csv')))

## this tells us which respondents directly observe kelp conditions
info <- read_csv(here('confidential_data', 'processed','cleaned_responseID_by_info_source_q9.csv')) 
do <- unique(info %>%
  filter(info_type == 'I directly observe conditions') %>%
  pull(response_id))

## this is where people are involved in kelp-related issues
loc <- read_csv(here('confidential_data','processed','cleaned_responseID_by_county.csv'))

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
q3q11 %<>% mutate(direct_observer=ifelse(response_id %in% do, '1','0'))  

if(write_out){   write_csv(q3q11, here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_4sen_',d.out,'.csv')))   }

## reformat. we want each ego to be matched with every alter so we can compare all pairs.
orgdat <- q3q11 %>% select(-org_name,-multi_org) %>%   ## this is either q3_individual_1 or q3_several_1
  pivot_longer(cols=starts_with('q3'), names_to='ego_level',values_to='ego') %>%  
  filter(!is.na(ego))

## empty df for tracking sample sizes
qc_df <- data.frame(q3=as.character(),
                    category=as.character(),
                    n_alters=as.numeric(),
                    rc_or_g2kr=as.numeric(),
                    n=as.numeric())

# INDIVIDUAL: no alters ---------------------------------------------------
View( filter(orgdat, grepl('Individual',ego)) )
## do we have location info for the people who work as individuals and don't have alters?
View( filter(orgdat, grepl('Individual',ego)) %>% filter(is.na(alter)) %>% left_join(loc,by='response_id') )
to_rmv <- filter(orgdat, grepl('Individual',ego)) %>% filter(is.na(alter)) %>% left_join(loc,by='response_id') %>% filter(is.na(county))

## remove individuals with no location info, no alters, and no organization
orgdat %<>% filter(!(response_id %in% to_rmv$response_id))
q3q11_out <- q3q11 %<>% filter(!(response_id %in% to_rmv$response_id))

qc_df %<>% bind_rows(
  data.frame(q3='Individual',
  category='no data',
  n_alters=0,
  rc_or_g2kr=0,
  n=2)
)

# INDIVIDUAL: Reef Check or G2KR ------------------------------------------
View( filter(orgdat, grepl('Individual',ego)) )
## one
orgdat0 <- orgdat %>% filter(grepl('Individual',ego) & ego_level=="q3_individual_1")
orgdat0_n <- length(unique(orgdat0$response_id))
## first of several
orgdat00 <- orgdat %>% filter(grepl('Individual',ego) & ego_level=="q3_several_1") %>%
  dplyr::select(response_id) %>% distinct() %>% left_join(orgdat, by='response_id')

## filter for only volunteer / cit sci 
filtdat0 <- orgdat0 %>%
  mutate(rc_or_g2kr=ifelse(alter %in% c("Reef Check","Giant Giant Kelp Restoration Project"), 1, 0)) %>%
  group_by(response_id) %>%
  summarise(rc_or_g2kr=sum(rc_or_g2kr), n_alter=length(unique(alter))) %>%
  mutate(q3='Individual',
         category='vol or citsci') 
filtdat00 <- orgdat00 %>%
  mutate(rc_or_g2kr=ifelse(alter %in% c("Reef Check","Giant Giant Kelp Restoration Project"), 1, 0)) %>%
  group_by(response_id) %>%
  summarise(rc_or_g2kr=sum(rc_or_g2kr), n_alter=length(unique(alter))) %>%
  mutate(q3='Individual',
         category='vol or citsci') 


## create corrected data frame
tofix0 <- filtdat0 %>% filter(rc_or_g2kr > 0, n_alter > 1) %>%  ## all n_alter > 1 when rc_or_g2kr > 0
  select(response_id) %>% left_join(orgdat0)
tofix0

tofix00 <- filtdat00 %>% filter(rc_or_g2kr > 0 & n_alter > 1) %>%  ## all n_alter > 1 when rc_or_g2kr > 0
  select(response_id) %>% left_join(orgdat00)
tofix00

update <- tofix0 %>%
  filter(type=='q11_1') %>%
  select(response_id, alter) %>%
  rename(ego=alter) %>% 
  left_join(tofix0 %>% filter(type != 'q11_1') %>% select(-ego)) %>%
  bind_rows(
    tofix00 %>% filter(ego_level=='q3_several_2') %>%
      mutate(ego_level='q3_individual_1') %>%
      filter(alter != ego) %>% distinct()
      
  )

## check
update

## record sample sizes for QC
qc_df %<>% bind_rows(
  filtdat0 %>% filter(response_id %in% update$response_id) %>%
    rename(n_alters=n_alter) %>%
    group_by(q3, category, rc_or_g2kr,n_alters) %>%
    summarise(n=length(unique(response_id)))
) %>% bind_rows(
  filtdat00 %>%
    rename(n_alters=n_alter) %>% filter(response_id %in% update$response_id) %>%
    group_by(q3, category, rc_or_g2kr,n_alters) %>%
    summarise(n=length(unique(response_id)))
)


# Save VERSION: Changes to here -------------------------------------------

# this is for when we assign individuals to administrative areas. Egos are changed only for respondents
#    who work as an individual
q3q11_out %<>%
  filter(!(response_id %in% update$response_id)) %>%
  bind_rows(update %>%
              pivot_wider(names_from='ego_level',values_from='ego') %>%
              mutate(org_name=q3_individual_1,multi_org=NA))
if(write_out){  q3q11_out %>%
write_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_updateIND_4sen_',d.out,'.csv')))   }

# 
# 
if(write_out){  q3q11_out %>%
  dplyr::select(response_id, org_name, starts_with('q3'), multi_org, type, alter) %>%
  write_csv(here('data','sen',paste0('processed_by_responseID_q3orgs_q11collabs_updateIND_4sen_',d.out,'.csv')))   }


rm(filtdat0,filtdat00,orgdat0,orgdat00)

# ONE EGO ORG: Exact Ego-Alter match ----------------------------------
orgdat1 <- filter(orgdat, ego_level=='q3_individual_1' & !(response_id %in% update$response_id) & !grepl('Individual',ego))
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
##    and they have more than one alter listed.
tofix <- filtdat1 %>% filter(n_alter > 1) %>%
  select(response_id) %>% distinct() %>% left_join(orgdat1)

update1 <- filter(tofix, ego!=alter)
update1 %<>% distinct()


## record sample sizes for QC
qc_df %<>% bind_rows(
  filtdat1 %>% filter(response_id %in% update1$response_id) %>%
    rename(n_alters=n_alter) %>%
    group_by(q3, category, rc_or_g2kr,n_alters) %>%
      summarise(n=length(unique(response_id)))
)



# Save VERSION: Changes to here -------------------------------------------

# this is for when we assign individuals to administrative areas. Egos are changed only for respondents
#    who work as an individual
q3q11_out %<>%
  filter(!(response_id %in% update1$response_id)) %>%
  bind_rows(update1 %>%
              pivot_wider(names_from='ego_level',values_from='ego') %>%
              mutate(org_name=q3_individual_1,multi_org=NA))

if(write_out){   q3q11_out %>%
  write_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_updateINDupdateONE_4sen_',d.out,'.csv')))    }


# q3q11_out %>%
#   write_csv(here('../california-kelp-SEN','data','survey','confidential',paste0('processed_by_responseID_q3orgs_q11collabs_updateINDupdateONE_4sen_',Sys.Date(),'.csv')))


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
# 18              26 
#   

## removing alters?
## remove alter IF first ego listed is also an exact match for an alter, and there is more than one alter
tofix_alter <- filter(filtdat2, category=='exact-first ego' & n_alter>1) %>%
  select(response_id) %>% distinct() %>% left_join(orgdat2) %>% distinct()
length(unique(tofix_alter$response_id)) #16

## removing egos?
## remove ego IF second or later ego listed is also an exact match for an alter
tofix_ego <- filter(filtdat2, category=='exact-other ego') %>%
  select(response_id) %>% distinct() %>% left_join(orgdat2)
length(unique(tofix_ego$response_id)) #26

## how many have both?
sum(unique(tofix_alter$response_id) %in% unique(tofix_ego$response_id))  #14

## adjust to create three data frames for changing stuff
tofix_both <- filter(tofix_alter, response_id %in% tofix_ego$response_id)
tofix_alter %<>% filter(!response_id %in% tofix_both$response_id)
tofix_ego %<>% filter(!response_id %in% tofix_both$response_id)
length(unique(tofix_both$response_id)) #14
length(unique(tofix_alter$response_id)) #2
length(unique(tofix_ego$response_id)) #12



## create output data frame
orgdat2_out <- filter(orgdat2, !(response_id %in% c(tofix_both$response_id, tofix_alter$response_id, tofix_ego$response_id)))


# 2+ ORGS: Removing Alters ------------------------------------------
update_alter <- tofix_alter %>%
  left_join( tofix_alter %>% filter(ego_level=='q3_several_1') %>% dplyr::select(response_id,ego) %>% 
               distinct() %>%
               rename(rmv_alter=ego), by='response_id')

update_alter %<>% filter(alter!=rmv_alter)
update_alter %<>% dplyr::select(-rmv_alter)
# check how many alters each person has, and that we retained all individuals
all(tofix_alter$response_id %in% update_alter$response_id)
update_alter %>% group_by(response_id) %>% summarise(n=length(unique(alter)))
  
## update the orgdat2 data frame
any(update_alter$response_id %in% orgdat2_out$response_id) # needs to be false

orgdat2_out %<>%
  bind_rows(update_alter)

## update q3q11_out 
q3q11_out %<>% filter(!(response_id %in% update_alter$response_id)) %>%
  bind_rows(update_alter %>%
              pivot_wider(names_from='ego_level',values_from='ego') %>%
              ## the org name and multi org columns haven't changed!
              left_join( q3q11_out %>% filter(response_id %in% update_alter$response_id) %>%
                           dplyr::select(response_id, org_name,multi_org) %>%
                           distinct(), by='response_id'))


## record sample sizes for QC
qc_df %<>% bind_rows(
  filter(filtdat2, category=='exact-first ego' & n_alter>1) %>%
    rename(n_alters=n_alter) %>%
    group_by(q3, category, rc_or_g2kr,n_alters) %>%
    summarise(n=length(unique(response_id)))
)



# 2+ ORGS: Removing egos --------------------------------------------------
View(tofix_ego)
update_ego <- tofix_ego %>%
  left_join( tofix_ego %>% filter(alter==ego) %>% 
               rename(rmv_ego=alter) %>%
               dplyr::select(response_id, rmv_ego) %>% 
               distinct(), by='response_id')
## check
View(update_ego %>% filter(ego_level=='q3_several_1') %>%
       dplyr::select(response_id,recipient_first_name,recipient_last_name,email,ego,rmv_ego) %>% distinct())

## make changes
update_ego2 <- update_ego %>% mutate(rmv_ego=case_when(
  response_id=='R_1GBVWsZocl0VprB' ~ NA,
  response_id=='R_1LY9bZGoDJpKr8U'~ NA,
  response_id=='R_1QxOFb6rcRAJfNj' & grepl('Davis',rmv_ego) ~ NA,
  response_id=='R_1qyNTuTpApbsKrv' ~ NA,
  response_id=='R_5vldi5R3qII19F6' ~ NA,
  response_id=='R_7417rkzwocKka1y' ~ NA,
  response_id=='R_3iPGxjt8gAiTFfa' ~ NA,
  response_id=='R_51QOureZvOe4yuD' ~ NA,
  .default=rmv_ego
)) %>% filter(!is.na(rmv_ego)) %>% dplyr::select(response_id,rmv_ego) %>% distinct() %>% rename(ego=rmv_ego)

View(update_ego2)

update_alter2 <- filter(update_ego, response_id %in% c('R_1GBVWsZocl0VprB','R_1LY9bZGoDJpKr8U','R_1qyNTuTpApbsKrv',
                                                       'R_5vldi5R3qII19F6','R_7417rkzwocKka1y','R_3iPGxjt8gAiTFfa',
                                                       'R_51QOureZvOe4yuD'))  %>%
  bind_rows(filter(update_ego, response_id=='R_1QxOFb6rcRAJfNj' & grepl('Davis',rmv_ego)))%>%
  dplyr::select(response_id, rmv_ego) %>%
  rename(alter=rmv_ego) %>% distinct()
View(update_alter2)


update_ego %<>% dplyr::select(-rmv_ego) %>% distinct()
update_ego %<>% anti_join(update_ego2)
update_ego %<>% anti_join(update_alter2)
update_ego %<>% filter(!(response_id=='R_1MEiTDdy7UTusoz' & ego %in% c('California Ocean Protection Council',
                                                                        'California Sea Grant',
                                                                        'Kelp Forest Alliance')))
update_ego

# check how many alters each person has, and that we retained all individuals
all(tofix_ego$response_id %in% update_ego$response_id)
update_ego %>% group_by(response_id) %>% summarise(n=length(unique(alter)))

## update the orgdat2 data frame
any(update_ego$response_id %in% orgdat2_out$response_id) # needs to be false

orgdat2_out %<>%
  bind_rows(update_ego)

## update q3q11_out 
q3q11_out %<>% filter(!(response_id %in% update_ego$response_id)) %>%
  bind_rows(update_ego %>%
              pivot_wider(names_from='ego_level',values_from='ego') %>%
              mutate(org_name=q3_several_1) %>%
              ## need to replace multi org column
              left_join(
                update_ego %>% group_by(response_id) %>%
                  summarise(multi_org=paste0(unique(ego),collapse=','))) %>%
              mutate(multi_org=ifelse(multi_org==q3_several_1, NA, multi_org)))


## record sample sizes for QC
# qc_df %<>% bind_rows(
#   filter(filtdat2, category=='exact-first ego' & n_alter>1) %>%
#     rename(n_alters=n_alter) %>%
#     group_by(q3, category, rc_or_g2kr,n_alters) %>%
#     summarise(n=length(unique(response_id)))
# )


# ORG 2+ : Adjusting ego and alter ----------------------------------------
View(tofix_both)
tofix_both_wide <- tofix_both %>% pivot_wider(names_from='ego_level', values_from='ego')

## 4 cases where volunteer divers / cit scientists were involved on behalf of all listed as alters. all of which were
##    a combo of RC, G2KR, and WA. didn't change any of these. 

update_both_wide <- bind_rows(
  ## remove alter for first listed ego, remove all other egos
  filter(tofix_both_wide, response_id %in% c('R_1AGjFLTVbTA4HTj','R_5rju7W12HtPajKX','R_7GVc2EVkNJq5OuE')) %>%
    filter(alter != q3_several_1) %>%
    mutate(q3_several_2=NA, q3_several_3=NA),
  
  ## removal alters only
  filter(tofix_both, response_id %in% c('R_3eavCPS10YuL9kK','R_3LjybcSyBNIJe9f')) %>%
    anti_join(
      filter(tofix_both, response_id %in% c('R_3eavCPS10YuL9kK','R_3LjybcSyBNIJe9f')) %>%
        filter(ego==alter) %>% dplyr::select(response_id,ego) %>%
        rename(alter=ego) %>% distinct(),
    by=c('response_id','alter')) %>% pivot_wider(names_from='ego_level', values_from='ego'),
  
  ## remove specific ego, two alters
  filter(tofix_both_wide, response_id=='R_7upUSPV19KS9FVh') %>%
    mutate(q3_several_3=NA) %>%    ## MLML-SSL
    filter(alter != q3_several_1 & alter != q3_several_2),
  
  ## remove ego only
  filter(tofix_both, response_id %in% c('R_5R8mhuPcpBgJCfg')) %>%
    anti_join(
      filter(tofix_both, response_id %in% c('R_5R8mhuPcpBgJCfg')) %>%
        filter(ego==alter & ego_level != 'q3_several_1') %>% dplyr::select(response_id,ego) %>%
        distinct(),
      by=c('response_id','ego')) %>% filter(alter!=ego) %>%
    pivot_wider(names_from='ego_level', values_from='ego'),
  
  ## move CDFW from ego to alter for one respondent in DISES.
  filter(tofix_both_wide, response_id =='R_5rju7W12HtPajKX') %>%
    mutate(alter=ifelse(grepl('North coast Evaluation',alter),'California Department of Fish and Wildlife',alter),
           q3_several_3=NA) %>%
    filter(alter != q3_several_2)
    
)



View(update_both_wide)

## update the orgdat2 data frame
any(update_both_wide$response_id %in% orgdat2_out$response_id) # needs to be false

orgdat2_out %<>%
  bind_rows(update_both_wide %>% pivot_longer(starts_with('q3_'), names_to='ego_level',values_to='ego') %>%
              filter(!is.na(ego)))

## update q3q11_out 
q3q11_out %<>% filter(!(response_id %in% update_both_wide$response_id)) %>%
  bind_rows(update_both_wide %>%
              mutate(org_name=q3_several_1) %>%
              ## need to replace multi org column
              left_join(
                update_both_wide %>% pivot_longer(starts_with('q3_'), names_to='ego_level',values_to='ego') %>%
                  filter(!is.na(ego)) %>%
                  summarise(multi_org=paste0(unique(ego),collapse=',')), by='response_id') %>%
              mutate(multi_org=ifelse(multi_org==q3_several_1, NA, multi_org)))



# SAVE THIS VERSION: Changes to here --------------------------------------

## fix the number for multiple orgs: when enough egos were removed that there is only one org for that respondent
q3q11_out %>% filter(is.na(q3_individual_1) & !is.na(q3_several_1) & is.na(multi_org))
q3q11_out %<>% mutate(q3_individual_1=ifelse(is.na(q3_individual_1) & 
                                                      !is.na(q3_several_1) & is.na(multi_org), q3_several_1,q3_individual_1)) %>%
  mutate(q3_several_1=ifelse(!is.na(q3_individual_1) & !is.na(q3_several_1) & is.na(multi_org), NA,q3_several_1))
q3q11_out %>% filter(is.na(q3_individual_1) & !is.na(q3_several_1) & is.na(multi_org))

## fix the number for multiple orgs: when we removed org #2 but not org #3
q3q11_out %>% filter(is.na(q3_several_1) & !is.na(q3_several_2))
q3q11_out %>% filter(is.na(q3_several_2) & !is.na(q3_several_3))
q3q11_out %<>% mutate(q3_several_2=ifelse(is.na(q3_several_2) & !is.na(q3_several_3),q3_several_3,q3_several_2)) %>%
  mutate(q3_several_3=ifelse(q3_several_3==q3_several_2,NA,q3_several_3))
q3q11_out %>% filter(is.na(q3_several_2) & !is.na(q3_several_3))

## fix the number for multiple orgs: when we removed org #2,3 but not org #4... NONE!
q3q11_out %>% filter(is.na(q3_several_2) & !is.na(q3_several_4))
q3q11_out %>% filter(is.na(q3_several_3) & !is.na(q3_several_4))

if(write_out){   q3q11_out %>%
    write_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_updateINDupdateORG_4sen_',d.out,'.csv')))    }


# q3q11_out %>%
#   write_csv(here('../california-kelp-SEN','data','survey','confidential',paste0('processed_by_responseID_q3orgs_q11collabs_updateINDupdateONE_4sen_',Sys.Date(),'.csv')))


# ORG 2+ : Check other egos -----------------------------------------------
## for anyone who listed >2 egos, check
many_egos <- filter(q3q11_out, !is.na(q3_several_3))
length(unique(many_egos$response_id))  ## 17

many_egos_long <- many_egos %>% pivot_longer(starts_with('q3_'), names_to='ego_level',values_to='ego') %>%
  dplyr::select(-multi_org) %>%
  filter(!is.na(ego)) %>% distinct()

# View(filter(dat_survey, response_id=='R_50v3Pt1UGJw51HP'))


update_many_egos <- bind_rows(
  filter(many_egos_long, response_id=='R_3rlB6ugHMc8cqTQ') %>%
    filter(!(ego %in% c('Moss Landing Marine Laboratories','Greater Farallones Assoc and NMS',
                        'The Nature Conservancy','University of California Davis - Bodega Marine Laboratory'))),
  filter(many_egos_long, response_id=='R_73yGa8USaSXYR0T') %>%
    filter(!(ego %in% c('University of California Davis',
                        'California Department of Fish and Wildlife',
                        'Monterey Bay National Marine Sanctuary',
                        'California Ocean Protection Council'))),
  filter(many_egos_long, response_id=='R_50v3Pt1UGJw51HP') %>%
    filter(!(ego %in% c('The Nature Conservancy',
                        'California Sea Grant'))),
  filter(many_egos_long, response_id=='R_6u3YTDhcZ4dOrtv') %>%
    filter(!(ego %in% c('California Ocean Protection Council','California Department of Fish and Wildlife'))),
  
  filter(many_egos_long, response_id=='R_56D6mBrAXbvzaBX') %>%
    filter(!(ego %in% c('The Nature Conservancy','California Sea Grant'))),
  
  filter(many_egos_long, response_id=='R_5dGJi242F31TvPK') %>%
    filter(!(ego %in% c('Seatrees','California Sea Grant'))),
)

## pivot wider
update_many_egos_wide <- update_many_egos %>% dplyr::select(-ego_level, -ego) %>%
  distinct() %>%
  ## first, update numbering for egos
  left_join(update_many_egos %>% dplyr::select(response_id,ego_level,ego) 
            %>% distinct() %>%
              arrange(response_id,ego_level) %>%
              group_by(response_id) %>% mutate(ego_level=paste0('q3_several_',1:n())),
            by='response_id') %>%
  ## update the multi org column
  group_by(response_id) %>% mutate(multi_org=paste0(unique(ego),collapse=',')) %>%
  ## now, pivot wider
  pivot_wider(names_from='ego_level', values_from='ego') %>%
  ## remove multi_org if there is only one ego now
  mutate(multi_org=ifelse(multi_org==q3_several_1,NA,multi_org)) %>%
  ## change 'several_1' to 'individual_1' if there is only one ego now
  mutate(q3_individual_1=ifelse(is.na(multi_org), q3_several_1,NA),
         q3_several_1=ifelse(is.na(multi_org),NA,q3_several_1))

## add an alter: California Ocean Protection Council for  R_73yGa8USaSXYR0T
update_many_egos_wide %<>% bind_rows(
  filter(update_many_egos_wide, response_id=='R_73yGa8USaSXYR0T') %>%
    slice(1) %>% mutate(alter='California Ocean Protection Council', type='q11_x'),
  ## add an alter: The Nature Conservancy, California Sea Grant for R_50v3Pt1UGJw51HP
  bind_rows(filter(update_many_egos_wide, response_id=='R_50v3Pt1UGJw51HP') %>%
              slice(1) %>% mutate(alter='The Nature Conservancy', type='q11_x'),
            filter(update_many_egos_wide, response_id=='R_50v3Pt1UGJw51HP') %>%
              slice(1) %>% mutate(alter='California Sea Grant', type='q11_x') ),
  ## add an alter: CA sea grant for R_56D6mBrAXbvzaBX
  filter(update_many_egos_wide, response_id=='R_56D6mBrAXbvzaBX') %>%
    slice(1) %>% mutate(alter='California Sea Grant', type='q11_x'),
  ## add an alter: Seatrees, CA Sea Grant for R_5dGJi242F31TvPK
  bind_rows( filter(update_many_egos_wide, response_id=='R_5dGJi242F31TvPK') %>%
               slice(1) %>% mutate(alter='California Sea Grant', type='q11_x'),
             filter(update_many_egos_wide, response_id=='R_5dGJi242F31TvPK') %>%
               slice(1) %>% mutate(alter='Seatrees', type='q11_x')  )
)

## does anyone who previously didn't have an alter, have one now?
no_alter <- filter(update_many_egos_wide, is.na(alter))
has_alter <- no_alter$response_id[which(no_alter$response_id %in% filter(update_many_egos_wide, !is.na(alter))$response_id)]

update_many_egos_wide %<>% filter(!(is.na(alter) & response_id %in% has_alter))
update_many_egos_wide %<>% arrange(response_id,type)

colnames(q3q11_out)[which(!colnames(q3q11_out) %in% colnames(update_many_egos_wide))]
colnames(update_many_egos_wide)[which(!colnames(update_many_egos_wide) %in% colnames(q3q11_out))]

## add to output data frame

q3q11_out %<>% filter(!(response_id %in% update_many_egos_wide$response_id)) %>%
  bind_rows(update_many_egos_wide)


# DISES: Check other egos -------------------------------------------------
## need to remove Santa Cruz for one individual


# Save --------------------------------------------------------------------

## did we end up removing any individuals? no!
all(q3q11$response_id %in% q3q11_out$response_id)


if(write_out){   q3q11_out %>%
    write_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_updateINDupdateORGmanEGO_4sen_',d.out,'.csv')))    }


q3q11_out %>%
  write_csv(here('../california-kelp-SEN','data','survey','confidential',paste0('processed_by_responseID_q3orgs_q11collabs_updateINDupdateORGmanEGO_4sen_',d.out,'.csv')))

