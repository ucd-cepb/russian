############### Process survey data for alters covered by survey ############### 
#
# Adjust processed social network data frame for alters who were
#   also survey respondents. Anyone who named a 'covered' alter
#   is linked to *all* of that alter's self-affiliates. 
# Then, remove the individual names from the alter column, and re-do
#   the filtering to remove alters that are also listed as ego 
#   organizations. 
#   This is necessary for getting accurate edge weights (counts of survey
#   respondents) for the SEN. 
#
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
source(here('R/subfxn/expand_org_names.R'))
#
d.in <- '2025-10-01'
d.out <- d.in
#
write_out <- TRUE
#
# process_prefix <- 'updateINDupdateORG'
process_prefix <- 'updateINDupdateORGmanEGO'


# Data --------------------------------------------------------------------
## this is the data frame from process_orgs_3
dat <- read_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_',process_prefix,'_4sen_',d.out,'.csv')))

## this is the survey data
dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
  clean_names() %>%
  slice(-c(1:2))
dat_survey %<>% filter(as.numeric(progress) > 75)
dim(dat_survey)  # 190

dat$response_id[which(!(dat$response_id %in% dat_survey$response_id))] # zero!



# Create survey respondent ID key -----------------------------------------

snames <- dat_survey %>% select(response_id, recipient_first_name, recipient_last_name, email)
snames %<>% filter(response_id %in% dat$response_id)

##  [[  save out surveys that have emails but no name. first run of script  ]]
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


## check for duplicate name - id matches. grab the id for the survey that the individual *finished*
dup <- snames %>% group_by(recipient_first_name, recipient_last_name) %>% summarise(n=length(unique(response_id))) %>% filter(n>1)
dup  # none! one is now fixed in process_orgs_3


# Social ties between survey respondents: Find ----------------------------

## break names out of alters column
datl <- dat %>% separate(alter, into=c('alter_org','alter_ind'), sep=':', remove=FALSE)
alti <- datl %>% filter(!is.na(alter_ind))
dim(datl) ; dim(alti) #226 out of 607 alters had names
length(unique(datl$alter)); length(unique(alti$alter_ind)) ## 219 unique, 99 unique names

alti %<>% select(response_id,alter,alter_org,alter_ind) %>% 
  mutate(alter_ind=str_trim(alter_ind)) %>%
  separate(alter_ind, into=c('alter_first_name','alter_last_name'), sep=' ')

## match!  (with all lowercase to avoid capitalization differences) 
alti %<>% 
  mutate_at(c('alter_first_name','alter_last_name'), str_to_lower) %>%
  left_join(snames %>%
              mutate(recipient_last_name=ifelse(recipient_last_name=='Basket','Baskett',recipient_last_name)) %>%
              mutate_at(c('recipient_first_name','recipient_last_name'), str_to_lower) %>%
              rename(alter_id=response_id), by=c('alter_first_name'='recipient_first_name','alter_last_name'='recipient_last_name'))

View(alti %>% dplyr::select(alter_first_name,alter_last_name,alter_id) %>% distinct())

## revisit match: hyphenated / composite last names, shortened first names
alti2 <- alti %>% filter(alter_last_name %in% c('murphy-cannella','giraldo','cuevas','arroyo') |
                           alter_first_name %in% c('josh','matthew','dan','mike','jennifer'))
alti2 %<>% mutate(alter_last_name=case_when(
  alter_last_name=='murphy-cannella' ~ 'murphy',
  alter_last_name=='giraldo' ~ 'giraldo ospina',
  alter_last_name=='cuevas' ~ 'cuevas uribe',
  alter_last_name=='arroyo' ~ 'arroyo esquivel',
  .default=alter_last_name
)) %>%
  mutate(alter_first_name=case_when(
    alter_first_name=='josh' ~ 'joshua',
    alter_first_name=='matthew' ~ 'matt',
    alter_first_name=='dan' ~ 'daniel',
    alter_first_name=='mike' ~ 'michael',
    alter_first_name=='jennifer' ~ 'jenn',
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

## how many are in our data set?
length(alti %>% filter(!is.na(alter_id)) %>% pull(alter_id)) ## 87
length(unique(alti %>% filter(!is.na(alter_id)) %>% pull(alter_id))) ## 30

# Social ties between survey respondents: What to Expand? --------------------------
## link survey respondents to all organizations that individual alters 'work on behalf of.' ##

## grab all affiliations for named alters in our survey data set
alter_info <- alti %>% filter(!is.na(alter_id)) %>%
  dplyr::select(alter_id,alter_org, alter) %>% distinct() %>%
  left_join(dat %>% dplyr::select(response_id,org_name,multi_org) %>% distinct(), by=c('alter_id'='response_id'))

## how many named alters are affiliated with multiple organizations? 
sum(!is.na(alter_info$multi_org)) ## 10



# Social ties between survey respondents: Expand Part 1: Alter org != Alter's Ego org ----------------------------------------

## how many alters have a primary organization that doesn't match the alter info?
length(filter(alter_info, alter_org != org_name) %>% pull(alter_id)) ## 6. three are different naming for the same org, my use of projects within orgs.
View(filter(alter_info, alter_org != org_name))

## create a new data frame to expand on alter org info. data frame has: orig_alter | alter
new_alter_info <- alter_info %>% filter(alter_org != org_name & is.na(multi_org))

## for name alters who we've affiliated with specific projects, add project name into alter name
new_alter_info %<>% mutate(new_alter=NA) %>%
  filter(!grepl(' - ', org_name)) %>%
  bind_rows(new_alter_info %>% filter(grepl(' - ', org_name)) %>%
              separate(col=alter, into=c('alter_org','alter_ind'), sep=':', remove=FALSE) %>%
              unite('new_alter', org_name,alter_ind,sep=':', remove=FALSE) %>%
              dplyr::select(colnames(new_alter_info), new_alter)
  )

## for one individual, adjust alter information to match self-identified affiliation. 
new_alter_info %<>% filter(!grepl("Tolowa", alter)) %>% bind_rows( 
  new_alter_info %>% filter(grepl("Tolowa", alter)) %>%
    separate(col=alter, into=c('alter_org','alter_ind'), sep=':', remove=FALSE) %>%
    unite('new_alter', org_name,alter_ind,sep=':', remove=FALSE) %>%
    dplyr::select(colnames(new_alter_info), new_alter)
)


## for remaining two individuals, add to alter orgs based on the alter's self-identified affiliation
new_alter_info %<>%
  bind_rows(
    new_alter_info %>% filter(is.na(new_alter)) %>%
      separate(col=alter, into=c('alter_org','alter_ind'), sep=':', remove=FALSE) %>%
      dplyr::select(-new_alter) %>%
      unite(col='new_alter', org_name, alter_ind, sep=':', remove=FALSE)
  )
new_alter_info %<>% mutate(new_alter=ifelse(is.na(new_alter), alter,new_alter))

## thin out the data frame
new_alter_info %<>% dplyr::select(alter,new_alter)

## replace the alters in our input data set!
dat %<>% anti_join(new_alter_info, by='alter') %>%
  bind_rows(new_alter_info %>% left_join(dat, by='alter') %>%
              dplyr::select(-alter) %>% rename(alter=new_alter))



# Social ties between survey respondents: Expand part 2: Add alter orgs for multiple affiliations --------------------------

## part 2: expand alters for named individuals affiliated with more than one organization
expand_alter <- filter(alter_info,!is.na(multi_org)) # grab alters affiliated with 2+ orgs
expand_alter %<>% separate(multi_org,into=c('alter.1','alter.2','alter.3','alter.4'), sep=',') %>% # separate out all orgs
  pivot_longer(starts_with('alter.'), names_to='alter_level',values_to='new_alter') %>% filter(!is.na(new_alter))
expand_alter %<>% filter(alter_org != new_alter) # remove alters we already have

expand_alter %<>% separate(col=alter, into=c('alter_org','alter_ind'), sep=':', remove=FALSE) %>%  # add ind names to new alters
  unite(col='new_alter', new_alter, alter_ind, sep=':', remove=TRUE)
expand_alter2 <- expand_alter %>% dplyr::select(alter,new_alter)

dat %<>% 
  bind_rows(expand_alter2 %>% left_join(dat, by='alter') %>%
              dplyr::select(-alter) %>% rename(alter=new_alter))


## Check if it worked!
dat %<>% distinct() %>% arrange(response_id,type)
View(dat) ## search for "Jon" and "Bush"



# Remove alters that match egos -------------------------------------------

## now that we've used individual names to adjust the social network data set, 
##  remove them from the alter column
##  EXCEPT for individual divers, artists. 
##  then re-check to make sure that organizations listed as egos aren't also 
##  listed as alters. 
## this doesn't remove ties from the SEN's social network, because 'collaboration' ties
## include shared personnel.

dat_out <- dat

dat_out %<>% separate(alter, into=c('alter','alter_ind'),sep=':')

## assign numbers to alters named as individuals
dat_out %<>% filter(!alter %in% c('Commercial Diver','Artist','Photographer')) %>%
  bind_rows(
    filter(dat_out, alter %in% c('Commercial Diver','Artist','Photographer')) %>%
  group_by(alter) %>% mutate(alter=paste0(alter,' ',seq(1:n()))) %>%
    ungroup()
  ) %>% arrange(response_id,alter)

dat_out %<>% dplyr::select(-alter_ind)

# ONE EGO ORG: Exact Ego-Alter match ----------------------------------

## Much of this is the same as in process_orgs_3
orgdat1 <- filter(dat_out, !is.na(q3_individual_1) & !grepl('Individual',q3_individual_1))
orgdat1 %<>% dplyr::select(-starts_with('q3'))
orgdat1 %<>% mutate(alter=ifelse(grepl("Kelp Restoration as an Integrated Socio-Ecological System",alter), "University of California Davis", alter))

## exact matches
View(orgdat1 %>% filter(org_name==alter))

## filter for exact matches
filtdat1 <- orgdat1 %>% filter(org_name==alter) %>%
  group_by(response_id) %>%
  summarise(rc_or_g2kr=ifelse(unique(org_name) %in% c("Reef Check","Giant Giant Kelp Restoration Project"), 1, 0)) %>%
  left_join(orgdat1, by='response_id') %>%
  group_by(response_id, rc_or_g2kr) %>%
  summarise(n_alter=length(unique(alter))) %>%
  mutate(q3='one',
         category='exact match') 

## create corrected data frame, only for those who have an alter other than the one that matches their ego org
##    and they have more than one alter listed.
tofix <- filtdat1 %>% filter(n_alter > 1) %>%
  dplyr::select(response_id) %>% distinct() %>% left_join(orgdat1)

update1 <- filter(tofix, org_name!=alter)

## get distinct alters, renumber them. 
update1 %<>% dplyr::select(-type) %>% distinct()
update1 %<>% 
  group_by(response_id) %>%
  mutate(type=paste0('q11_',seq(1:n())))
update1 %<>% ungroup()

## replace data in output data frame
dat_out %<>% filter(!(response_id %in% update1$response_id)) %>%
  bind_rows(
    update1 %>% left_join(dat_out %>% dplyr::select(response_id,starts_with('q3')) %>% distinct(), by='response_id')
  )

## save work to here!!
if(write_out){   dat_out %>%
    write_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_',process_prefix,'2_4sen_',d.out,'.csv')))    }



# 2+ ORGS: Exact Ego-Alter Match ------------------------------------------

## grab respondents who work on behalf of multiple orgs
orgdat2 <- filter(dat_out, !is.na(q3_several_1)) %>%
  select(response_id) %>% distinct() %>% left_join(dat_out)
orgdat2_n <- length(unique(orgdat2$response_id))  # 50

orgdat2 %<>% pivot_longer(starts_with('q3'), names_to='ego_level',values_to='ego') %>% filter(!is.na(ego))

## exact matches
View(orgdat2 %>% filter(ego==alter) %>% 
       mutate(category=ifelse(ego_level=='q3_several_1', 'exact-first ego','exact-other ego')))


## identify exact matches
filtdat2a <- orgdat2 %>% filter(ego==alter) %>%
  mutate(category=ifelse(ego_level=='q3_several_1', 'exact-first ego','exact-other ego')) 

filtdat2 <- filtdat2a %>%
  group_by(response_id) %>% summarise(category=paste0(unique(category),collapse=','), n_ego=length(unique(ego))) %>%
  left_join(orgdat2,by='response_id') %>%
  dplyr::select(response_id,n_ego, category,alter) %>% distinct() %>%
  ## count up alters that are not egos
  left_join(filtdat2a %>% dplyr::select(response_id,alter) %>% distinct() %>% mutate(alter_an_ego=1), by=c('response_id','alter')) %>%
  group_by(response_id, n_ego, category) %>%
  summarise(n_alter=length(unique(alter)),
            n_alter_an_ego=sum(alter_an_ego,na.rm=TRUE)) 
with(filtdat2, table(category))  ## 18 exact first, 12 exact other


## there are more alters than the ones that are also egos: remove all alters ##
tofix_alter1 <- filter(filtdat2, n_alter > n_alter_an_ego) %>%
  select(response_id) %>% distinct() %>% left_join(orgdat2) %>% distinct()
length(unique(tofix_alter1$response_id)) #11

dim(tofix_alter1)

update_alter1 <- tofix_alter1 %>% 
  ## identify which alters match to egos using the 'category' column from filtdat2a
  left_join(dplyr::select(filtdat2a,response_id,alter,category) %>% distinct(), by=c('response_id','alter')) %>%
  filter(is.na(category))

dim(update_alter1) ## 147 to 108 rows
update_alter1 %>% filter(ego==alter) ## should be 0!

## get distinct alters, renumber them. 
update_alter1 %<>% pivot_wider(names_from=ego_level, values_from=ego) %>% 
  dplyr::select(-type) %>% distinct() %>% 
  group_by(response_id) %>%
  mutate(type=paste0('q11_',seq(1:n())))

## remove extra columns. 
update_alter1 %<>% ungroup() %>% dplyr::select(colnames(dat_out)[which(colnames(dat_out) %in% colnames(update_alter1))])
## check alter renumbering
# update_alter1

## replace data in output data frame
dat_out %<>% filter(!(response_id %in% update_alter1$response_id)) %>%
  bind_rows(
    update_alter1
  )

## all alters are egos: remove all but first alter (to keep in network) ##
tofix_alter2 <- filter(filtdat2, n_alter==n_alter_an_ego) %>%
  select(response_id) %>% distinct() %>% left_join(orgdat2)
length(unique(tofix_alter2$response_id)) #10


update_alter2 <- tofix_alter2 %>% 
  ## identify which alters match to egos using the 'category' column from filtdat2a
  left_join(dplyr::select(filtdat2a,response_id,alter,category) %>% distinct(), by=c('response_id','alter')) %>%
  filter(category=='exact-first ego')

dim(update_alter2); dim(tofix_alter2) ## 49 to 23

## make sure we didn't lose any respondents
all(update_alter2$response_id %in% tofix_alter2$response_id)

## get distinct alters, renumber them. 
update_alter2 %<>% pivot_wider(names_from=ego_level, values_from=ego) %>% 
  dplyr::select(-type) %>% distinct() %>% 
  group_by(response_id) %>%
  mutate(type=paste0('q11_',seq(1:n())))

## remove extra columns. 
update_alter2 %<>% ungroup() %>% dplyr::select(colnames(dat_out)[which(colnames(dat_out) %in% colnames(update_alter1))])



## replace data in output data frame
dat_out %<>% filter(!(response_id %in% update_alter2$response_id)) %>%
  bind_rows(
    update_alter2
  )


## who is left? 
filtdat2 %<>% filter(!response_id %in% c(update_alter1$response_id, update_alter2$response_id))
filtdat2


# 2+ ORGS: Duplicate Alters ------------------------------------------
## get duplicated alters (happened because we removed individual names)
filtdat3 <- dat_out %>% group_by(response_id,alter) %>% mutate(n=n()) %>%
  filter(n>1)

## how many rows in the original data for this group of respondents?
update_alter3 <- dat_out %>% filter(response_id %in% filtdat3$response_id) 
dim(update_alter3) #102

## remove duplicates, renumber alter 'type' column
update_alter3 %<>% dplyr::select(-type) %>% distinct() %>%
  group_by(response_id) %>%
  mutate(type=paste0('q11_',seq(1:n()))) %>% ungroup()

dim(update_alter3) # 76

## replace data in output data frame
dat_out %<>% filter(!(response_id %in% update_alter3$response_id)) %>%
  bind_rows(
    update_alter3
  )


# 2+ ORGS: Similar Ego-Alter ------------------------------------------
## for individuals with more than one alter, is the alter name contained within the ego name?
##   checking this because egos can contain project names (e.g., Cal Poly Humboldt "kelp culture" v "nereo")
##   while alters may not. 
filtdat4 <- dat_out %>%
  pivot_longer(starts_with('q3'), names_to='ego_level',values_to='ego') %>% filter(!is.na(ego)) %>%
    group_by(response_id) %>% mutate(n_alter=length(unique(alter))) %>%
    ## we don't want to remove these alters if they're the only one attached to the respondent. 
    filter(n_alter>1) %>% ungroup() %>%
  rowwise() %>%
  filter(grepl(alter,ego))

## exclude MLML v. MLML - SSL
filtdat4 %<>% filter(ego != 'Moss Landing Marine Laboratories - Sunflower Star Laboratory')
length(unique(filtdat4$response_id))

## investigate Cal Poly Humboldt by bringing back any individual names
View( dat %>% filter(response_id %in% filtdat4$response_id) %>%
  dplyr::select(response_id,alter) %>% separate(alter, into=c('alter','alter_ind'),sep=':') %>%
  distinct() %>% filter(alter %in% filtdat4$alter) )

##still remove: R_3uHTLrh3ea49nmF (non-specific); R_1LY9bZGoDJpKr8U (in same group); R_59T9thuNU6T04lX (non-specific);
## R_5rju7W12HtPajKX (non-specific and duplicated?); R_7PSlOsSbZJnSQVt (non-specific)

## how many rows in the original data for this group of respondents?
update_alter4 <- dat_out %>% filter(response_id %in% filtdat4$response_id) 
dim(update_alter4) #45

## remove duplicates, renumber alter 'type' column
update_alter4 %<>% anti_join(filtdat4) %>%
  dplyr::select(-type) %>% distinct() %>%
  group_by(response_id) %>%
  mutate(type=paste0('q11_',seq(1:n()))) %>% ungroup()
dim(update_alter4) #39

## replace data in output data frame
dat_out %<>% filter(!(response_id %in% update_alter4$response_id)) %>%
  bind_rows(
    update_alter4
  )



# SAVE --------------------------------------------------------------------

if(write_out){   dat_out %>%
    write_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_',process_prefix,'2_4sen_',d.out,'.csv')))    }

if(write_out){   dat_out %>% dplyr::select(-starts_with('recipient'), -email) %>%
    write_csv(here('data','sen',paste0('processed_by_responseID_q3orgs_q11collabs_',process_prefix,'2_4sen_',d.out,'.csv')))    }
