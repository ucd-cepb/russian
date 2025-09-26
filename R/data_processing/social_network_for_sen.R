############### Create social network for kelp SEN ############### 
#
# Take the output from the script `process_orgs_3.R` and make sure 
#    that organization names cross-ref each other, between 
#    egos and alters. 
# Alters are collaborators
# Collaboration ties are created between ego-alter *and* between
#    organizations with people working on behalf of multiple orgs
# Save out an edgelist and an adjacency matrix for the kelp SEN.
# The Level 1 SEN includes only those individuals who are
#    counted as "systematic direct observers" in the kelp SEN
# The Level 1+2 SEN includes collaboration ties among individuals
#    who are *not* systematic direct observers, and between
#    levels 1 and 2.
#
#
# 7/23/2025 - Mary Fisher
# Last edited: 9/25/2025
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
# d.out <- Sys.Date()
d.out <- d.in
#
# process_prefix <- 'updateINDupdateORG'
process_prefix <- 'updateINDupdateORGmanEGO'


# Data --------------------------------------------------------------------

## output from script 3: ego and alter orgs processed
# dat <- read_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_updateINDupdateONE_4sen_',d.in,'.csv')))
dat <- read_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_',process_prefix,'_4sen_',d.in,'.csv')))

## survey data - grab just response IDs, names, and emails
survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv')) %>%
  clean_names() %>% 
  slice(-c(1:2))

snames <- survey %>% select(response_id, recipient_first_name, recipient_last_name, email)
snames %<>% filter(response_id %in% dat$response_id)

## direct observers
dat_lvl <- read_csv(here('confidential_data','processed','datares_by_responseID_wCounty_2025-09-23.csv'))


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
dat %>% select(alter) %>% distinct() %>%
  write_csv(here('data','sen','levels12_alter_list.csv'))



# First pass: only respondents 'on the water' -----------------------------
# ndat <- read_csv(here('../','california-kelp-SEN','data','kelp_sites','kelp_sites_confidential','final_assignments','areas_by_responseID_DataRes_STRICT_simplified.csv'))
# sen_ids <- ndat %>% select(response_id) %>% distinct() %>% pull(response_id)
# length(unique(sen_ids)) #113
# 
# dat %<>% filter(response_id %in% sen_ids)

# Create survey respondent ID key -----------------------------------------

## check for duplicate name - id matches. grab the id for the survey that the individual *finished*
dup <- snames %>% group_by(recipient_first_name, recipient_last_name) %>% summarise(n=length(unique(response_id))) %>% filter(n>1)
dup  # one person
dup %<>% filter(!is.na(recipient_first_name)) %>%
  left_join(select(snames, recipient_first_name, recipient_last_name, response_id)) %>%
  left_join(dat)
## one person responded twice with different alters each time. clean this up.
##   use the response id associated with the finished survey, but bring in extra info from the unfinished survey. 
dup_update <- dup %>% filter(!(response_id=='R_6tLmKtUmSAbqmXf' & org_name=='University of California')) %>%
  mutate(response_id='R_6tLmKtUmSAbqmXf', 
         email=unique(dup$email[which(!is.na(dup$email))]) )
##  remove duplicated alters and re-number them.
dup_update %<>% dplyr::select(-type) %>% distinct() %>%
  group_by(response_id) %>% mutate(type=paste0('q11_',1:n())) %>%
  mutate(recipient_last_name='Pomeroy')

snames %<>% mutate(recipient_last_name=ifelse(recipient_last_name=='Pomerory', 'Pomeroy', recipient_last_name))
snames %<>% filter(!(response_id %in% dup$response_id & !(response_id %in% dup_update$response_id)))

## update dat!
dat %<>% filter(!(response_id %in% dup$response_id)) %>%
  bind_rows(dup_update %>% dplyr::select(-n))



# Social ties between survey respondents: Find ----------------------------

## break names out of alters column
datl <- dat %>% separate(alter, into=c('alter_org','alter_ind'), sep=':', remove=FALSE)
alti <- datl %>% filter(!is.na(alter_ind))
dim(datl) ; dim(alti) #267 out of 717 alters had names
length(unique(datl$alter)); length(unique(alti$alter_ind)) ## 220 unique, 99 unique names

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
length(alti %>% filter(!is.na(alter_id)) %>% pull(alter_id)) ## 117
length(unique(alti %>% filter(!is.na(alter_id)) %>% pull(alter_id))) ## 32

# Social ties between survey respondents: What to Expand? --------------------------
## link survey respondents to all organizations that individual alters 'work on behalf of.' ##

## grab all affiliations for named alters in our survey data set
alter_info <- alti %>% filter(!is.na(alter_id)) %>%
  dplyr::select(alter_id,alter_org, alter) %>% distinct() %>%
  left_join(dat %>% dplyr::select(response_id,org_name,multi_org) %>% distinct(), by=c('alter_id'='response_id'))

## how many named alters are affiliated with multiple organizations? 
sum(!is.na(alter_info$multi_org)) ## 13

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

## !!!!  for one individual, update org name to be more specific
new_ego_info <- filter(new_alter_info, alter_id %in% c('R_6D5kmxtODIPb1hT')) %>%
  dplyr::select(alter_id, alter_org) %>% rename(response_id=alter_id,org_name=alter_org) %>% mutate(q3_individual_1=org_name)
dat %<>% filter(response_id != new_ego_info$response_id) %>%
  bind_rows(dat %>% filter(response_id == new_ego_info$response_id) %>%
              dplyr::select(-colnames(new_ego_info)[2:3]) %>%
              left_join(new_ego_info, by='response_id'))

new_alter_info %<>% filter(alter_id != 'R_6D5kmxtODIPb1hT')

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

# Social ties between survey respondents: Adjust & Expand ----------------------------


## part 1: replace the alters in our input data set!
dat %<>% anti_join(new_alter_info, by='alter') %>%
  bind_rows(new_alter_info %>% left_join(dat, by='alter') %>%
              dplyr::select(-alter) %>% rename(alter=new_alter))

## part 2: expand alters for named individuals affiliated with more than one organization
expand_alter <- filter(alter_info,!is.na(multi_org)) # grab alters affiliated with 2+ orgs
expand_alter %<>% separate(multi_org,into=c('alter.1','alter.2','alter.3','alter.4'), sep=',') %>% # separate out all orgs
  pivot_longer(starts_with('alter.'), names_to='alter_level',values_to='new_alter') %>% filter(!is.na(new_alter))
expand_alter %<>% filter(alter_org != new_alter) # remove alters we already have
expand_alter %<>% filter(!(alter_id=='R_3ocCcrb6NY9Nk5I' & new_alter=='University of California Santa Cruz')) # ugh iterative fix to ego

expand_alter %<>% separate(col=alter, into=c('alter_org','alter_ind'), sep=':', remove=FALSE) %>%  # add ind names to new alters
  unite(col='new_alter', new_alter, alter_ind, sep=':', remove=TRUE)
expand_alter2 <- expand_alter %>% dplyr::select(alter,new_alter)

dat %<>% 
  bind_rows(expand_alter2 %>% left_join(dat, by='alter') %>%
              dplyr::select(-alter) %>% rename(alter=new_alter))

## ugh iterative fix to ego for dises member
dat %<>% filter(response_id != 'R_3ocCcrb6NY9Nk5I') %>%
  bind_rows(dat %>% filter(response_id == 'R_3ocCcrb6NY9Nk5I') %>%
              mutate(q3_several_2=NA,multi_org=NA,
                     q3_individual_1=q3_several_1) %>%
              mutate(q3_several_1=NA))

## Check if it worked!
dat %<>% distinct() %>% arrange(response_id,type)
View(dat) ## search for "Jon" and "Carr"



# Cross-check org names ---------------------------------------------------
## Revise organization names that don't match between alter / ego data.
## for the SEN, this includes org names that I made extra specific to help
## connect people to kelp administrative areas.

all_egos <- unique(dat %>% dplyr::select(response_id,multi_org) %>%
                     separate(multi_org, into=c('org1','org2','org3','org4','org5','org6'), sep=',') %>% 
                     pivot_longer(starts_with('org'), names_to='org_level',values_to='org_name') %>%
                     filter(!is.na(org_name)) %>%
                     bind_rows(
                       dat %>%
                         filter(!is.na(org_name) & is.na(multi_org)) %>% dplyr::select(response_id,org_name)
                     ) %>%
                     pull(org_name))

length(all_egos)

all_alters <- dat %>% dplyr::select(response_id,alter) %>%
                       separate(alter,into=c('alter','alter_ind'), sep=':')

all_alters %<>% filter(!(alter %in% c('Commercial Diver','Artist','Photographer'))) %>% bind_rows(
  all_alters %>% filter(alter %in% c('Commercial Diver','Artist','Photographer')) %>%
    unite('alter',alter, alter_ind, sep=':'))

all_alters <- unique(filter(all_alters,!is.na(alter)) %>% pull(alter))
length(all_alters)


all_egos
all_alters


## there are multiple project / lab names in the same organization.
all_orgs <- data.frame(egos_full_name=all_egos) %>%
  separate(egos_full_name, into=c('org_name','ego_subgroup'), sep=' - ',remove=FALSE) %>%
  mutate(ego=1) %>%
  bind_rows(
    data.frame(alters=all_alters) %>%
      separate(alters, into=c('org_name','alter_subgroup'), sep=' - ') %>%
      mutate(alter=1)
  ) %>%
  group_by(org_name) %>% summarise(ego_full_name=paste0(unique(egos_full_name),collapse=','),
                                   ego=sum(ego,na.rm=TRUE),alter=sum(alter,na.rm=TRUE),
                                   ego_sub=paste0(unique(ego_subgroup),collapse=','),
                                   alter_sub=paste0(unique(alter_subgroup),collapse=','))

## save this to create a manual key
write_csv(all_orgs,here('data','sen',paste0('sn_match_ego_alter_organizations_',d.out,'.csv')))
# 
# data.frame(org_name=unique(c(all_egos,all_alters))) %>%
#   write_csv(here('data','sen','sn_match_ego_alter_organizations_KEY.csv'))  ## this will overwrite existing key!!

# Create social network ---------------------------------------------------

# First Level: Data Res collabs only ------------------------------------
dat_out <- dat
dat_out %<>% filter(response_id %in% dat_lvl$response_id)

## we have to edit names first! ##

## read in names key
key <- readxl::read_excel(here('data','sen',paste0('sn_match_ego_alter_organizations_',d.out,'_KEY.xlsx')), sheet='key')
head(key)

## melt out data frame so that each 'ego' organization gets one row
dat_out %<>% 
  pivot_longer(starts_with('q3_'), names_to='ego_level',values_to='ego') %>%
  filter(!is.na(ego))

## make sure we're not missing any orgs, and that the 'org_name' column always represents the q3_1 org
any(is.na(dat_out$org_name)); any(is.na(dat_out$ego)); any(is.na(dat_out$alter))
any(filter(dat_out,ego_level=='q3_individual_1')$org_name != filter(dat_out,ego_level=='q3_individual_1')$ego) # false
any(filter(dat_out,ego_level=='q3_several_1')$org_name != filter(dat_out,ego_level=='q3_several_1')$ego) #false

# apply re-naming to 'dat' dataframe. make sure we get all orgs for multi org respondents
dat_out %<>% dplyr::select(-org_name) %>%
  separate(alter, into=c('alter','alter_ind'), sep=':') %>%
  ## we need to keep names attached to artists, divers named as individuals
  mutate(alter=ifelse(alter %in% c('Commercial Diver','Artist'), paste0(alter, ':', alter_ind),alter)) %>%
  ## new names for egos
  left_join(key, by=c('ego'='org_name')) %>%
  ## new names for alters
  left_join(key %>% rename(alter=org_name, sn_alter=sn_org_name,sn_alter_scale=sn_org_scale,sn_alter_type=sn_org_type), by=c('alter'))



## missing an ego name? these are *all individuals*
any(is.na(dat_out$sn_org_name))
View(dat_out %>% filter(is.na(sn_org_name)))

## rename individual according to last 4 digits of response ID
##    except for the person who listed their 'individual' status as secondary to their organizational affiliation.
dat_out %<>% filter(!is.na(sn_org_name)) %>%
  bind_rows(dat_out %>%
              filter(is.na(sn_org_name)) %>%
              filter(ego_level != 'q3_several_2') %>%
              mutate(ego=paste0('Individual:', str_sub(response_id, -4,-1))) %>%
              mutate(sn_org_name=ego,sn_org_scale='Individual',sn_org_type='Individual')
  )

## missing a corrected alter name?
any(is.na(filter(dat_out, !is.na(alter))$sn_alter))
# View(filter(dat_out, !is.na(alter) & is.na(sn_alter)))

## save as data frame
write_csv(dat_out, here('confidential_data','processed',paste0('sn_datares_',process_prefix, '_',d.out,'_4sen.csv')))


# Edge list and Matrix ----------------------------------------------------

## first, do collabs
el_out <- dat_out %>% group_by(sn_org_name,sn_org_scale,sn_org_type,sn_alter,sn_alter_scale,sn_alter_type) %>%
  summarise(r=length(unique(response_id)))
## remove self-ties
el_out %<>% filter(sn_org_name != sn_alter)


expand_unique <- function(x){
  x <- x[,2,drop=TRUE]
 tmp <- matrix(data=NA,nrow=length(x),ncol=length(x),dimnames=list(x,x))
 k <- arrayInd(which(upper.tri(tmp)), dim(tmp))
 out <- data.frame(org_name=rownames(tmp)[k[,1]], alter=colnames(tmp)[k[,2]])
 return(out)
}

## next, do shared individuals (i.e., wherever two orgs were listed by a single individual)
el_add <- dat_out %>% filter(!is.na(multi_org))
## grab just the affiliations, no alters 
el_add %<>% dplyr::select(response_id,ego_level,ego) %>% distinct()
## next by response ID, use custom function to get all pairs *without inverse duplicates*
el_add %<>% group_by(response_id) %>% nest() %>%
  mutate(pairs=map(data, ~expand_unique(.x)))

## unnest to get new ego x alter matches (and the number of individuals affiliated with both of the pair)
el_add %<>% dplyr::select(-data) %>% unnest(c(response_id,pairs))
el_add %<>% group_by(org_name,alter) %>% summarise(r=length(unique(response_id)))

## bring in corrected names from key
el_add %<>% left_join(key, by='org_name') %>%
  left_join(key %>% rename(sn_alter=sn_org_name, sn_alter_scale=sn_org_scale, sn_alter_type=sn_org_type), by=c('alter'='org_name'))
## fix NAs, which are individuals
el_add %<>% filter(!is.na(sn_alter)) %>%
  bind_rows(el_add %>% filter(is.na(sn_alter)) %>%
              mutate(sn_alter=alter,sn_alter_scale='Individual',sn_alter_type='Individual'))

all(colnames(el_add) %in% colnames(el_out))

el_out %<>% bind_rows(el_add %>% ungroup() %>% dplyr::select(-org_name,-alter))

## look up one multiple affiliate - THERE MAY BE INVERSE DUPLICATES IN THIS EDGE LIST. 
filter(el_out, sn_org_name==el_out$sn_org_name[1])

## temporary save so we don't lose work
write_csv(el_out, here('data','sen',paste0('sn_datares_collab_multi_',process_prefix,'_',d.out,'_4sen.csv')))

# clean up duplicates in edge list ----------------------------------------

## turn into graph
## simplify
## have igraph put out edge list
## re-attach names from key




  

## as matrix
mat_out <- dat_out %>% filter(!is.na(sn_alter)) %>%
  group_by(sn_org_name,sn_alter) %>%
  summarise(r=length(unique(response_id))) %>%
  #zeros v. missing data
  pivot_wider(id_cols=sn_org_name,names_from=sn_alter,values_from=r) %>%
  pivot_longer(cols=2:(length(unique(dat_out$sn_alter))),names_to='sn_alter',values_to='r') %>%
  mutate(r=ifelse(is.na(r) & sn_alter %in% dat_out$sn_org_name,0,r)) %>%
  pivot_wider(id_cols=sn_org_name,names_from=sn_alter,values_from=r) %>%
  column_to_rownames('sn_org_name')

sum(mat_out==0,na.rm=TRUE)

#save
write_csv(mat_out, here('data','sen',paste0('sn_datares_STRICT_',Sys.Date(),'_matrix_4sen.csv')))

# Second Pass: all collaborations -----------------------------------------





