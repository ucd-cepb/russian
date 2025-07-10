############### Process survey data on ego organization ############### 
#
# Adjust processed social network data frame for richer info on 
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


## custom fxn
clean_org <- function(x) {
  x <- as.character(x)
  return(
    case_when(
      grepl(paste(c('Humboldt','NEREO','Student assistant with Sean Craig'),collapse="|"), x) ~ "California State Polytechnic University Humboldt",
      grepl(paste(c('Reef check','reef check','reefcheck','Reef Check','ReefCheck',"Reefcheck CA"),collapse="|"), x) ~ "Reef Check",
      grepl(paste(c('moss landing','Moss Landing','MLML','Moss landing',"Moss Landing Marine Labs","Moss landing marine labs"),collapse="|"), x) ~ "Moss Landing Marine Laboratories",
      grepl(paste(c("Sunflower Sea Star Laboratory","Sunflower Star Lab","https://www.sunflowerstarlab.org/"),collapse="|"),x) ~ "Moss Landing Marine Laboratories - Sunflower Star Laboratory",
      x=="Moss Landing Marine Labs Aquaculture Center" ~ "Moss Landing Marine Laboratory - Aquaculture Center",
      grepl(paste(c('Giant Giant Kelp','g2kr','giant giant kelp','G2KR','Great Great Kelp',
                    'UrchinsKelpOtters',"Giant Kelp Project","Giant Giant Kelp Restoration Project",
                    "Giant Giant Kelp Restoration Project (G2KR)",
                    "Giant kelp restoration project - g2kr",
                    "Great Great Kelp Restoration"),collapse="|"), x) ~ "Giant Giant Kelp Forest Restoration Project",
      grepl(paste(c('UCSC','UC Santa Cruz'),collapse="|"), x) ~ "University of California Santa Cruz",
      grepl(paste(c('CA Santa Barbara','California, Santa Barbara',"UC Santa Barbara"),collapse="|"),x) ~ "University of California Santa Barbara",
      grepl(paste(c("Bodega marine labs","Bodega Marine Laboratory"),collapse="|"), x) ~ "University of California Davis - Bodega Marine Laboratory",
      grepl(paste(c('UC Davis','Uc Davis','Uc davis','California Davis','California, Davis'),collapse="|"), x) ~ "University of California Davis",
      grepl(paste(c('UC Berkeley','UCB','Uc berkeley'),collapse="|"), x) ~ "University of California Berkeley",
      grepl('U.S. Geological Survey', x) ~ "US Geological Survey",
      grepl(paste(c('CDFW','CALIFORNIA DEPARTMENT OF FISH AND WILDLIFE', 
                    'CA Dept of Fish and Wildlife',
                    'California Department of Fish and Wildlife (working with opc as our contractee)',
                    'Fish and Wildlife'),collapse="|"), x) ~ "California Department of Fish and Wildlife",
      grepl('Sea Urchin Commission', x) ~ "California Sea Urchin Commission",
      grepl(paste(c('Earth equty', 'Earth equity'),collapse="|"), x) ~ "Earth Equity",
      grepl('Cal Poly Pomona', x) ~ "California State Polytechnic University Pomona",
      grepl('USFWS', x) ~ "US Fish and Wildlife Service",
      grepl(paste(c('CA Ocean Protection Council','California Ocean Protection Council','Calif Ocean Protection Council'),collapse='|'),x) ~ 'California Ocean Protection Council',
      x=='California Ocean Protection Council (contracted by them)' ~ 'California Ocean Protection Council',
      x=='UC San Diego' ~ "University of California San Diego", 
      grepl(paste(c("Watermens Alliance","Watermans Alliance","Waterman's Alliance"),collapse="|"), x) ~ "Watermen's Alliance",
      grepl(paste(c("GFNMS Advisory Council","Greater Farralones"),collapse="|"), x) ~ 'Greater Farallones National Marine Sanctuary',
      x=="Ocean Rainforest Inc" ~ 'Ocean Rainforest',
      x=="Coastal Stewardship Task Force, The Sea Ranch Association" ~ "The Sea Ranch Association: Coastal Stewardship Task Force",
      grepl(paste(c("Nature Conservancy","TNC","The Nature Conservancy CA"),collapse='|'),x) ~ "The Nature Conservancy",
      x=="Vantuna Research Group at Occidental College" ~ "Occidental College - Vantuna Research Group",
      x=="Sherwood Valley Rancheria" ~ "Sherwood Valley Band of Pomo Indians",
      x=="Fish Reef Project (fishreef.org)"~"Fish Reef Project",
      x=="The Greater Farallones Association" ~ "Greater Farallones Association",
      grepl(paste(c("KRMP","KRMP community workgroup (CA DFW)"),collapse='|'),x) ~ "Kelp Restoration and Management Plan Community Working Group",
      x=="CDFW Red Abalone Recovery Working Group" ~ "Red Abalone Recovery Working Group",
      x=="Monterey Surfrider Chapter" ~ "Surfrider Foundation - Surfrider Monterey",
      x=="Humboldt Surfrider Foundation Humboldt Chapter" ~ "Surfrider Foundation - Surfrider Humboldt",
      x=="MPA Long Term Monitoring Program" ~ "California MPA Monitoring Program",
      x=="WaveWalker Charters" ~ "Wave Walker Charters",
      x=="West Coast Region, Office of National Marine Sanctuaries" ~ "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries - West Coast Region",
      grepl(paste(c("CA Sea Grant","CA Seagrant"),collapse='|'),x) ~ "California Sea Grant",
      grepl("PISCO",x) ~ "Partnership for Interdisciplinary Studies of Coastal Oceans",
      x=="The Kelp Forest Alliance" ~ "Kelp Forest Alliance",
      x=="SBC LTER" ~ "University of California Santa Barbara - SBC LTER",
      x=="MBNMS" ~ "Monterey Bay National Marine Sanctuary",
      x=="Scripps Institution of Oceanography" ~ "University of California San Diego - Scripps Institution of Oceanography",
      x=="Birch Aquarium" ~  "University of California San Diego - Scripps Institution of Oceanography - Birch Aquarium",
      x=="Monterey Abalone Co." ~ "Monterey Abalone Company",
      x=="Bamboo Reef Scuba Diving Centers" ~ "Bamboo Reef Scuba Diving Centers - Triton Spearfishing",
      x=='Urchin removal diver' ~ NA,
      x=="Stanford University (past)" ~ NA,
      x=="co-PI Santa Barbara Coastal LTER site" ~ "University of California Santa Barbara - SBC LTER",
      x=="PI - Seaweed CDR Project at UCSB" ~ "University of California Santa Barbara - Seaweed CDR Project",
      x=="Stillwater Cove urchin divers" ~ "Stillwater Cove urchin divers",
      grepl(paste(c("Volunteer Diving groups","North Coast KelpFest!","Independent filmmaking","x",
                    "Triton Spearfishing",  # merged with _2
                    "ANCKR", # merged with _1
                    "CPC"    # name?
      ),collapse='|'),x) ~ NA, 
      x=="Greater Farallones and Cordell Bank National Marine Sanctuaries" ~ "Greater Farallones National Marine Sanctuary, Cordell Bank National Marine Sanctuary",
      TRUE ~ x
    ))
}
#



# Data --------------------------------------------------------------------

## this is the survey data
dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
  clean_names() %>%
  slice(-c(1:2))

colnames(dat_survey)

## this is the cleaned up social network
sn <- read_csv(here('confidential_data', 'processed', 'cleaned_social_network_with_org_2025-03-14.csv'))


## FOR FIRST PASS 3/17/2025:
##   this is the cleaned up data set on the answer to the question: What are the main ways you learn about kelp forest-related issues?
info <- read_csv(here('confidential_data', 'processed','cleaned_responseID_by_info_source_q9.csv'))

## helpful Qs in the survey data
qs_of_interest <- make_clean_names(c('ResponseId','Status',"RecipientLastName","RecipientFirstName",
                                     "Q3 Individual_1",
                                     colnames(dat_survey)[grepl('Q4',colnames(dat_survey))],
                                     colnames(dat_survey)[grepl('Q9',colnames(dat_survey))]))



# Identify & Remove affiliations that are collaborations -----------------------------

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


