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



# FIRST PASS: select direct observers. ------------------------------------

## grab individuals who directly observe conditions
do <- unique(info %>%
  filter(info_type == 'I directly observe conditions') %>%
  pull(response_id))

## how many are not in the social network?
length(which(!(do %in% sn$response_id))) # 24

noSN_dat <- dat_survey %>% filter(response_id %in% do & !(response_id %in% sn$response_id)) %>% filter(finished=="True")

with(noSN_dat, table(q2))
# No, I am involved on my own       Yes, I am involved on behalf of one organization or group 
# 2                                                              11 
# Yes, I am involved on behalf of several organizations or groups 
# 4 



# FIRST PASS: add direct observers to SN data frame --------------------------

## No, I am involved on my own
tmp_add <- filter(noSN_dat, q2=='No, I am involved on my own')
sn2 <- sn %>% bind_rows(data.frame(response_id=tmp_add$response_id) %>%
                             mutate(num_id=-1,
                                    org_name="Individual",
                                    type=NA,
                                    alter=NA,
                                    edge_type=NA))
dim(sn) #995
dim(sn2) #997


## Yes, I am involved on behalf of one organization or group
tmp_add <- filter(noSN_dat, q2=='Yes, I am involved on behalf of one organization or group') %>%
  dplyr::select(response_id,q3_individual_1)
### fix up names
tmp_add %<>% mutate(org_name=case_when(q3_individual_1 %in% c("CDFW", "CA Dept of Fish and Wildlife") ~ "California Department of Fish and Wildlife",
                                       q3_individual_1=="UC Santa Barbara" ~ "University of California Santa Barbara",
                                       q3_individual_1=="UCSC" ~ "University of California Santa Cruz",
                                       q3_individual_1=="Reef check" ~ "Reef Check",
                                       q3_individual_1=="SBC LTER" ~ "University of California Santa Barbara - SBC LTER",
                                       q3_individual_1=="Moss Landing Marine Labs Aquaculture Center" ~ "Moss Landing Marine Laboratory - Aquaculture Center",
                                       q3_individual_1=="MBNMS" ~ "Monterey Bay National Marine Sanctuary",
                                       .default=q3_individual_1
))
### one person didn't provide an organization name, so add that in.
# View(filter(dat_survey, response_id=='R_1ulIyefXrmNqx1f'))
tmp_add %<>% mutate(org_name=ifelse(response_id=='R_1ulIyefXrmNqx1f', "California Sea Grant", org_name))

sn2 %<>% bind_rows(data.frame(response_id=tmp_add$response_id,
                             org_name=tmp_add$org_name) %>%
                    mutate(num_id=-1,
                           type=NA,
                           alter=NA,
                           edge_type=NA))
dim(sn2) #1008


## Yes, I am involved on behalf of several organizations or groups
do_add <- filter(noSN_dat, q2=='Yes, I am involved on behalf of several organizations or groups') %>%
  dplyr::select(response_id,starts_with('q3')) %>% dplyr::select(-q3_individual_1)
### melt
do_add %<>% pivot_longer(starts_with('q3'), names_to='tmp', values_to='org_name') %>% 
  filter(!is.na(org_name))
### one of the MLML responses is a duplicate
do_add %<>% filter(org_name != "Moss Landing Marine Labs")
### adjust names
do_add %<>% mutate(multi_orgs = case_when(org_name=="Reef check" ~ "Reef Check",
                                          org_name %in% c("CA Sea Grant","CA Seagrant") ~ "California Sea Grant",
                                          org_name=="Sunflower Star Lab" ~ "Moss Landing Marine Laboratory - Sunflower Star Lab",
                                          org_name=="Scripps Institution of Oceanography" ~ "University of California San Diego - Scripps Institution of Oceanography",
                                          org_name=="Birch Aquarium" ~  "University of California San Diego - Scripps Institution of Oceanography - Birch Aquarium",
                                          org_name=="Monterey Abalone Co." ~ "Monterey Abalone Company",
                                          .default=org_name))
### merge to one column, in order
do_add %<>% arrange(response_id, tmp) %>% group_by(response_id) %>%
  ## to make sure it worked in order
  mutate(multi_orgs=paste0(multi_orgs,collapse=', '))
### get unique answers and address one particular survey response:
# R_3REDy37W41gd9F7 q3_several_1 I am a commercial Nearshore Rockfish Fisherman
# R_3REDy37W41gd9F7 q3_several_2 I was an Abalone Diver                        
# R_3REDy37W41gd9F7 q3_several_3 I am a harbor Commissioner  
do_add %<>% dplyr::select(response_id,multi_orgs) %>% distinct() %>%
  mutate(multi_orgs=ifelse(response_id=="R_3REDy37W41gd9F7", "Noyo Harbor Commission", multi_orgs))  ## save this for next section


# Get info on multiple orgs -------------------------------------------------------

## First cut of the social network data just grabbed the first org listed
##  Create a new column 'multi_orgs'

multis_id <- dat_survey %>% filter(q2=='Yes, I am involved on behalf of several organizations or groups') %>%
  dplyr::select(response_id,starts_with('q3')) %>% 
  # remove individual
  dplyr::select(-q3_individual_1)

## replace the first organization with our cleaned up org names from the script `process_social_network.r`
multis <- multis_id %>%
  left_join(dplyr::select(sn, response_id, org_name) %>% distinct(), by='response_id')

## remove the "direct observer" individuals that we fixed in the section above
multis %<>% filter(!(response_id %in% do_add$response_id))

## any remaining NAs?
multis[which(is.na(multis$org_name)),]

## remove any who didn't finish the survey
to_rmv <- multis %>% filter(is.na(org_name)) %>% left_join(dat_survey) %>% filter(finished=="False") %>% dplyr::select(finished,response_id,recipient_first_name,recipient_last_name,starts_with('q3'), starts_with('q9'), starts_with('q5'))
to_rmv %<>% filter(response_id != 'R_7DMyWjdBdNj6bfz') %>% pull(response_id)

multis %<>% filter(!(response_id %in% to_rmv))

multis[which(is.na(multis$org_name)),]          
                   
## check on NA responses to see if they answered other key questions
View(filter(dat_survey, response_id=='R_7DMyWjdBdNj6bfz') %>% dplyr::select(finished,response_id,recipient_first_name,recipient_last_name,starts_with('q3'), starts_with('q9'), starts_with('q5')))

## fix missing org_names and then remove the q3_several_1 column (which is an uncleaned version of org_name)
multis %<>% mutate(org_name=case_when(
  response_id=='R_6EPTJ59BfL4cJQM'~'Reef Check',
   response_id=='R_3Hqxere4rGNArPa'~'California Ocean Science Trust',
  response_id=='R_7DMyWjdBdNj6bfz'~'University of California Santa Barbara - SBC LTER & Seaweed CDR Project',
  .default=org_name))


multis[which(is.na(multis$org_name)),] # 0 !


# Org renaming (following process_social_network) -------------------------

## Org # 2 listed
unique(multis %>% pull(q3_several_2))
multis %<>% mutate(org_name2 = case_when(
  grepl(paste(c('Humboldt','NEREO','Student assistant with Sean Craig'),collapse="|"), q3_several_2) ~ "California State Polytechnic University Humboldt",
  grepl(paste(c('Reef check','reef check','reefcheck','Reef Check','ReefCheck',"Reefcheck CA"),collapse="|"), q3_several_2) ~ "Reef Check",
  grepl(paste(c('moss landing','Moss Landing','MLML','Moss landing',"Moss Landing Marine Labs","Moss landing marine labs"),collapse="|"), q3_several_2) ~ "Moss Landing Marine Laboratories",
  q3_several_2=="https://www.sunflowerstarlab.org/" ~ "Moss Landing Marine Laboratories - Sunflower Star Laboratory",
  grepl(paste(c('Giant Giant Kelp','g2kr','giant giant kelp','G2KR','Great Great Kelp',
                'UrchinsKelpOtters',"Giant Kelp Project","Giant Giant Kelp Restoration Project",
                "Giant Giant Kelp Restoration Project (G2KR)",
                "Giant kelp restoration project - g2kr"),collapse="|"), q3_several_2) ~ "Giant Giant Kelp Forest Restoration Project",
  grepl(paste(c('UCSC','UC Santa Cruz'),collapse="|"), q3_several_2) ~ "University of California Santa Cruz",
  grepl(paste(c('CA Santa Barbara','California, Santa Barbara',"UC Santa Barbara"),collapse="|"),q3_several_2) ~ "University of California Santa Barbara",
  grepl(paste(c('UC Davis','Uc Davis','Uc davis','California Davis','California, Davis'),collapse="|"), q3_several_2) ~ "University of California Davis",
  grepl(paste(c('UC Berkeley','UCB','Uc berkeley'),collapse="|"), q3_several_2) ~ "University of California Berkeley",
  grepl('U.S. Geological Survey', q3_several_2) ~ "US Geological Survey",
  grepl('Urchin removal diver', q3_several_2) ~ "Individual",
  grepl(paste(c('CDFW','CALIFORNIA DEPARTMENT OF FISH AND WILDLIFE'),collapse="|"), q3_several_2) ~ "California Department of Fish and Wildlife",
  grepl('Sea Urchin Commission', q3_several_2) ~ "California Sea Urchin Commission",
  grepl('Earth equty', q3_several_2) ~ "Earth Equity",
  grepl('Cal Poly Pomona', q3_several_2) ~ "California State Polytechnic University Pomona",
  grepl('USFWS', q3_several_2) ~ "US Fish and Wildlife Service",
  grepl(paste(c('CA Ocean Protection Council','California Ocean Protection Council (contracted by them)'),collapse='|'),q3_several_2) ~ 'California Ocean Protection Council',
  q3_several_2=='California Ocean Protection Council (contracted by them)' ~ 'California Ocean Protection Council',
  q3_several_2=='UC San Diego' ~ "University of California San Diego", 
  grepl(paste(c("Watermens Alliance","Watermans Alliance","Watermen's Alliance"),collapse="|"), q3_several_2) ~ "Watermen's Alliance",
  q3_several_2=="GFNMS Advisory Council" ~ 'Greater Farallones National Marine Sanctuary',
  q3_several_2=="Ocean Rainforest Inc" ~ 'Ocean Rainforest',
  q3_several_2=="Coastal Stewardship Task Force, The Sea Ranch Association" ~ "The Sea Ranch Association: Coastal Stewardship Task Force",
  grepl(paste(c("Nature Conservancy","TNC"),collapse='|'),q3_several_2) ~ "The Nature Conservancy",
  q3_several_2=="Vantuna Research Group at Occidental College" ~ "Occidental College - Vantuna Research Group",
  q3_several_2=="Sherwood Valley Rancheria" ~ "Sherwood Valley Band of Pomo Indians",
  q3_several_2=="Fish Reef Project (fishreef.org)"~"Fish Reef Project",
  q3_several_2=="Greater Farallones and Cordell Bank National Marine Sanctuaries" ~ "Greater Farallones National Marine Sanctuary, Cordell Bank National Marine Sanctuary",
  q3_several_2=="Scripps Institution of Oceanography"~"University of California San Diego - Scripps Institution of Oceanography",
  q3_several_2=="Stanford University (past)" ~ "Stanford University",
  q3_several_2=="Greater Farralones" ~ "Greater Farallones National Marine Sanctuary",
  q3_several_2=="KRMP" ~ "Kelp Restoration and Management Plan Community Working Group",
  q3_several_2== "co-PI Santa Barbara Coastal LTER site" ~ NA,
  q3_several_2=="Stillwater Cove urchin divers" ~ NA,
  q3_several_2=="Bamboo Reef Scuba Diving Centers" ~ "Bamboo Reef Scuba Diving Centers - Triton Spearfishing",
  TRUE ~ q3_several_2
))


### check some specific answers
View(filter(dat_survey, response_id=='R_3at4LH8iRw4FZmh') %>% dplyr::select(finished,response_id,recipient_first_name,recipient_last_name,starts_with('q3'), starts_with('q9'), starts_with('q5')))


## Org # 3 + listed

## add a project name to a first org
multis %<>%
  filter(response_id != 'R_3at4LH8iRw4FZmh') %>%
  bind_rows(filter(multis, response_id=='R_3at4LH8iRw4FZmh') %>%
              mutate(org_name="California Sea Urchin Commission - Accelerating North Coast Kelp Recovery Project")
  )

## let's combine these next few columns....


unique(multis %>% pull(q3_several_3))
unique(multis %>% pull(q3_several_4))
unique(multis %>% pull(q3_several_5))
unique(multis %>% pull(q3_several_6))
unique(multis %>% pull(q3_several_8))
### check some specific answers
View(filter(dat_survey, response_id=='R_50f07NmTN6tehhY') %>% dplyr::select(finished,response_id,recipient_first_name,recipient_last_name,starts_with('q3'), starts_with('q9'), starts_with('q5')))
View(filter(dat_survey,q3_several_4=="CPC"))


clean_org <- function(x) {
  x <- as.character(x)
  return(
    case_when(
      grepl(paste(c('Humboldt','NEREO','Student assistant with Sean Craig'),collapse="|"), x) ~ "California State Polytechnic University Humboldt",
      grepl(paste(c('Reef check','reef check','reefcheck','Reef Check','ReefCheck',"Reefcheck CA"),collapse="|"), x) ~ "Reef Check",
      grepl(paste(c('moss landing','Moss Landing','MLML','Moss landing',"Moss Landing Marine Labs","Moss landing marine labs"),collapse="|"), x) ~ "Moss Landing Marine Laboratories",
      x=="Sunflower Sea Star Laboratory" ~ "Moss Landing Marine Laboratories - Sunflower Star Laboratory",
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
                    'California Department of Fish and Wildlife (working with opc as our contractee)'),collapse="|"), x) ~ "California Department of Fish and Wildlife",
      grepl('Sea Urchin Commission', x) ~ "California Sea Urchin Commission",
      grepl('Earth equty', x) ~ "Earth Equity",
      grepl('Cal Poly Pomona', x) ~ "California State Polytechnic University Pomona",
      grepl('USFWS', x) ~ "US Fish and Wildlife Service",
      grepl(paste(c('CA Ocean Protection Council','California Ocean Protection Council','Calif Ocean Protection Council'),collapse='|'),x) ~ 'California Ocean Protection Council',
      x=='California Ocean Protection Council (contracted by them)' ~ 'California Ocean Protection Council',
      x=='UC San Diego' ~ "University of California San Diego", 
      grepl(paste(c("Watermens Alliance","Watermans Alliance","Watermen's Alliance"),collapse="|"), x) ~ "Watermen's Alliance",
      x=="GFNMS Advisory Council" ~ 'Greater Farallones National Marine Sanctuary',
      x=="Ocean Rainforest Inc" ~ 'Ocean Rainforest',
      x=="Coastal Stewardship Task Force, The Sea Ranch Association" ~ "The Sea Ranch Association: Coastal Stewardship Task Force",
      grepl(paste(c("Nature Conservancy","TNC","The Nature Conservancy CA"),collapse='|'),x) ~ "The Nature Conservancy",
      x=="Vantuna Research Group at Occidental College" ~ "Occidental College - Vantuna Research Group",
      x=="Sherwood Valley Rancheria" ~ "Sherwood Valley Band of Pomo Indians",
      x=="Fish Reef Project (fishreef.org)"~"Fish Reef Project",
      x=="The Greater Farallones Association" ~ "Greater Farallones Association",
      grepl(paste(c("KRMP","KRMP community workgroup (CA DFW)"),collapse='|'),x) ~ "Kelp Restoration and Management Plan Community Working Group",
      x=="CDFW Red Abalone Recovery Working Group" ~ "Red Abalone Recovery Working Group",
      grepl(paste(c(
        "Volunteer Diving groups","North Coast KelpFest!","Independent filmmaking","x",
        "Triton Spearfishing",  # merged with _2
        "ANCKR", # merged with _1
        "CPC"    # name?
      ),collapse='|'),x) ~ NA,
      x=="Monterey Surfrider Chapter" ~ "Surfrider Foundation - Surfrider Monterey",
      x=="Humboldt Surfrider Foundation Humboldt Chapter" ~ "Surfrider Foundation - Surfrider Humboldt",
      x=="MPA Long Term Monitoring Program" ~ "California MPA Monitoring Program",
      x=="WaveWalker Charters" ~ "Wave Walker Charters",
      x=="West Coast Region, Office of National Marine Sanctuaries" ~ "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries - West Coast Region",
      x=="Fish and Wildlife" ~ "California Department of Fish and Wildlife",
      grepl(paste(c("CA Sea Grant","CA Seagrant"),collapse='|'),x) ~ "California Sea Grant",
      grepl("PISCO",x) ~ "Partnership for Interdisciplinary Studies of Coastal Oceans",
      x=="The Kelp Forest Alliance" ~ "Kelp Forest Alliance",
      TRUE ~ x
    ))
}

## apply fxn
multis %<>% 
  rename(q3_severalx_1=q3_several_1, q3_severalx_2=q3_several_2) %>%
  mutate_at(.vars=vars(starts_with('q3_several_')), clean_org)



# Merge into multi_org column ---------------------------------------------

## merge all into one column!
multis_collapse <- multis %>% 
  dplyr::select(-starts_with('q3_severalx')) %>%
  rename(q3_several_1=org_name, q3_several_2=org_name2) %>%
  pivot_longer(cols=starts_with('q3_several_'), names_to='tmp', values_to='all_orgs')
  
multis_collapse %<>% filter(!is.na(all_orgs)) %>%
  arrange(response_id,tmp) %>%
  group_by(response_id) %>% 
  summarise(multi_org=paste0(unique(all_orgs), collapse=", "))


### check a few
multis_id <- left_join(multis_id, multis_collapse)




# Save --------------------------------------------------------------------

## Just the organizations
sn2 %>% left_join(multis_collapse,by='response_id') %>%
  dplyr::select(response_id,org_name,multi_org) %>% distinct() %>%
  write_csv(here('confidential_data', 'processed','cleaned_multiple_orgs_2025-03-17.csv'))


## Added into the social network
sn2 %>% left_join(multis_collapse,by='response_id') %>%
  write_csv(here('confidential_data', 'processed','cleaned_social_network_with_org_allDOs_2025-03-17.csv'))

