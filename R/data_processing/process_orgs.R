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



# FIRST PASS: select direct observers. ------------------------------------

## grab individuals who directly observe conditions
do <- unique(info %>%
  filter(info_type == 'I directly observe conditions') %>%
  pull(response_id))

## how many are not in the social network?
length(which(!(do %in% sn$response_id))) # 24

noSN_dat <- dat_survey %>% filter(response_id %in% do & !(response_id %in% sn$response_id)) %>% filter(finished=="True" | response_id=='R_7DMyWjdBdNj6bfz')

with(noSN_dat, table(q2))
# No, I am involved on my own       Yes, I am involved on behalf of one organization or group 
# 2                                                              11 
# Yes, I am involved on behalf of several organizations or groups 
# 5 



# FIRST PASS: add direct observers to SN data frame & Clean org names --------------------------

### No, I am involved on my own ###
tmp_add <- filter(noSN_dat, q2=='No, I am involved on my own')
sn2 <- sn %>% bind_rows(data.frame(response_id=tmp_add$response_id) %>%
                             mutate(num_id=-1,
                                    org_name="Individual",
                                    type=NA,
                                    alter=NA,
                                    edge_type=NA))
dim(sn) #995
dim(sn2) #997


### Yes, I am involved on behalf of one organization or group ###
tmp_add <- filter(noSN_dat, q2=='Yes, I am involved on behalf of one organization or group') %>%
  dplyr::select(response_id,q3_individual_1)
### fix up names
tmp_add %<>% mutate(org_name=clean_org(x=q3_individual_1))

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



### Yes, I am involved on behalf of several organizations or groups ###
do_add <- filter(noSN_dat, q2=='Yes, I am involved on behalf of several organizations or groups') %>%
  dplyr::select(response_id,starts_with('q3')) %>% dplyr::select(-q3_individual_1)
## Save the first listed as the org_name
do_add %<>% mutate(org_name=clean_org(x=q3_several_1))
### melt
do_add %<>% pivot_longer(starts_with('q3'), names_to='tmp', values_to='org_name2') %>% 
  filter(!is.na(org_name2)) 
do_add %<>% filter(tmp!='q3_several_1')
### one of the MLML responses is a duplicate
do_add %<>% filter(org_name2 != "Moss Landing Marine Labs")
### adjust names
do_add %<>% mutate(multi_org = clean_org(x=org_name2))

### merge to one column, in order
do_add %<>% arrange(response_id, tmp) %>% group_by(response_id,org_name) %>%
  ## to make sure it worked in order
  mutate(multi_org=paste0(multi_org,collapse=', '))
### get unique answers and address one particular survey response:
# R_3REDy37W41gd9F7 q3_several_1 I am a commercial Nearshore Rockfish Fisherman
# R_3REDy37W41gd9F7 q3_several_2 I was an Abalone Diver                        
# R_3REDy37W41gd9F7 q3_several_3 I am a harbor Commissioner  
do_add %<>% dplyr::select(response_id,org_name,multi_org) %>% distinct() %>%
  mutate(org_name=ifelse(response_id=="R_3REDy37W41gd9F7", "Noyo Harbor Commission", org_name))  ## save this for next section



# Revisit Research Universities: Multiple Groups --------------------------
## these are research universities with multiple groups working in different locations. 

### UCSB -- LTER, MPA/MLPA, SONGS? ###
ucsb <- filter(sn2, grepl('University of California Santa Barbara', org_name))
## add names 
ucsb %<>% left_join(dat_survey %>% select(all_of(qs_of_interest)))

ucsb %<>% rename(orig_org = org_name) %>%
  mutate(org_name = case_when(
    response_id %in% c('R_59T9thuNU6T04lX') ~ paste0(orig_org, ' - SONGS'),
    response_id %in% c('R_5dnbJSY7qhuMdsB','R_1MEiTDdy7UTusoz') ~ paste0(orig_org, ' - MPA'),
    response_id == 'R_6mwVifciTCq64Hs' ~  paste0(orig_org, ' - SONGS & MPA'),
    .default=orig_org
  ))

#respondent "R_8h9I2A79jUPDFkN" already specifies LTER

### Cal Poly Humboldt ###
humboldt <- filter(sn2, grepl('California State Polytechnic University Humboldt', org_name))
## add names 
humboldt %<>% left_join(dat_survey %>% select(all_of(qs_of_interest)))

humboldt %<>% rename(orig_org = org_name) %>%
  mutate(org_name = case_when(
    response_id %in% c('R_3uHTLrh3ea49nmF','R_1gumT7uft6SpkJY') ~ paste0(orig_org, ' - Kelp Culture'),
    response_id %in% c('R_5rju7W12HtPajKX','R_1LY9bZGoDJpKr8U','R_7PSlOsSbZJnSQVt') ~ paste0(orig_org, ' - MPA'),
    .default=orig_org
  ))
#respondents "R_1OwaKPJzCpBm5QR" and "R_1PTOVqgZmThkEyB" are in other programs at Cal Poly Humboldt


### add to sn2 ###
sn2 %<>% filter(!(response_id %in% ucsb$response_id) & !(response_id %in% humboldt$response_id)) %>%
  bind_rows(select(ucsb,all_of(colnames(sn2)))) %>%
  bind_rows(select(humboldt,all_of(colnames(sn2))))


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

## check ucsb multi orgs info


# Org renaming (following process_social_network) -------------------------

## Org # 2 listed
unique(multis %>% pull(q3_several_2))
multis %<>% mutate(org_name2 = clean_org(q3_several_2))
           


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
unique(multis %>% pull(q3_several_7))
unique(multis %>% pull(q3_several_8))
### check some specific answers
View(filter(dat_survey, response_id=='R_50f07NmTN6tehhY') %>% dplyr::select(finished,response_id,recipient_first_name,recipient_last_name,starts_with('q3'), starts_with('q9'), starts_with('q5')))
View(filter(dat_survey,q3_several_4=="CPC"))



## apply fxn
multis %<>% 
  rename(q3_severalx_1=q3_several_1, q3_severalx_2=q3_several_2) %>%
  mutate_at(.vars=vars(starts_with('q3_several_')), clean_org)





# Revisit Research Universities: Multiple Groups --------------------------
ucsb <- multis %>%
  pivot_longer(all_of(c(starts_with('q'),starts_with('org'))),names_to='level',values_to='org_name') %>%
  filter(grepl('University of California Santa Barbara', org_name)) %>%
  select(response_id) %>% distinct() %>% left_join(multis)
## add respondent names & corrected org names from sn2 (above)
ucsb_new <- ucsb %>%
  pivot_longer(all_of(c(starts_with('q'),starts_with('org'))),names_to='level',values_to='org_name') %>%
  rename(orig_org=org_name) %>%
  mutate(replace=ifelse(grepl('University of California Santa Barbara',orig_org), 1, 0)) %>%
  left_join(filter(sn2,grepl('University of California Santa Barbara', org_name)) %>%
              dplyr::select(response_id,org_name) %>% distinct() %>% mutate(replace=1)) %>%
  left_join(dat_survey %>% select(all_of(qs_of_interest)))

## move over original org names where ok -- includes one UCSB individual doing satellite / remote sensing work
ucsb_new %<>% mutate(org_name=ifelse(is.na(org_name), orig_org, org_name))

## pivot wider again and put back into multis df
ucsb_new2 <- ucsb_new %>% select(response_id,level,org_name) %>%
  pivot_wider(names_from = level, values_from=org_name)

multis %<>% filter(!(response_id %in% ucsb_new2$response_id)) %>%
  bind_rows(ucsb_new2)




humboldt <- multis %>%
  pivot_longer(all_of(c(starts_with('q'),starts_with('org'))),names_to='level',values_to='org_name') %>%
  filter(grepl('Humboldt', org_name)) %>%
  select(response_id) %>% distinct() %>% left_join(multis)
## add respondent names & corrected org names from sn2 (above)
humboldt_new <- humboldt %>%
  left_join(filter(sn2,grepl('Humboldt', org_name)) %>% select(response_id, org_name) %>% distinct(), by='response_id')
humboldt_new %<>% rename(org_name=org_name.x) %>% mutate(org_name=ifelse(org_name != org_name.y, org_name.y))
## fix two duplicate orgs, one weird glitch in a several_5 cleanup
humboldt_new %<>% mutate(org_name2=ifelse(response_id=='R_7PSlOsSbZJnSQVt', NA, org_name2),
                         q3_several_5=ifelse(response_id=='R_1LY9bZGoDJpKr8U', 'Surfrider Foundation - Humboldt Chapter', q3_several_5))

humboldt_new %<>% dplyr::select(-org_name.y)


multis %<>% filter(!(response_id %in% humboldt_new$response_id)) %>%
  bind_rows(humboldt_new)

# Remove affiliations that are collaborations -----------------------------
# 
# my_grepl <- function(x,y){
#   out <- c()
#   for(i in 1:length(x)){out[i] <- grepl(x[i],y[i])}
#   return(out)
# }
# 
# ## for respondents in social network only - grab collaborative ties
# sn_collab <- filter(sn2, edge_type=="collaboration")
# sn_collab %<>% separate(alter, into=c('alter','tmp'), sep=': ')
# sn_collab %<>% separate(alter, into=c('alter','tmp2'), sep='- ')
# sn_collab %<>% dplyr::select(response_id,alter) %>% distinct() %>%
#   group_by(response_id) %>%
#   summarise(alters=paste0(alter,collapse=','))
#   
# # group_by(across(c(-alter))) %>%
# 
# multis_check <- left_join(multis,sn_collab)
# 
# 
# multis_check %<>% 
#   dplyr::select(-starts_with('q3_severalx')) %>%
#   mutate(in_collab=my_grepl(x=org_name2, y=alters)) %>%
#   mutate(org_name2=ifelse(in_collab=="TRUE", NA, org_name2))
# #View(multis_check) ## check
# 
# multis_check %<>%
#   dplyr::select(-in_collab) %>% mutate(in_collab=my_grepl(x=q3_several_3, y=alters)) %>%
#   mutate(q3_several_3=ifelse(in_collab=="TRUE", NA, q3_several_3)) %>%
#   dplyr::select(-in_collab) %>% mutate(in_collab=my_grepl(x=q3_several_4, y=alters)) %>%
#   mutate(q3_several_4=ifelse(in_collab=="TRUE", NA, q3_several_4)) %>%
#   dplyr::select(-in_collab) %>% mutate(in_collab=my_grepl(x=q3_several_5, y=alters)) %>%
#   mutate(q3_several_5=ifelse(in_collab=="TRUE", NA, q3_several_5))%>%
#   dplyr::select(-in_collab) %>% mutate(in_collab=my_grepl(x=q3_several_6, y=alters)) %>%
#   mutate(q3_several_6=ifelse(in_collab=="TRUE", NA, q3_several_6))
#   
# #View(multis_check)
# 
# ## merge all into one column!
# multis_collapse <- multis_check %>% 
#   dplyr::select(-starts_with('q3_severalx'), -in_collab,-alter) %>%
#   distinct() %>%
#   pivot_longer(cols=c(org_name2, starts_with('q3_several_')), names_to='tmp', values_to='all_orgs')

# Merge into multi_org column ---------------------------------------------

## merge all into one column!
multis_collapse <- multis %>% 
  # dplyr::select(-starts_with('q3_severalx'), -in_collab,-alter) %>%
  dplyr::select(-starts_with('q3_severalx')) %>%
  distinct() %>%
  pivot_longer(cols=c(org_name2, starts_with('q3_several_')), names_to='tmp', values_to='all_orgs') %>%
  filter(!is.na(all_orgs))
  
multis_collapse %<>% arrange(response_id,tmp) %>%
  group_by(response_id,org_name) %>% 
  summarise(multi_org=paste0(unique(all_orgs), collapse=", "))

## add  in the 
multis_collapse %<>% bind_rows(do_add)


### check a few
multis_id <- left_join(multis_id, multis_collapse)



# Save --------------------------------------------------------------------

## Just the organizations
sn2 %>% left_join(select(multis_collapse, response_id, multi_org),by='response_id') %>%
  dplyr::select(response_id,org_name,multi_org) %>% distinct() %>%
  write_csv(here('confidential_data', 'processed','cleaned_multiple_orgs_2025-04-30.csv'))


## Added into the social network
sn2 %>% left_join(select(multis_collapse, response_id, multi_org),by='response_id') %>%
  write_csv(here('confidential_data', 'processed','cleaned_social_network_with_org_allDOs_2025-04-30.csv'))

