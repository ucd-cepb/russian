############### Process survey data on ego organization ############### 
#
# Adjust survey responses to q2. People can work on kelp-related issues
#    as an individual, on behalf of one organization, or on behalf of 
#    two or more organizations.
#
# For now, the script only processes information for people who work
#    on behalf of two or more organizations if they also answered
#    question 9 (this means they could be in the SEN)
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

# Data --------------------------------------------------------------------

## this is the survey data
dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
  clean_names() %>%
  slice(-c(1:2))

##   this is the cleaned up data set on the answer to the question: What are the main ways you learn about kelp forest-related issues?
info <- read_csv(here('confidential_data', 'processed','cleaned_responseID_by_info_source_q9.csv'))

## these are the questions with org info
question_3 <- dat_survey %>%
  dplyr::select(response_id, recipient_last_name, recipient_first_name, email, starts_with('q3')) %>% 
  pivot_longer(starts_with('q3'), values_to='org_name',names_to='org_level')

# Add missing -------------------------------------------------------------
## manually add in orgs
## from Gabby's script whatisgoingon_summary.R
##    section # Question 4.1 Heatmap.  on 7/3/2025; Gabby's
##    script version from  Jun 4, 2025 russian/commit/7d19a0ff2edb2352d5ee726d74138ae2f0a0f4a6
question_3 %<>%
  mutate(org_name = case_when(
    ## from Gabby whatisgoingon_summary.R Feb 2025
    # response_id == "R_7zc4m6dh9wc0YdK" & org_level=='q3_individual_1' ~ "California State Polytechnic University Humboldt",  # Mary removed 7/9 this is incorrect
    response_id == "R_1LLVDCfsYqnIq9H" & org_level=='q3_individual_1'  ~ "Get Inspired",
    response_id == "R_7JW5UqsrkFfRVRL" & org_level=='q3_individual_1'  ~ "California State University Long Beach",
    response_id == "R_7N1Uusw5TPiIBZ4" & org_level=='q3_individual_1'  ~ "Reef Check",
    response_id == "R_5dnbJSY7qhuMdsB" & org_level=='q3_individual_1'  ~ "University of California Santa Barbara",
    response_id == "R_38BizK7TnctWB0Z" & org_level=='q3_individual_1'  ~ "Moss Landing Marine Laboratories",
    response_id == "R_6U4QGgoI5tIAT6r" & org_level=='q3_individual_1'  ~ "Reef Check",
    response_id == "R_7DeczPHJmSjx6Tj" & org_level=='q3_individual_1'  ~ "Reef Check",
    response_id == "R_1NqY8R9wV0B745S" & org_level=='q3_individual_1'  ~ "Strategic Earth Consulting",
    response_id == "R_1OwaKPJzCpBm5QR" & org_level=='q3_individual_1'  ~ "California State Polytechnic University Humboldt",
    response_id == "R_7oHQgfbxkVtFRfP" & org_level=='q3_individual_1'  ~ "City College of San Francisco",
    response_id == "R_3jTauwChLV63T1f" & org_level=='q3_individual_1'  ~ "Giant Giant Kelp Restoration Project",
    response_id == "R_5p0hFxU61rtshee" & org_level=='q3_individual_1'  ~ "Giant Giant Kelp Restoration Project",
    response_id == "R_5DbUjzy5UEB6Bmv" & org_level=='q3_individual_1'  ~ "Giant Giant Kelp Restoration Project",
    response_id == "R_1A45wn3R1qfgQo1" & org_level=='q3_individual_1'  ~ "Individual",
    response_id == "R_1DIA6BHO7pKYU8x" & org_level=='q3_individual_1'  ~ "Individual",
    response_id == "R_3REDy37W41gd9F7" & org_level=='q3_individual_1'  ~ "Individual",
    ## added by Mary on 7/3/2025. note that some of these didn't provide alters, so they won't be in the social network.
    response_id == "R_5pzEeA0Y9QJuUVY" & org_level=='q3_individual_1'  ~ "University of California Davis - Bodega Marine Laboratory",
    response_id == "R_1ulIyefXrmNqx1f" & org_level=='q3_individual_1'  ~ "California Sea Grant",
    response_id == "R_3cBAzkvBbrEzWDq" & org_level=='q3_individual_1'  ~ "California Fish and Game Commission",
    response_id == "R_7ekodqIQhL8kW9Y" & org_level=='q3_individual_1'  ~ "California State Polytechnic University Humboldt",
    response_id == "R_5crCJOcIX1ZVu5X" & org_level=='q3_individual_1'  ~ "University of California San Diego - Scripps Institution of Oceanography",
    response_id == "R_7uHWBgcPD0hY0pj" & org_level=='q3_individual_1'  ~ "California Ocean Protection Council",  # finished=False
    response_id == "R_3KxOe3DfFTAjUg0" & org_level=='q3_individual_1'  ~ "California Department of Fish and Wildlife", # finished=False
    response_id == "R_1giJDgseUTPJq9R" & org_level=='q3_individual_1'  ~ "Comunidad y Biodiversidad",
    response_id == "R_6D5kmxtODIPb1hT" & org_level=='q3_individual_1'  ~ "University of California Santa Cruz", # finished=False
    response_id == "R_7OdkuIOsnSx3lGH" & org_level=='q3_individual_1'  ~ "University of California Merced", # finished=False
    response_id %in% c("R_6mLbnsU7eGt3zQ7","R_5OluY6cgVytnMXy") & org_level=='q3_individual_1'  ~ "California Department of Fish and Wildlife", # finished=False
    response_id == "R_3P6kxzidTnvjaEb" & org_level=='q3_individual_1'  ~ "Ocean Science Trust", # finished=False
    response_id == "R_6LMd7dTdf6Rfhim" & org_level=='q3_several_1'  ~ "Individual: Commercial Fishing",
    response_id == "R_6LMd7dTdf6Rfhim" & org_level=='q3_several_2'  ~ "The Good Captain Co.",
    response_id == "R_1oSFZfE9boXZM7N" & org_level=='q3_individual_1'  ~ "Universidad Autónoma de Baja California", # finished=False
    ## manually adjust where subgroups are listed as independent orgs
    response_id == "R_1PjehnKerfCz2KZ" & org_level=='q3_several_1'  ~ "Universidad Autónoma de Baja California - Facultad de Ciencias Marinas",
    response_id == "R_1PjehnKerfCz2KZ" & org_level=='q3_several_2'  ~ "Management of Ecosystems Across the Californias (MexCal)",
    response_id == "R_1PjehnKerfCz2KZ" & org_level=='q3_several_3'  ~ NA,
    TRUE ~ org_name))




## get rid of NA rows.
question_3 %<>% filter(!is.na(org_name) & org_name != 'x')


## who did we lose? use this to iteritively adjust code above.
lost_responses <- anti_join(dplyr::select(dat_survey, response_id, recipient_last_name, recipient_first_name, email,recipient_email,finished, 
                                          starts_with('q3'), starts_with('q11')), question_3) %>%
  filter(is.na(recipient_last_name) | !(recipient_last_name %in% question_3$recipient_last_name))



# Adjust organization names -----------------------------------------------

## use custom R function to adjust the names of organizations.

question_3 %<>% mutate(clean_org_name=clean_org_names(org_name))


## make decisions on "NA" answers
View(filter(question_3, is.na(clean_org_name)))

q3 <- question_3 %>% mutate(clean_org_name=case_when(org_name=='Decline to state' ~ 'Individual', 
                                                org_name=='Management of Ecosystems Across the Californias (MexCal)' ~ 'Management of Ecosystems Across the Californias (MexCal)',
                                                .default=clean_org_name)) %>%
  filter(response_id != 'R_1qyNTuTpApbsKrv') %>%
  bind_rows(
    filter(question_3, response_id=='R_1qyNTuTpApbsKrv' & org_level=='q3_several_2') %>% 
      mutate(org_level='q3_individual_1')
  )

q3 %<>% filter(!is.na(clean_org_name))

## re-format
q3 %<>% select(-org_name) %>% rename(org_name=clean_org_name)
q3 %<>% pivot_wider(names_from=org_level, values_from=org_name)
q3 %<>% select(response_id, recipient_first_name, recipient_last_name,email,q3_individual_1,colnames(q3)[which(grepl('q3_several',colnames(q3)))] )

## check for issues
filter(q3, is.na(q3_individual_1) & is.na(q3_several_1))
View(question_3 %>% select(clean_org_name) %>% distinct())




# Add individuals ---------------------------------------------------------
q3_ind <- dat_survey %>%
  dplyr::select(response_id, recipient_last_name, recipient_first_name, email, starts_with('q2')) %>%
  filter(q2=='No, I am involved on my own' & !(response_id %in% q3$response_id))
dim(q3_ind)


q3 %<>% bind_rows(
q3_ind %>% select(-q2) %>% mutate(q3_individual_1="Individual")
)

dim(q3) #191

# Save --------------------------------------------------------------------

q3 %>% select(response_id,starts_with('q3')) %>%
  write_csv(here('data','sen','processed_by_responseID_orgs.csv'))







# Direct observers: Clean up Part 2 ---------------------------------------

q3_clean <- q3

## grab individuals who directly observe conditions
do <- unique(info %>%
               filter(info_type == 'I directly observe conditions') %>%
               pull(response_id))

## how many are not in the social network?
length(which(!(do %in% q3$response_id))) # 0

## duplicated orgs for individuals involved on behalf of several organizations?
any(q3$q3_several_1 == q3$q3_several_2)
to_fix <- q3 %>% filter(q3_several_1 == q3_several_2)
View(filter(question_3, response_id %in% to_fix$response_id))


q3_clean %<>% filter(!response_id %in% to_fix) %>%
  bind_rows(
    to_fix %>% mutate(q3_several_2=q3_several_1,
                      q3_several_1=q3_several_3,
                      q3_individual_1=NA) %>%
      mutate(q3_several_3=NA)
  )


## cases where the same base org is listed for individuals involved on behalf of several organizations?
to_fix2 <- q3_clean %>% mutate(partial_match=str_detect(q3_several_1,q3_several_2)) %>%
  filter(partial_match=='TRUE')

## check raw survey data
View(dat_survey %>% filter(response_id %in% to_fix2$response_id))

## for both of these, it's ok to keep the more specific answer only
q3_clean %<>% mutate(q3_several_2=ifelse(response_id %in% to_fix2$response_id, NA, q3_several_2))

## cases where the same base org is listed for individuals involved on behalf of several organizations?
to_fix3 <- q3_clean %>% mutate(partial_match=str_detect(q3_several_2,q3_several_1)) %>%
  filter(partial_match=='TRUE')
## leave this in, scripps as separate from birch aquarium


# Direct observers: Clean up Part 3: Revisit Research Groups ----------------
## there are multiple research groups at the same university
##   who are associated with different sites.
##   make sure our data set has that specificity
##   for when we're building the SEN.

ucsb <- q3_clean %>%
  pivot_longer(all_of(starts_with('q3')),names_to='level',values_to='org_name') %>%
  filter(grepl('University of California Santa Barbara', org_name)) %>%
  select(response_id) %>% distinct() %>% left_join(q3_clean)
## add respondent names & corrected org names from sn2 (above)
ucsb_new <- ucsb %>%
  mutate(q3_individual_1=case_when(
    response_id=='R_5dnbJSY7qhuMdsB' ~ paste0(q3_individual_1, '- MPA'),
    response_id=='R_6mwVifciTCq64Hs' ~ paste0(q3_individual_1, '- SONGS & MPA'),
    .default=q3_individual_1
  ),
  q3_several_1=case_when(
    response_id=='R_59T9thuNU6T04lX' ~ paste0(q3_several_1, '- SONGS'),
    response_id=='R_1MEiTDdy7UTusoz' ~ paste0(q3_several_1, '- MPA'),
    .default=q3_several_1
  ))
  

q3_clean %<>% filter(!(response_id %in% ucsb_new$response_id)) %>%
  bind_rows(ucsb_new)

rm(ucsb_new)


humboldt <- q3_clean %>%
  pivot_longer(all_of(c(starts_with('q'),starts_with('org'))),names_to='level',values_to='org_name') %>%
  filter(grepl('Humboldt', org_name)) %>%
  select(response_id) %>% distinct() %>% left_join(q3_clean)
## no involvement in kelp forest issues
q3_clean %<>% filter(response_id != 'R_7ekodqIQhL8kW9Y')

## try again
humboldt <- q3_clean %>%
  pivot_longer(all_of(c(starts_with('q'),starts_with('org'))),names_to='level',values_to='org_name') %>%
  filter(grepl('Humboldt', org_name)) %>%
  select(response_id) %>% distinct() %>% left_join(q3_clean)
## NEREO (the MPA monitoring program) or Aquaculture
View(dat_survey %>% filter(response_id %in% humboldt$response_id))
View(dat_survey %>% filter(response_id=='R_1gumT7uft6SpkJY'))
humboldt_new <- humboldt %>% 
  filter(!response_id %in% c('R_1OwaKPJzCpBm5QR','R_1PTOVqgZmThkEyB','R_3RSD7GUxRnXMGbL')) %>%
  mutate(q3_individual_1=case_when(
    response_id %in% c('R_3uHTLrh3ea49nmF','R_1gumT7uft6SpkJY') ~ "California State Polytechnic University Humboldt - Aquaculture",
  .default=q3_individual_1),
  q3_several_1=case_when(
    response_id =='R_1LY9bZGoDJpKr8U' ~ "California State Polytechnic University Humboldt - North coast Evaluation of Reef Ecosystems Organization",
    .default=q3_several_1))


q3_clean %<>% filter(!(response_id %in% humboldt_new$response_id)) %>%
  bind_rows(humboldt_new)



# Merge first listed org --------------------------------------------------

q3_clean %<>% mutate(org_name=ifelse(is.na(q3_individual_1),q3_several_1,q3_individual_1))
any(is.na(q3_clean$org_name))

# Create one multi-org column ---------------------------------------------

multis_collapse <- q3_clean %>% 
  pivot_longer(starts_with('q3_several'), names_to='tmp', values_to='all_orgs') %>%
  filter(!is.na(all_orgs)) %>%
  group_by(response_id) %>% 
  summarise(multi_org=paste0(unique(all_orgs), collapse=","))

q3_clean %<>% select(response_id, org_name, starts_with('q3')) %>%
  left_join(multis_collapse)
  

# Save --------------------------------------------------------------------

## in russian
q3_clean %>%
  write_csv(here('data','sen',paste0('processed_by_responseID_orgs_4sen_',Sys.Date(),'.csv')))
## in SEN repo
q3_clean %>%
  write_csv(here('../','california-kelp-SEN','data','survey','confidential',
                 paste0('processed_by_responseID_orgs_4sen_',Sys.Date(),'.csv')))





