############### Raw survey data to SEN Prep ############### 
#
# Add counties to social network - QUESTION 5
# Create levels in social network -- QUESTIONS 4 & 9 
# Identify orgs to look for site-level data
#
# 2025-02-04  Mary Fisher
#
##########################################################


# Set up  -----------------------------------------------------------------

library(readr)
library(tidyverse) 
library(dplyr) 
library(tidyr) 
library(here)
library(magrittr)
library(janitor)
#
library(ergm)
library(sna) 
library(igraph)





# Data --------------------------------------------------------------------

## social network (processed)
kelp.edge <- read_csv(here('confidential_data','processed','kelp_social_by_org_2025-02-04.csv'))


## survey data
survey_dat <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv')) %>%
  clean_names() %>%
  slice(-c(1:2))

colnames(survey_dat)


########################### Counties where people work ###########################
##################################################################################

# Grab counties -----------------------------------------------------------

## counties q was number 5
county.net <- survey_dat %>% 
  select(response_id, starts_with("q5"))

## re-format
county.net %<>%
  pivot_longer(cols = -c(response_id), names_to = "tmp", values_to = "county") %>%
  dplyr::select(-tmp) %>%
  filter(!is.na(county))

## check out counties
county.net %>%
  ggplot(aes(x=county)) +
           geom_bar() +
  theme_bw() + coord_flip()

## clean up a bit
county.net %<>% 
  # sadly have to remove non CA respondents
  filter(!(county %in% c("Baja California","Ensenada, Mexico","Other","Other:","none"))) %>%
  # fix spelling and too-specific answer
  mutate(county = case_when(county=='Del Notre' ~ 'Del Norte',
                            county=='all counties with emphasis on Humboldt and Northern California' ~ '(all counties)',
                            TRUE ~ county)) 

## did anyone list out all counties instead of choosing 'all'? if so, replace their answers with (all counties) for consistency
all_counties <- county.net %>% group_by(response_id) %>% summarise(nc=length(unique(county))) %>%
  ungroup() %>%
  filter(nc > 14)
all_counties

county.net %<>% filter(!(response_id %in% all_counties$response_id)) %>%
  bind_rows(
    county.net %>% filter(response_id %in% all_counties$response_id) %>%
      dplyr::select(-county) %>% distinct() %>%
      mutate(county='(all counties)')
  )

## check out counties again
county.net %>%
  ggplot(aes(x=county)) +
  geom_bar() +
  theme_bw() + coord_flip()


## how many counties do individuals work across?
county.net %>%
  group_by(response_id) %>% summarise(nc=length(unique(county))) %>%
  ggplot(aes(x=nc)) +
  geom_histogram() +
  scale_x_continuous(breaks=seq(1,14,by=1), name='Counties per individual') +
  theme_bw() 


# Add org -----------------------------------------------------------------

head(kelp.edge)
head(county.net)

county.net %<>% left_join(
  kelp.edge %>% dplyr::select(response_id, org_name) %>%
    distinct(), by='response_id')


## save 
write_csv(county.net, here('confidential_data','kelp_by_org_by_CAcounty_2025-02-04.csv'))



# Network sketch ORG to COUNTY --------------------------------------------

tmp_df <- county.net %>%
  mutate(org_name=ifelse(is.na(org_name), "Individual",org_name)) %>%
  group_by(county,org_name) %>%
  summarise(n=length(unique(response_id)))

tmp_g <- graph_from_edgelist(as.matrix(tmp_df[,c(1:2)]), directed=FALSE)

V(tmp_g)$type <- data.frame(name=V(tmp_g)$name) %>%
  mutate(type=ifelse(name %in% tmp_df$org_name, 0,1)) %>%
  pull(type)

E(tmp_g)$weight <- tmp_df$n

plot(tmp_g, layout = layout_as_bipartite,
     vertex.color=c("cyan","green")[V(tmp_g)$type+1],
     edge.width=E(tmp_g)$weight)



######################### Source of info for kelp issues #########################
##################################################################################

# Get Information Source ---------------------------------------------

info <-  survey_dat %>% 
  filter(response_id %in% county.net$response_id) %>%  # only CA people in data set
  dplyr::select(response_id, starts_with("q9")) %>%
  filter(if_all(starts_with("q9"), is.na)) %>%
  mutate(q9_1='no response') %>%
  bind_rows(
    survey_dat %>% 
      dplyr::select(response_id, starts_with("q9")) %>%
      filter(!if_all(starts_with("q9"), is.na))
  ) %>% 
  pivot_longer(cols = -c(response_id), names_to = "tmp", values_to = "info_type") %>%
  dplyr::select(-tmp) %>% filter(!is.na(info_type))

## did we lose anyone?
all(county.net$response_id %in% info$response_id) #TRUE

## response rate for this question?
length(unique(filter(info, info_type != "no response")$response_id))/length(unique(info$response_id))
#96%

## check out infoions
info %>%
  ggplot(aes(x=info_type)) +
  geom_bar() +
  theme_bw() + coord_flip()

# make sure all "others" have text responses
info %>% filter(response_id %in% (info %>%
                      filter(info_type=="Other:") %>% pull(response_id))) %>%
  mutate(is_other=ifelse(info_type=="Other:", 'other', 'actual')) %>%
  group_by(response_id, is_other) %>% 
  summarise(n=n()) %>% pivot_wider(names_from=is_other, values_from=n) # they do!

View(info %>% filter(response_id %in% (info %>%
                                        filter(info_type=="Other:") %>% pull(response_id))) %>%
  filter(info_type != "Other:"))


## first-order info
info_1od <- c("I directly observe conditions","I collect or analyze data",
                  "I actively do kelp restoration","Direct impact cultural uses",
                  "I study my restoration projects")

## this assigns first-order info; all others (including NAs) are second-order 
info %<>% mutate(info_ord=ifelse(info_type %in% info_1od, 1,2))


## check out infoion orders
info %>%
  dplyr::select(response_id, info_ord) %>%
  distinct() %>%
  ggplot(aes(x=as.factor(info_ord))) +
  geom_bar() +
  xlab('info order') +
  theme_bw() + coord_flip()

## how many people responded with more than one information order? lots
info %>% group_by(response_id) %>% summarise(n_ord=length(unique(info_ord))) %>%
  group_by(n_ord) %>% summarise(n_respondents=length(unique(response_id)))

# n_ord n_respondents
# <int>         <int>
# 1            36
# 2           142


# Add org 
info %<>% left_join(
  kelp.edge %>% dplyr::select(response_id, org_name) %>%
    distinct(), by='response_id')



## look at breakdown of first order informations
info %>% filter(info_ord==1) %>%
  ggplot(aes(x=org_name)) +
  geom_bar() +
  facet_grid(cols=vars(info_type)) +
  theme_bw() + coord_flip()

## that's a lot of organizations. how many organizations are *not* at 1od?
dplyr::select(kelp.edge, org_name) %>%
  distinct() %>%
  filter(!(org_name %in% filter(info,info_ord==1)$org_name))
# 8 

## what proportion of covered orgs *are* at 1od?
sum(unique(kelp.edge$org_name) %in% filter(info,info_ord==1)$org_name)/ length(unique(kelp.edge$org_name))
# 85%




###################### How people are involved in kelp issues ######################
####################################################################################

# Get Involvement Type ---------------------------------------------
interact <- select(survey_dat, response_id, starts_with('q4'))


# make sure those who put 'other' included a text answer
View(
  interact %>% 
    unite(col='interact_type', starts_with('q4'), sep=',', na.rm=TRUE) %>%
    filter(grepl('Other:', interact_type))
)

## other responses: do they enrich other words, or add a new context? seem to do both for different individuals
interact_other <- interact %>%
  pivot_longer(starts_with('q4'), names_to='tmp', values_to='interact_type') %>% 
  filter(interact_type == 'Other:')
View(interact %>% filter(response_id %in% interact_other$response_id))

## write out so its easier to reference while coding
interact %>%
  filter(response_id %in% interact_other$response_id) %>%
  left_join(dplyr::select(kelp.edge, response_id, org_name) %>% distinct(), by='response_id') %>%
  write_csv(here('confidential_data','raw','q4_how_are_you_involved_other.csv'))


# Clean up 'other' --------------------------------------------------------

interact %<>%
  # if other is urchin culling or urchin removal, add or extend 'harvesting or fishing or growing' answer
  mutate(q4_1 = ifelse( response_id %in% c('R_5R8mhuPcpBgJCfg', 'R_6MY7AcdWIwKItCE', 'R_1eDbXLR4l7rE6wZ',
                                           'R_1eDbXLR4l7rE6wZ', 'R_3CCIZ5j7PiHSIsp'), 'Harvesting or fishing or growing:Urchin removal', q4_1)) %>%
  # environmental management
  mutate(q4_2 = case_when( response_id == 'R_3Hqxere4rGNArPa' ~ 'Environmental management:Policy development', 
                           response_id == 'R_1F990DdwF6i05R7' ~ 'Environmental management:Government',
                           .default=q4_2)) %>%
  # people management
  mutate(q4_3 = ifelse( response_id == 'R_6l4R7aWAfyJhiNk', 'People management:Supporting restoration diving operations', q4_3)) %>%
  # research
  mutate(q4_4 = case_when( response_id %in% c('R_3DhYjkyoy2NGWSY','R_3OJIfjza4it1gCi','R_3CCIZ5j7PiHSIsp',
                                           'R_1DOJ10bxtj6sNhY','R_5dLYySNPGDpX0fo','R_6aqSOPIVqzbD1KW',
                                           # volunteer , supporter who did not select research in survey
                                           'R_7upUSPV19KS9FVh','R_61dpMIWMs89lZLY') ~ 'Research:CitSci kelp forest surveys', 
                           response_id %in% c('R_5hm2WjZi0py0QMx', 'R_3H4Mz7SlkUD54nD') ~ 'Research:Otters', 
                           response_id == 'R_59T9thuNU6T04lX' ~ 'Research:Mitigation monitoring',
                           response_id == 'R_5J96mgzzSF0Ec4h' ~ 'Research:Experimental restoration',
                           response_id == 'R_3V7Q2T5Rvt15jcF' ~ 'Research:Data collection',
                           .default=q4_4)) %>%
  # advocacy and outreach -- education / teaching, media
  mutate(q4_5 = case_when( response_id =='R_3iPGxjt8gAiTFfa' ~ 'Advocacy or outreach:Education',
                           response_id =='R_70XAbTfFsJfV3ix' ~ 'Advocacy or outreach:Teaching', 
                           response_id == 'R_3CCIZ5j7PiHSIsp' ~ 'Advocacy or outreach:outreach docent',
                           response_id == 'R_56D6mBrAXbvzaBX' ~ 'Advocacy or outreach:Film, photography, media',
                           response_id == 'R_3UWJ3j85Z8A7G8N' ~ 'Advocacy or outreach:Media film',
                           .default=q4_5)) %>%
  # other
  mutate(q4_11 = case_when(response_id == 'R_1LLVDCfsYqnIq9H' ~ 'Restoration',
                           response_id == 'R_6aIROXxP5g9ceLh' ~ 'Restoration and stewardship',
                           response_id == 'R_5p0hFxU61rtshee' ~ 'Restoration volunteer (NGO)',
                           response_id == 'R_50sTP7MIe3Y7drH' ~ 'Kelp gametophyte biobanking', 
                           response_id == 'R_5J96mgzzSF0Ec4h' ~ 'Experimental restoration',
                           response_id == 'R_56yWtIgrU0bJwpX' ~ 'Volunteer (NGO)',
                           .default=NA))

## remove q4_11_text
interact %<>% dplyr::select(-q4_11_text)
           
## re format
interact %<>% pivot_longer(starts_with('q4'), names_to='tmp', values_to='interact_type') %>%
  dplyr::select(-tmp) %>% filter(!is.na(interact_type) & interact_type!="Other:")

## unique responses?
unique(interact$interact_type)

## add in affiliations
interact %<>% left_join(
  dplyr::select(kelp.edge, response_id, org_name) %>% distinct()
)

# which orgs / other interaction types are citizen scientists associated with?
interact %>% filter(grepl('CitSci', interact_type)) %>%
  group_by(org_name) %>% summarise(n=length(unique(response_id)))

# which orgs / other interaction types are harvesters / fishers / growers associated with? lots!
interact %>% filter(grepl('Harvesting', interact_type)) %>%
  group_by(org_name) %>% summarise(n=length(unique(response_id)))

# did people who put 'restoration' also select research?


write_csv(interact, here('confidential_data','processed','kelp_by_org_by_interact_2025-02-05.csv')) 


# To investigate for site-level info --------------------------------------
## add in counties as one column
out.1od <- county.net %>% dplyr::select(response_id, county) %>%
  group_by(response_id) %>% 
  summarise(counties=paste0(county,collapse=',')) %>%
  right_join(info,by='response_id') %>%
  filter(info_ord==1) 

## summarise counties for data collectors, direct impact, restoration
clean_counties <- out.1od %>% 
  filter(!is.na(org_name)) %>%
  filter(info_type!='I directly observe conditions') %>%
  group_by(org_name) %>%
  summarise(counties=paste0(unique(counties),collapse=',')) %>%
  mutate(info_ord=1) %>%
  mutate(counties=strsplit(counties, ",")) %>%
  pull(counties)

clean_counties <- lapply(clean_counties, unique)
clean_counties <- lapply(clean_counties, paste, collapse=',')


## summarise counties for directly observe conditions
clean_counties2 <- out.1od %>% 
  filter(!is.na(org_name)) %>%
  filter(info_type=='I directly observe conditions') %>%
  group_by(org_name) %>%
  summarise(counties=paste0(unique(counties),collapse=',')) %>%
  mutate(info_ord=1) %>%
  mutate(counties=strsplit(counties, ",")) %>%
  pull(counties)

clean_counties2 <- lapply(clean_counties2, unique)
clean_counties2 <- lapply(clean_counties2, paste, collapse=',')
  
## summarise orgs for data collectors, direct impact, restoration
out.1od %>% 
  filter(!is.na(org_name)) %>%
  filter(info_type!='I directly observe conditions') %>%
  group_by(org_name) %>%
  summarise(info_type=paste0(unique(info_type),collapse=','),
            response_id=paste0('count_',length(unique(response_id)))) %>%
  mutate(info_ord=1) %>%
  bind_cols(data.frame(counties=unlist(clean_counties))) %>%
  ## summarise orgs for directly observe conditions
  bind_rows(
    out.1od %>% 
      filter(!is.na(org_name)) %>%
      filter(info_type=='I directly observe conditions') %>%
      group_by(org_name) %>%
      summarise(info_type=paste0(unique(info_type),collapse=','),
                response_id=paste0('count_',length(unique(response_id)))) %>%
      mutate(info_ord=1) %>%
      bind_cols(data.frame(counties=unlist(clean_counties2)))
  ) %>%
  ## individual responses
  bind_rows(out.1od) %>%
  write_csv(here('confidential_data','intermediate','kelp_jan2025_first_order_kelp_information_summary.csv'))



# Add levels to s-e network --------------------------------------------
## split 'first order' into 1, 2
out.1od <- county.net %>% dplyr::select(response_id, county) %>%
  group_by(response_id) %>% 
  summarise(counties=paste0(county,collapse=',')) %>%
  right_join(info,by='response_id') %>%
  filter(info_ord==1) 

## use the **lowest order** information type per respondent
county.net %<>% left_join(
  info %>% group_by(response_id) %>%
    summarise(info_ord=min(info_ord)), by='response_id')


# Network sketch ORG to COUNTY --------------------------------------------

## second order information only
tmp_df2 <- county.net %>%
  filter(info_ord==2) %>%
  mutate(org_name=ifelse(is.na(org_name), "Individual",org_name)) %>%
  group_by(county,org_name) %>%
  summarise(n=length(unique(response_id)))

tmp_g2 <- graph_from_edgelist(as.matrix(tmp_df2[,c(1:2)]), directed=FALSE)

V(tmp_g2)$type <- data.frame(name=V(tmp_g2)$name) %>%
  mutate(type=ifelse(name %in% tmp_df2$org_name, 0,1)) %>%
  pull(type)

E(tmp_g2)$weight <- tmp_df2$n

plot(simplify(tmp_g2), layout = layout_as_bipartite,
     vertex.color=c("cyan","green")[V(tmp_g2)$type+1],
     vertex.shape=c("circle", "square")[V(tmp_g2)$type+1],
     vertex.label.cex=c(0.3,0.6)[V(tmp_g2)$type+1],
     edge.width=E(tmp_g2)$weight)



# track down survey data for specific respondents -------------------------
r <- 'R_5p0hFxU61rtshee'
View(filter(survey_dat, response_id==r) %>% dplyr::select(recipient_last_name,recipient_first_name,q1,
                                                          starts_with('q9'),
                                                          q18,email))
View(filter(kelp.edge, response_id==r))



