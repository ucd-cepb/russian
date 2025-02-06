############### Raw survey data to SEN Prep ############### 
#
# Add counties to social network
# Create levels in social network
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
kelp.edge <- read_csv(here('confidential_data','kelp_social_by_org_2025-02-04.csv'))


## survey data
survey_dat <- read_csv(here('confidential_data','kelp_jan.9.25_copy.csv')) %>%
  clean_names() %>%
  slice(-c(1:2))

colnames(survey_dat)



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


# Get Interaction Type ---------------------------------------------

interact <-  survey_dat %>% 
  filter(response_id %in% county.net$response_id) %>%  # only CA people in data set
  dplyr::select(response_id, starts_with("q9")) %>%
  filter(if_all(starts_with("q9"), is.na)) %>%
  mutate(q9_1='no response') %>%
  bind_rows(
    survey_dat %>% 
      dplyr::select(response_id, starts_with("q9")) %>%
      filter(!if_all(starts_with("q9"), is.na))
  ) %>% 
  pivot_longer(cols = -c(response_id), names_to = "tmp", values_to = "interact_type") %>%
  dplyr::select(-tmp) %>% filter(!is.na(interact_type))

## did we lose anyone?
all(county.net$response_id %in% interact$response_id) #TRUE

## response rate for this question?
length(unique(filter(interact, interact_type != "no response")$response_id))/length(unique(interact$response_id))
#96%

## check out interactions
interact %>%
  ggplot(aes(x=interact_type)) +
  geom_bar() +
  theme_bw() + coord_flip()

# make sure all "others" have text responses
interact %>% filter(response_id %in% (interact %>%
                      filter(interact_type=="Other:") %>% pull(response_id))) %>%
  mutate(is_other=ifelse(interact_type=="Other:", 'other', 'actual')) %>%
  group_by(response_id, is_other) %>% 
  summarise(n=n()) %>% pivot_wider(names_from=is_other, values_from=n) # they do!

# View(interact %>% filter(response_id %in% (interact %>%
#                                         filter(interact_type=="Other:") %>% pull(response_id))) %>%
#   filter(interact_type != "Other:"))


## first-order interactions
interact_1od <- c("I directly observe conditions","I collect or analyze data",
                  "I actively do kelp restoration","Direct impact cultural uses",
                  "I study my restoration projects")

## this assigns first-order interactions; all others (including NAs) are second-order 
interact %<>% mutate(interact_ord=ifelse(interact_type %in% interact_1od, 1,2))


## check out interaction orders
interact %>%
  dplyr::select(response_id, interact_ord) %>%
  distinct() %>%
  ggplot(aes(x=as.factor(interact_ord))) +
  geom_bar() +
  xlab('interaction order') +
  theme_bw() + coord_flip()

## how many people responded with more than one interaction order? lots
interact %>% group_by(response_id) %>% summarise(n_ord=length(unique(interact_ord))) %>%
  group_by(n_ord) %>% summarise(n_respondents=length(unique(response_id)))

# n_ord n_respondents
# <int>         <int>
# 1            36
# 2           142


# Add org 
interact %<>% left_join(
  kelp.edge %>% dplyr::select(response_id, org_name) %>%
    distinct(), by='response_id')



## look at breakdown of first order interactions
interact %>% filter(interact_ord==1) %>%
  ggplot(aes(x=org_name)) +
  geom_bar() +
  facet_grid(cols=vars(interact_type)) +
  theme_bw() + coord_flip()

## that's a lot of organizations. how many organizations are *not* at 1od?
dplyr::select(kelp.edge, org_name) %>%
  distinct() %>%
  filter(!(org_name %in% filter(interact,interact_ord==1)$org_name))
# 8 

## what proportion of covered orgs *are* at 1od?
sum(unique(kelp.edge$org_name) %in% filter(interact,interact_ord==1)$org_name)/ length(unique(kelp.edge$org_name))
# 85%



# To investigate for site-level info --------------------------------------
## add in counties as one column
out.1od <- county.net %>% dplyr::select(response_id, county) %>%
  group_by(response_id) %>% 
  summarise(counties=paste0(county,collapse=',')) %>%
  right_join(interact,by='response_id') %>%
  filter(interact_ord==1) 

## summarise counties for data collectors, direct impact, restoration
clean_counties <- out.1od %>% 
  filter(!is.na(org_name)) %>%
  filter(interact_type!='I directly observe conditions') %>%
  group_by(org_name) %>%
  summarise(counties=paste0(unique(counties),collapse=',')) %>%
  mutate(interact_ord=1) %>%
  mutate(counties=strsplit(counties, ",")) %>%
  pull(counties)

clean_counties <- lapply(clean_counties, unique)
clean_counties <- lapply(clean_counties, paste, collapse=',')


## summarise counties for directly observe conditions
clean_counties2 <- out.1od %>% 
  filter(!is.na(org_name)) %>%
  filter(interact_type=='I directly observe conditions') %>%
  group_by(org_name) %>%
  summarise(counties=paste0(unique(counties),collapse=',')) %>%
  mutate(interact_ord=1) %>%
  mutate(counties=strsplit(counties, ",")) %>%
  pull(counties)

clean_counties2 <- lapply(clean_counties2, unique)
clean_counties2 <- lapply(clean_counties2, paste, collapse=',')
  
## summarise orgs for data collectors, direct impact, restoration
out.1od %>% 
  filter(!is.na(org_name)) %>%
  filter(interact_type!='I directly observe conditions') %>%
  group_by(org_name) %>%
  summarise(interact_type=paste0(unique(interact_type),collapse=','),
            response_id=paste0('count_',length(unique(response_id)))) %>%
  mutate(interact_ord=1) %>%
  bind_cols(data.frame(counties=unlist(clean_counties))) %>%
  ## summarise orgs for directly observe conditions
  bind_rows(
    out.1od %>% 
      filter(!is.na(org_name)) %>%
      filter(interact_type=='I directly observe conditions') %>%
      group_by(org_name) %>%
      summarise(interact_type=paste0(unique(interact_type),collapse=','),
                response_id=paste0('count_',length(unique(response_id)))) %>%
      mutate(interact_ord=1) %>%
      bind_cols(data.frame(counties=unlist(clean_counties2)))
  ) %>%
  ## individual responses
  bind_rows(out.1od) %>%
  write_csv(here('confidential_data','intermediate','kelp_jan2025_first_order_kelp_interactions_summary.csv'))



# Add levels to s-e network --------------------------------------------
## split 'first order' into 1, 2
out.1od <- county.net %>% dplyr::select(response_id, county) %>%
  group_by(response_id) %>% 
  summarise(counties=paste0(county,collapse=',')) %>%
  right_join(interact,by='response_id') %>%
  filter(interact_ord==1) 

## use the **lowest order** interaction type per respondent
county.net %<>% left_join(
  interact %>% group_by(response_id) %>%
    summarise(interact_ord=min(interact_ord)), by='response_id')


# Network sketch ORG to COUNTY --------------------------------------------

## second order interactions only
tmp_df2 <- county.net %>%
  filter(interact_ord==2) %>%
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
r <- 'R_1GBVWsZocl0VprB'
View(filter(survey_dat, response_id==r) %>% dplyr::select(recipient_last_name,recipient_first_name,q1,
                                                          starts_with('q9'),
                                                          q18,email))
View(filter(kelp.edge, response_id==r))



