############### Refine L1 social network for kelp SEN ############### 
#
# The Level 1 SEN includes only those individuals who are
#    counted as "systematic direct observers" in the kelp SEN.
#
# In the input for this script, the alters can be those involved 
#   in field work or not. Also, individuals associated with multiple
#   organizations may have some orgs that they directly observe
#   kelp forests for, and others that they do other forms of 
#   work for (e.g., education and outreach).
# The output data set includes only ego actors *and* alters
#   that are "systematic direct observers" / employ individuals
#   to conduct systematic direct observation. 
#
#
# 9/25/2025 - Mary Fisher
# Last edited: 10/1/2025
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
library(igraph)
#
source(here('R','subfxn','fill_adjacency_matrix_with_missing_data.R'))
#
d.in <- '2025-10-01'
# d.out <- Sys.Date()
d.out <- '2025-12-31'
#
# process_prefix <- 'updateINDupdateORG'
process_prefix <- 'updateINDupdateORGmanEGO2'


# Data --------------------------------------------------------------------

## output from script 4: level 1 survey respondents' social network
el_final_l1 <- read_csv(here('data','sen','networks',paste0('sn_dataresRID_collab_multi_',process_prefix,'_EDGELIST_4sen_',d.in,'.csv')))
head(el_final_l1)
nodelist_l1 <- read_csv(here('data','sen','networks',paste0('sn_dataresRID_collab_multi_',process_prefix,'_NODEATT_4sen_',d.in,'.csv')))
head(nodelist_l1)

## output from script 4: 
##   this tells us which alters are not covered by survey respondents.
sn_dat <- read_csv(here('confidential_data','processed',paste0('sn_dataresRID_',process_prefix, '_',d.in,'_4sen.csv')))

## this is the org categorizations key. I've done these categorizations by hand. the excel spreadsheet contains notes describing
##   the decisions made during categorization.
l1key <-  readxl::read_excel(here('data','sen',paste0('field_key_level1_egos_alters_2025-09-23_KEY.xlsx')),sheet='KEY')
l1key %<>% mutate(in_field=ifelse(in_field==0,2,in_field))



# Categorize orgs ---------------------------------------------------------

## the social network contains data for level 1 survey respondents
## but for those affiliated with multiple institutions, some institutions may not themselves be 'level 1'
## also, some alters may not be 'level'. 
el_final_l1 %<>% left_join(l1key, by=c('from'='org_name')) %>%
  rename(from_lvl=in_field) %>%
  left_join(l1key, by=c('to'='org_name')) %>%
  rename(to_lvl=in_field)

any(is.na(el_final_l1$from_lvl))

any(is.na(el_final_l1$to_lvl))
View(filter(el_final_l1,is.na(to_lvl))) ## retaining an isolate
el_final_l1 %<>% mutate(to_lvl=ifelse(is.na(to_lvl),1,to_lvl))


any(is.na(el_final_l1$to_lvl))


# Save l1-l1 --------------------------------------------------------------


## l1 - l1 connections.
el1_out <- filter(el_final_l1, from_lvl==1 & to_lvl==1)
dim(el1_out); dim(el_final_l1)  # 361 to 163 links

## l1 actors- missing any systematic direct observers that don't have level 1 ties? these will be isolates in the l1-l1 network
act1 <- filter(el_final_l1, from_lvl==1) %>% dplyr::select(from) %>% distinct() %>% rename(actor=from) %>%
  bind_rows(filter(el_final_l1, to_lvl==1) %>% dplyr::select(to) %>% distinct() %>% rename(actor=to))
act1 %<>% filter(!(actor %in% el1_out$from) & !(actor %in% el1_out$to))
act1
el1_out %<>% bind_rows(
  act1 %>% 
    rename(from=actor) %>% mutate(to=NA,r=1,tie='collab',from_lvl=1,to_lvl=1)
)

## let's condense commercial divers, recreational divers nodes
el1_out %<>% mutate(to=case_when(
  to != 'Recreational or Commercial Divers' &
    grepl('Commercial Diver',to) ~ 'Commercial Divers',
  to!= 'Recreational or Commercial Divers' &
    grepl('Recreational Diver',to) ~ 'Recreational Divers',
  .default=to)) %>%
  filter(to!="Recreational or Commercial Divers" | is.na(to)) %>% 
  bind_rows(filter(el1_out, to=="Recreational or Commercial Divers") %>%
              mutate(to='Recreational Divers'),
            filter(el1_out, to=="Recreational or Commercial Divers") %>%
              mutate(to='Commercial Divers'))

el1_out %<>% distinct() %>%
  group_by(from,to,tie) %>% summarise(r=sum(r))


## save edgelist and node attribute data frame

write_csv(el1_out, here('data','sen', 'networks',paste0('sn_l1l1_collab_multi_', process_prefix,'_EDGELIST_4sen_', d.out, '.csv')))

nodelist_l1out <- filter(nodelist_l1, sn_org_name %in% c(el1_out$from, el1_out$to))
write_csv(nodelist_l1out, here('data','sen', 'networks',paste0('sn_l1l1_collab_multi_', process_prefix,'_NODEATT_4sen_', d.out, '.csv')))


# Create & save l1-l1 matrix ----------------------------------------------

## get the alters that are not covered by survey data
na_alters <- unique(c(el1_out$from, el1_out$to))[which(!(unique(c(el1_out$from, el1_out$to)) %in% sn_dat$sn_org_name))]
length(na_alters); length(na_alters)/length(unique(c(el1_out$from, el1_out$to)))

na_alters
na_alters <- na_alters[!is.na(na_alters)]

## create a matrix
## igraph wants symmetrical matrices for undirected graphs. 
## use a custom function to create an adjacency matrix that maintains the NA/0 entries for missing data/missing tie
el1_out_4mat <- el1_out %>% filter(!is.na(to))  # remove isolates for this function
el1_mat <- fill_adjacency_matrix_with_missing_data(el=dplyr::select(el1_out_4mat, from,to,r), missing_data=na_alters, el_directed=FALSE)

dim(el1_mat)  ## 57 x 57
heatmap(as.matrix(el1_mat))  ## sparse matrix
isSymmetric(el1_mat)  ## yes!

## add back in isolates
to_add <- filter(el1_out, is.na(to)) %>% pull(from); to_add
any(to_add %in% colnames(el1_mat)) # should be FALSE
el1_mat <- rbind(el1_mat,matrix(data=0,nrow=length(to_add),ncol=dim(el1_mat)[2], dimnames=list(to_add,colnames(el1_mat))))
el1_mat <- make_square(el1_mat,values_fill=0)

dim(el1_mat)  ## 61 x 61
heatmap(as.matrix(el1_mat))  ## sparse matrix
isSymmetric(el1_mat)  ## yes!

## check matrix entries against input data frame
sum(el1_mat[upper.tri(el1_mat)],na.rm=TRUE) == (sum(el1_out$r)-length(to_add))  ## true
max(el1_mat, na.rm=TRUE) == max(el1_out$r, na.rm=TRUE)         ## true


## save
saveRDS(el1_mat, here('data','sen', 'networks',paste0('sn_l1l1_collab_multi_', process_prefix,'_MATRIX_4sen_', d.out, '.rds')))


## CHANGE TO BINARY
el1_mat2 <- el1_mat
el1_mat2[!is.na(el1_mat2) & el1_mat2 > 1] <- 1
max(el1_mat2,na.rm=TRUE)

## save
saveRDS(el1_mat2, here('data','sen', 'networks',paste0('sn_l1l1_collab_multi_', process_prefix,'_MATRIXbin_4sen_', d.out, '.rds')))




# Summarize l1 actors ----------------------------------------------
## this is for our results section

## how many total? without collapsing commercial divers
length(unique(filter(nodelist_l1out, !is.na(sn_org_name) & sn_org_name != 'Recreational or Commercial Divers')$sn_org_name))
## how many individuals? without collapsing commercial divers
length(unique(filter(nodelist_l1out, !is.na(sn_org_name) & sn_org_name != 'Recreational or Commercial Divers' & sn_org_type=='Individual')$sn_org_name))
## how many organizations
length(unique(filter(nodelist_l1out, !is.na(sn_org_name) & sn_org_type!='Individual')$sn_org_name))

## Without collapsing diver alters: how many l1 actors in each group?
filter(nodelist_l1out, !is.na(sn_org_name) & sn_org_name != 'Recreational or Commercial Divers') %>%
  group_by(alter_only) %>% summarise(n=length(unique(sn_org_name)),n_respondents=sum(r))
#       n    n_respondents
#  0    40  131
#  1    29  43




## Collapsing diver alters

nodelist_l1out_collapse <- nodelist_l1out %>% filter(!is.na(sn_org_name)) %>%
  filter(sn_org_name != 'Recreational or Commercial Divers') %>% 
  mutate(sn_org_name=case_when(
    grepl('Commercial Diver',sn_org_name) ~ 'Commercial Divers',
    grepl('Recreational Diver',sn_org_name) ~ 'Recreational Divers',
    .default=sn_org_name)) %>%
  bind_rows(nodelist_l1out %>% filter(sn_org_name == 'Recreational or Commercial Divers') %>%
              mutate(sn_org_name = 'Recreational Divers'),
            nodelist_l1out %>% filter(sn_org_name == 'Recreational or Commercial Divers') %>%
              mutate(sn_org_name = 'Commercial Divers')) %>%
  group_by(sn_org_name,sn_org_scale,sn_org_type,has_alter,alter_only) %>% summarise(r=sum(r))

## how many actors total?
length(unique(filter(nodelist_l1out_collapse, !is.na(sn_org_name))$sn_org_name))
## how many l1 actors in each group? 
filter(nodelist_l1out_collapse, !is.na(sn_org_name)) %>%
  group_by(alter_only) %>% summarise(n=length(unique(sn_org_name)),n_respondents=sum(r))
#       n    n_respondents
#  0    40  131
#  1    21  46


filter(nodelist_l1out_collapse, !is.na(sn_org_name)) %>%
  group_by(alter_only,sn_org_type) %>% summarise(n=length(unique(sn_org_name)))
# alter_only sn_org_type                    n
#   0 Commercial                     3
#   0 Consulting                     1
#   0 Government                     8
#   0 Individual                     3
#   0 NGO                           10
#   0 Research                      12
#   0 Tribal and Local Community     3
#   1 Commercial                     3
#   1 Government                     3
#   1 Individual                     2
#   1 NGO                            3
#   1 Research                       9
#   1 Tribal and Local Community     1

## how many organizations v individuals as egos, alters?
filter(nodelist_l1out_collapse, !is.na(sn_org_name)) %>%
  mutate(sn_org_type=ifelse(sn_org_type=='Individual','Individual','organization')) %>%
  group_by(alter_only,sn_org_type) %>% summarise(n=length(unique(sn_org_name)))

# alter_only sn_org_type      n
# 0   Individual              3
# 0   organization            37
# 1   Individual              2
# 1   organization            19






