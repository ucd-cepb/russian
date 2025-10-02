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
#    counted as "systematic direct observers" in the kelp SEN.
#    alters can be those involved in field work or not. 
# The Level 2 SEN includes collaboration ties among individuals
#    who are *not* systematic direct observers. 
#    alters can be those involved in field work or not.
# The Combined SEN includes social network data for all survey
#    respondents. Organizations are classified according to 
#    whether they have "systematic direct observers" or not.
#    This means that in this network, social network data 
#    from Levels 1 and 2 *are merged* for organizations that
#    have any Level 1 individuals.
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
library(igraph)
#
d.in <- '2025-09-23'
# d.out <- Sys.Date()
d.out <- d.in
#
# process_prefix <- 'updateINDupdateORG'
process_prefix <- 'updateINDupdateORGmanEGO'





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

# Create First Level Social Network ---------------------------------------
## Data Res egos only ##
dat_out <- dat
dat_out %<>% filter(response_id %in% dat_lvl$response_id)

# Create First Level Social Network: clean up names -----------------------
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


## adjust UC Davis / BML affiliations
dat_out %<>% filter(response_id != 'R_50f07NmTN6tehhY') %>%
  bind_rows(dat_out %>% filter(response_id=='R_50f07NmTN6tehhY') %>%
              mutate(ego='University of California Davis - Bodega Marine Laboratory', 
                     sn_org_name='University of California Davis - Bodega Marine Laboratory',
                     multi_org=str_replace(multi_org,'University of California Davis','University of California Davis - Bodega Marine Laboratory')))
dat_out %<>% 
  mutate(alter_ind=str_trim(alter_ind)) %>%
  mutate(sn_alter=ifelse(alter=='University of California Davis' & alter_ind=='John Largier','University of California Davis - Bodega Marine Laboratory',sn_alter))

## save as data frame
write_csv(dat_out, here('confidential_data','processed',paste0('sn_datares_',process_prefix, '_',d.out,'_4sen.csv')))


# Create First Level Social Network: df as edge list --------------

## first, do collabs
el0 <- dat_out %>% group_by(sn_org_name,sn_org_scale,sn_org_type,sn_alter,sn_alter_scale,sn_alter_type) %>%
  summarise(r=length(unique(response_id))) %>% mutate(tie='collab')
## remove self-ties
el0 %<>% filter(sn_org_name != sn_alter)


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

all(colnames(el_add) %in% colnames(el0))

el0 %<>% bind_rows(el_add %>% ungroup() %>% dplyr::select(-org_name,-alter) %>% mutate(tie='shared_personnel'))

## look up one multiple affiliate - THERE MAY BE INVERSE DUPLICATES IN THIS EDGE LIST. 
filter(el0, sn_org_name==el_add$sn_org_name[1])

## temporary save so we don't lose work
write_csv(el0, here('data','sen',paste0('sn_datares_collab_multi_',process_prefix,'_EDGELIST_4sen_',d.out,'.csv')))

# Create First Level Social Network: clean up duplicates --------------

## collapse the collab / shared personnel duplicates
el_4graph <- el0 %>% group_by(sn_org_name,sn_org_scale,sn_org_type,sn_alter,sn_alter_scale,sn_alter_type) %>%
  summarise(r=sum(r), tie=paste0(unique(tie),collapse=','))
dim(el0); dim(el_4graph)  ## 440 to 425

## create and simplify a graph in which the edge attribute is the number of responses
g1 <- graph_from_edgelist(cbind(el_4graph$sn_org_name, el_4graph$sn_alter), directed=FALSE)
g1; is_simple(g1)
E(g1)$r <- el_4graph$r  # add edge attribute
g2 <- simplify(g1, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'sum')
g2; is_simple(g2)  ## down to 359 from 424
E(g2)$r
el_outr <- igraph::as_data_frame(g2,what='edges')

## create and simplify a graph in which the edge attribute is the tie type
g1 <- graph_from_edgelist(cbind(el_4graph$sn_org_name, el_4graph$sn_alter), directed=FALSE)
g1; is_simple(g1)
E(g1)$type <- el_4graph$tie # add edge attribute
g2 <- simplify(g1, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'concat')
g2; is_simple(g2)  ## down to 359 from 424
E(g2)$type
el_outt0 <- igraph::as_data_frame(g2,what='edges')
el_outt1 <- el_outt0 %>% group_by(from,to) %>% summarise(tie=paste0(unique(unlist(type)),collapse=','))
el_outt <- el_outt1 %>% group_by(from,to) %>% mutate(tie=paste0(unique(unlist(str_split(tie,',')[[1]])),collapse=','))

filter(el_outt, from==el_outt0[1,'from'])
filter(el_outt0, from==el_outt0[1,'from'])
filter(el_outt, from==el_outt0[46,'from'])
filter(el_outt0, from==el_outt0[46,'from'])

rm(el_outt0, el_outt1)

## merge
head(el_outt); head(el_outr)
el_out <- left_join(el_outr,el_outt, by=c('from','to'))
head(el_out)

## save over file above
write_csv(el_out, here('data','sen',paste0('sn_datares_collab_multi_',process_prefix,'_EDGELIST_4sen_',d.out,'.csv')))


## as matrix
mat_out <- el_out %>% dplyr::select(-tie) %>%
  #zeros v. missing data: start by making everyone that is blank NA
  pivot_wider(id_cols=from,names_from=to,values_from=r) %>%
  pivot_longer(cols=2:(length(unique(el_out$to))+1),names_to='to',values_to='r') %>%
  #if either actor in the pair was in our dat_out data frame as an ego, switch NA to 0
  mutate(r=ifelse(is.na(r) & (from %in% dat_out$sn_org_name | to %in% dat_out$sn_org_name),0,r)) %>%
  #pivot wider into a matrix again
  pivot_wider(id_cols=from,names_from=to,values_from=r) %>%
  column_to_rownames('from')

sum(mat_out==0,na.rm=TRUE)
sum(is.na(mat_out))

#save
saveRDS(mat_out, here('data','sen',paste0('sn_dataresRID_collab_multi_',process_prefix,'_MATRIX_4sen_',d.out,'.rds')))

# Second Pass: all collaborations -----------------------------------------
## No data res ##
dat_out2 <- dat
dat_out2 %<>% filter(!(response_id %in% dat_lvl$response_id))

# Create Second Level Social Network: clean up names -----------------------
## read in names key
key <- readxl::read_excel(here('data','sen',paste0('sn_match_ego_alter_organizations_',d.out,'_KEY.xlsx')), sheet='key')
head(key)

## melt out data frame so that each 'ego' organization gets one row
dat_out2 %<>% 
  pivot_longer(starts_with('q3_'), names_to='ego_level',values_to='ego') %>%
  filter(!is.na(ego))

## make sure we're not missing any orgs, and that the 'org_name' column always represents the q3_1 org
any(is.na(dat_out2$org_name)); any(is.na(dat_out2$ego)); any(is.na(dat_out2$alter))
View(filter(dat_out2,is.na(alter))) ## some individuals didn't provide alters
any(filter(dat_out2,ego_level=='q3_individual_1')$org_name != filter(dat_out2,ego_level=='q3_individual_1')$ego) # false
any(filter(dat_out2,ego_level=='q3_several_1')$org_name != filter(dat_out2,ego_level=='q3_several_1')$ego) #false

# apply re-naming to 'dat' dataframe. make sure we get all orgs for multi org respondents
dat_out2 %<>% dplyr::select(-org_name) %>%
  separate(alter, into=c('alter','alter_ind'), sep=':') %>%
  ## we need to keep names attached to artists, divers named as individuals
  mutate(alter=ifelse(alter %in% c('Commercial Diver','Artist','Photographer'), paste0(alter, ':', alter_ind),alter)) %>%
  ## new names for egos
  left_join(key, by=c('ego'='org_name')) %>%
  ## new names for alters
  left_join(key %>% rename(alter=org_name, sn_alter=sn_org_name,sn_alter_scale=sn_org_scale,sn_alter_type=sn_org_type), by=c('alter'))



## missing an ego name? these are *all individuals*
any(is.na(dat_out2$sn_org_name))
View(dat_out2 %>% filter(is.na(sn_org_name)))

## rename individual according to last 4 digits of response ID
##    except for the person who listed their 'individual' status as tertiary to their organizational affiliation.
dat_out2 %<>% filter(!is.na(sn_org_name)) %>%
  bind_rows(dat_out2 %>%
              filter(is.na(sn_org_name)) %>%
              filter(ego_level != 'q3_several_3') %>%
              mutate(ego=paste0('Individual:', str_sub(response_id, -4,-1))) %>%
              mutate(sn_org_name=ego,sn_org_scale='Individual',sn_org_type='Individual')
  )

## missing a corrected alter name?
any(is.na(filter(dat_out2, !is.na(alter))$sn_alter))
# View(filter(dat_out2, !is.na(alter) & is.na(sn_alter)))

## save as data frame
write_csv(dat_out2, here('confidential_data','processed',paste0('sn_level2_',process_prefix, '_',d.out,'_4sen.csv')))



# Create Second Level Social Network: df as edge list --------------

## first, do collabs
el0 <- dat_out2 %>% group_by(sn_org_name,sn_org_scale,sn_org_type,sn_alter,sn_alter_scale,sn_alter_type) %>%
  summarise(r=length(unique(response_id))) %>% mutate(tie='collab')
## remove self-ties
el0 %<>% filter(sn_org_name != sn_alter)


expand_unique <- function(x){
  x <- x[,2,drop=TRUE]
  tmp <- matrix(data=NA,nrow=length(x),ncol=length(x),dimnames=list(x,x))
  k <- arrayInd(which(upper.tri(tmp)), dim(tmp))
  out <- data.frame(org_name=rownames(tmp)[k[,1]], alter=colnames(tmp)[k[,2]])
  return(out)
}

## next, do shared individuals (i.e., wherever two orgs were listed by a single individual)
el_add <- dat_out2 %>% filter(!is.na(multi_org))
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

all(colnames(el_add) %in% colnames(el0))

el0 %<>% bind_rows(el_add %>% ungroup() %>% dplyr::select(-org_name,-alter) %>% mutate(tie='shared_personnel'))

## look up one multiple affiliate - THERE MAY BE INVERSE DUPLICATES IN THIS EDGE LIST. 
filter(el0, sn_org_name==el_add$sn_org_name[1])

## temporary save so we don't lose work
write_csv(el0, here('data','sen',paste0('sn_level2_collab_multi_',process_prefix,'_EDGELIST_4sen_',d.out,'.csv')))

# Create Second Level Social Network: clean up duplicates --------------

## collapse the collab / shared personnel duplicates
el_4graph <- el0 %>% group_by(sn_org_name,sn_org_scale,sn_org_type,sn_alter,sn_alter_scale,sn_alter_type) %>%
  summarise(r=sum(r), tie=paste0(unique(tie),collapse=','))
dim(el0); dim(el_4graph)  ## 161 to 159

## create and simplify a graph in which the edge attribute is the number of responses
g1 <- graph_from_edgelist(cbind(el_4graph$sn_org_name, el_4graph$sn_alter), directed=FALSE)
g1; is_simple(g1)
E(g1)$r <- el_4graph$r  # add edge attribute
g2 <- simplify(g1, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'sum')
g2; is_simple(g2)  ## down to 147 from 159
E(g2)$r
el_outr <- igraph::as_data_frame(g2,what='edges')

## create and simplify a graph in which the edge attribute is the tie type
g1 <- graph_from_edgelist(cbind(el_4graph$sn_org_name, el_4graph$sn_alter), directed=FALSE)
g1; is_simple(g1)
E(g1)$type <- el_4graph$tie # add edge attribute
g2 <- simplify(g1, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'concat')
g2; is_simple(g2)  ## down to 147 from 159
E(g2)$type
el_outt0 <- igraph::as_data_frame(g2,what='edges')
el_outt1 <- el_outt0 %>% group_by(from,to) %>% summarise(tie=paste0(unique(unlist(type)),collapse=','))
el_outt <- el_outt1 %>% group_by(from,to) %>% mutate(tie=paste0(unique(unlist(str_split(tie,',')[[1]])),collapse=','))

filter(el_outt, from==el_outt0[1,'from'])
filter(el_outt0, from==el_outt0[1,'from'])
filter(el_outt, from==el_outt0[46,'from'])
filter(el_outt0, from==el_outt0[46,'from'])

rm(el_outt0, el_outt1)

## merge
head(el_outt); head(el_outr)
el_out <- left_join(el_outr,el_outt, by=c('from','to'))
head(el_out)

## save over file above
write_csv(el_out, here('data','sen',paste0('sn_level2RID_collab_multi_',process_prefix,'_EDGELIST_4sen_',d.out,'.csv')))


## as matrix
mat_out <- el_out %>% dplyr::select(-tie) %>%
  #zeros v. missing data: start by making everyone that is blank NA
  pivot_wider(id_cols=from,names_from=to,values_from=r) %>%
  pivot_longer(cols=2:(length(unique(el_out$to))+1),names_to='to',values_to='r') %>%
  #if either actor in the pair was in our dat_out data frame as an ego, switch NA to 0
  mutate(r=ifelse(is.na(r) & (from %in% dat_out2$sn_org_name | to %in% dat_out2$sn_org_name),0,r)) %>%
  #pivot wider into a matrix again
  pivot_wider(id_cols=from,names_from=to,values_from=r) %>%
  column_to_rownames('from')

sum(mat_out==0,na.rm=TRUE)  ## 2322
sum(is.na(mat_out)) ##195

#save
saveRDS(mat_out, here('data','sen',paste0('sn_level2RID_collab_multi_',process_prefix,'_MATRIX_4sen_',d.out,'.rds')))




# Combined social network edgelist ----------------------------------------
## read in both edge lists.
el_final_l1 <- read_csv(here('data','sen','networks',paste0('sn_dataresRID_collab_multi_',process_prefix,'_EDGELIST_4sen_',d.out,'.csv')))
el_final_l2 <- read_csv(here('data','sen','networks',paste0('sn_level2RID_collab_multi_',process_prefix,'_EDGELIST_4sen_',d.out,'.csv')))
  
## create a key to classify both ego and alter actors / organizations as level 1 or not.
dat_org_lvl <- dat_out %>% group_by(sn_org_name,ego,sn_org_type) %>% 
  summarise(r=length(unique(response_id)),ego_level=paste0(unique(ego_level),collapse=','))

dat_org_lvl %<>% rename(orig_org_name=ego) %>%
  bind_rows(
  dat_out %>% group_by(sn_alter,alter,sn_alter_type) %>% 
    summarise(r=length(unique(response_id))) %>%
    mutate(ego_level='ALTER') %>%
    rename(sn_org_name=sn_alter,orig_org_name=alter, sn_org_type=sn_alter_type)
)

# write_csv(dat_org_lvl, here('data','sen',paste0('field_key_level1_egos_alters_',d.out,'.csv')))
l1key <-  readxl::read_excel(here('data','sen',paste0('field_key_level1_egos_alters_',d.out,'_KEY.xlsx')),sheet='KEY')
l1key %<>% mutate(in_field=ifelse(in_field==0,2,in_field))

## identify actors / organizations that should be considered "level 1" and those that are exclusively "level 2"
el_final_l1 %<>% left_join(l1key %>% rename(from_lvl=in_field), by=c('from'='org_name'))
any(is.na(el_final_l1$from_lvl))
el_final_l1 %<>% left_join(l1key %>% rename(to_lvl=in_field), by=c('to'='org_name'))
any(is.na(el_final_l1$to_lvl))


el_final_l2 %<>% left_join(l1key %>% rename(from_lvl=in_field), by=c('from'='org_name'))
any(is.na(el_final_l2$from_lvl))
el_final_l2 %<>% left_join(l1key %>% rename(to_lvl=in_field), by=c('to'='org_name'))
any(is.na(el_final_l2$to_lvl))
##  FOR NOW, AUTOMATICALLY MAKE UNKNOWN ALTERS LEVEL 2 FOR LEVEL 2 SURVEY RESPONDENTS
el_final_l2 %<>% mutate(from_lvl=ifelse(is.na(from_lvl),2,from_lvl),
                        to_lvl=ifelse(is.na(to_lvl),2,to_lvl))

## bind rows
el_final <- bind_rows(el_final_l1,el_final_l2)


## summarise across l1 and l2 data sets, using the key designations for l1 and l2 organizations
el_final %<>% group_by(from,from_lvl,to,to_lvl) %>%
  summarise(r=sum(r),
            tie=paste0(unique(tie),collapse=','))
dim(el_final)


## save 
write_csv(el_final, here('data','sen','networks',paste0('sn_ALLties_collab_multi_',process_prefix,'_EDGELIST_4sen_',d.out,'.csv')))

# Combined social network adjacency matrix --------------------------------

dat_out <- read_csv(here('confidential_data','processed',paste0('sn_datares_',process_prefix, '_',d.out,'_4sen.csv')))
dat_out2 <- read_csv(here('confidential_data','processed',paste0('sn_level2_',process_prefix, '_',d.out,'_4sen.csv')))
el_final <- read_csv(here('data','sen','networks',paste0('sn_ALLties_collab_multi_',process_prefix,'_EDGELIST_4sen_',d.out,'.csv')))
l1key <-  readxl::read_excel(here('data','sen',paste0('field_key_level1_egos_alters_',d.out,'_KEY.xlsx')),sheet='KEY')
l1key %<>% mutate(in_field=ifelse(in_field==0,2,in_field))


## all actors
el_all_actors <- unique(c(el_final$from, el_final$to))
dat_all_egos <- unique(c(dat_out$sn_org_name, dat_out2$sn_org_name))

## some actors had no social network connections. add these in with NA
all(dat_all_egos %in% el_all_actors)
dat_all_egos[which(!dat_all_egos%in% el_all_actors)]

el_final %<>% mutate(to=ifelse(to=='Ocean Protection Council','California Ocean Protection Council',to)) %>% distinct()

## get the alters that are not covered by survey data
na_alters <- el_all_actors[which(!(el_all_actors %in% dat_all_egos))]

length(na_alters); length(na_alters)/length(el_all_actors)  # 51%

na_alters

## create a matrix
## igraph wants symmetrical matrices for undirected graphs. 
## use a custom function to create an adjacency matrix that maintains the NA/0 entries for missing data/missing tie
mat_out <- fill_adjacency_matrix_with_missing_data(el=dplyr::select(el_final, from,to), missing_data=na_alters, el_directed=TRUE)

dim(mat_out)  ## 167x167
heatmap(as.matrix(mat_out))  ## sparse matrix
isSymmetric(mat_out)  ## yes!
max(mat_out, na.rm=TRUE) ## 1

## add survey respondents with no ties provided. 
to_add <- dat_all_egos[which(!dat_all_egos%in% el_all_actors)] ## 9
mat_out <- rbind(mat_out, matrix(data=0,nrow=length(to_add), ncol=ncol(mat_out), dimnames=list(to_add,colnames(mat_out))))
mat_out <- make_square(mat_out)
dim(mat_out)  ## 167x167
isSymmetric(mat_out)  ## yes!


## save
saveRDS(mat_out, here('data','sen', 'networks',paste0('sn_l1l1_collab_multi_', process_prefix,'_MATRIX_bin_4sen_', d.out, '.rds')))



