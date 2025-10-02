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
d.in <- '2025-09-23'
# d.out <- Sys.Date()
d.out <- d.in
#
# process_prefix <- 'updateINDupdateORG'
process_prefix <- 'updateINDupdateORGmanEGO'


# Data --------------------------------------------------------------------

## output from script 4: level 1 survey respondents' social network
el_final_l1 <- read_csv(here('data','sen','networks',paste0('sn_dataresRID_collab_multi_',process_prefix,'_EDGELIST_4sen_',d.in,'.csv')))
head(el_final_l1)

## output from script 4: 
##   this tells us which alters are not covered by survey respondents.
sn_dat <- read_csv(here('confidential_data','processed',paste0('sn_datares_',process_prefix, '_',d.in,'_4sen.csv')))

## this is the org categorizations key. I've done these categorizations by hand. the excel spreadsheet contains notes describing
##   the decisions made during categorization.
l1key <-  readxl::read_excel(here('data','sen',paste0('field_key_level1_egos_alters_',d.out,'_KEY.xlsx')),sheet='KEY')
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



# Save l1-l1 --------------------------------------------------------------


## first data frame: l1 - l1 connections.
el1_out <- filter(el_final_l1, from_lvl==1 & to_lvl==1)
dim(el1_out); dim(el_final_l1)  # 359 to 159 links

write_csv(el1_out, here('data','sen', 'networks',paste0('sn_l1l1_collab_multi_', process_prefix,'_EDGELIST_4sen_', d.out, '.csv')))



# Create & save l1-l1 matrix ----------------------------------------------

## get the alters that are not covered by survey data
na_alters <- unique(c(el1_out$from, el1_out$to))[which(!(unique(c(el1_out$from, el1_out$to)) %in% sn_dat$sn_org_name))]
length(na_alters); length(na_alters)/length(unique(c(el1_out$from, el1_out$to)))

na_alters

## create a matrix
## igraph wants symmetrical matrices for undirected graphs. 
## use a custom function to create an adjacency matrix that maintains the NA/0 entries for missing data/missing tie

el1_mat <- fill_adjacency_matrix_with_missing_data(el=dplyr::select(el1_out, from,to,r), missing_data=na_alters, el_directed=FALSE)

dim(el1_mat)  ## 65 x 65
heatmap(as.matrix(el1_mat))  ## sparse matrix
isSymmetric(el1_mat)  ## yes!

## check matrix entries against input data frame
sum(el1_mat[upper.tri(el1_mat)],na.rm=TRUE) == sum(el1_out$r)  ## true
max(el1_mat, na.rm=TRUE) == max(el1_out$r, na.rm=TRUE)         ## true

## CHANGE TO BINARY - THE PREVIOUS SCRIPT DOESN'T CALCULATE THE 'R' COLUMN CORRECTLY
el1_mat[!is.na(el1_mat) & el1_mat > 1] <- 1
max(el1_mat,na.rm=TRUE)

## save
saveRDS(el1_mat, here('data','sen', 'networks',paste0('sn_l1l1_collab_multi_', process_prefix,'_MATRIX_bin_4sen_', d.out, '.rds')))














