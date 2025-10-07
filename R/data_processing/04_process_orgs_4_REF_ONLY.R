
############### ############### ############### ############### ############### 
#   THIS IS A REFERENCE VERSION OF THE SCRIPT. THE ACTUAL SCRIPT CONTAINS
#   SURVEY RESPONDENT NAMES IN THE CODE, SO IT IS KEPT AS A LOCAL COPY ONLY.
############### ############### ############### ############### ############### 


############### Process survey data for alters covered by survey ############### 
#
# Adjust processed social network data frame for alters who were
#   also survey respondents. Anyone who named a 'covered' alter
#   is linked to *all* of that alter's self-affiliates. 
# Then, remove the individual names from the alter column, and re-do
#   the filtering to remove alters that are also listed as ego 
#   organizations. 
#   This is necessary for getting accurate edge weights (counts of survey
#   respondents) for the SEN. 
#

#
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
source(here('R/subfxn/expand_org_names.R'))
#
d.in <- '2025-10-01'
d.out <- d.in
#
write_out <- TRUE
#
# process_prefix <- 'updateINDupdateORG'
process_prefix <- 'updateINDupdateORGmanEGO'


# Data --------------------------------------------------------------------
## this is the data frame from process_orgs_3
dat <- read_csv(here('confidential_data','processed',paste0('processed_by_responseID_q3orgs_q11collabs_',process_prefix,'_4sen_',d.out,'.csv')))

## this is the survey data
dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
  clean_names() %>%
  slice(-c(1:2))
dat_survey %<>% filter(as.numeric(progress) > 75)
dim(dat_survey)  # 190

dat$response_id[which(!(dat$response_id %in% dat_survey$response_id))] # zero!



# Sections of script ------------------------------------------------------

# Create survey respondent ID key
# Social ties between survey respondents: Find
# Social ties between survey respondents: What to Expand?
# Social ties between survey respondents: Expand Part 1: Alter org != Alter's Ego org
# Social ties between survey respondents: Expand part 2: Add alter orgs for multiple affiliations
# Remove alters that match egos
# ONE EGO ORG: Exact Ego-Alter match 
# 2+ ORGS: Exact Ego-Alter Match 
# 2+ ORGS: Duplicate Alters 
# 2+ ORGS: Similar Ego-Alter 
# SAVE