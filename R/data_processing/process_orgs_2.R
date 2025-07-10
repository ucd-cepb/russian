############### Process survey data on alter organization ############### 
#
# Adjust the names of alter organizations.
#   
#
# For now, this script only completes this process for individuals
#   who responded to QUESTION 9 (info source) by saying that they 
#   directly observed kelp. *AND* only looks at collaborative ties.
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

