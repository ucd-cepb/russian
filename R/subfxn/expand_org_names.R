#' Expand organization names
#'
#' The survey asked who respondents collaborated with. This function
#' takes answers that include an umbrella institution like PISCO or DISES
#' and expands to include all participating organizations.
#' {Primary Organization} - {Subgroup / Project}: {Individual}
#' 
#' Last updated: 7/3/2025
#'
#' @param data a data frame that contains *at least* two columns: `org_name` = the names to clean, `response_id` = unique respondent ID.
#' @param return_original true / false. FALSE: return output as a 2-col data frame: response ID and org name. TRUE: return output as a 3-col data frame, with third column the original organization names
#' @return if !collab: vector or list (if return_original). if collab: data frame  (because 3 organizations are folded into DISES). 
#' @examples
#' 
#' dat_survey <- read_csv(here('confidential_data','raw','kelp_jan.9.25_copy.csv'))  %>%
#' clean_names() %>% slice(-c(1:2))
#' 
#' question_3 <- dat_survey %>%
#' dplyr::select(response_id, recipient_last_name, recipient_first_name, email, starts_with('q3')) %>% 
#' pivot_longer(starts_with('q3'), values_to='org_name',names_to='org_level')
#' 
#' question_3_clean <- clean_org_names(data=dplyr::select(dat_survey, response_id,org_name), ind_fix=TRUE)
#' 
#' @export


expand_org_names <- function() {
  data$new_org <- out
  ###################################
  
  dises_to_add <- filter(data, grepl("Kelp Restoration as an Integrated Socio-Ecological System", org_name)) %>%
    select(response_id)
  
  dises_to_add %<>% mutate(new_org='Cal Poly Humboldt') %>%
    bind_rows(dises_to_add %>% mutate(new_org='UC Santa Cruz'))
  
  ###################################
  
  pisco_to_add <- filter(data, org_name=="Partnership for Interdisciplinary Studies of Coastal Oceans") %>%
    select(response_id)
  pisco_to_add %<>% mutate(new_org='Oregon State University') %>%
    bind_rows(pisco_to_add %>% mutate(new_org='Stanford University - Hopkins Marine Station')) %>%
    bind_rows(pisco_to_add %>% mutate(new_org='University of California Santa Barbara')) %>%
    bind_rows(pisco_to_add %>% mutate(new_org='University of California Santa Cruz'))
  
  #######################################
  out <- data %>% bind_rows(dises_to_add,pisco_to_add) %>% select(response_id,new_org)
}