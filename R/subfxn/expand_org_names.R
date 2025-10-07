#' Expand organization names
#'
#' The survey asked who respondents collaborated with. This function
#' takes answers that include an umbrella institution like PISCO or DISES
#' and expands to include all participating organizations. It is
#' intended to run within the function clean_org_names.
#' Formatting for organizations:
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
  
  dises_to_add %<>% mutate(new_org='University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System') %>%
    bind_rows(dises_to_add %>% mutate(new_org='University of California Davis: Marissa Baskett')) %>%
    bind_rows(dises_to_add %>% mutate(new_org='University of California Davis: Tyler Scott')) %>%
    bind_rows(dises_to_add %>% mutate(new_org='University of California Davis: Mike Springborn')) %>%
    bind_rows(dises_to_add %>% mutate(new_org='University of California Davis - Bodega Marine Laboratory: John Largier')) %>%
    bind_rows(dises_to_add %>% mutate(new_org='University of California Santa Cruz: Mark Carr')) %>%
    bind_rows(dises_to_add %>% mutate(new_org='University of California Santa Cruz: Carrie Pomeroy'))%>%
    bind_rows(dises_to_add %>% mutate(new_org='California State Polytechnic University Humboldt: Sean Craig'))%>%
    bind_rows(dises_to_add %>% mutate(new_org='California State Polytechnic University Humboldt: Laurie Richmond'))
  
  ###################################
  
  pisco_to_add <- filter(data, org_name=="Partnership for Interdisciplinary Studies of Coastal Oceans") %>%
    select(response_id)
  pisco_to_add %<>% mutate(new_org='Oregon State University') %>%
    bind_rows(pisco_to_add %>% mutate(new_org='Stanford University - Hopkins Marine Station')) %>%
    bind_rows(pisco_to_add %>% mutate(new_org='University of California Santa Barbara')) %>%
    bind_rows(pisco_to_add %>% mutate(new_org='University of California Santa Cruz'))
  
  #######################################
  divers_to_add <- filter(data, org_name=="Recreational and Commercial Divers") %>% 
    select(response_id)
  divers_to_add %<>% mutate(new_org='Recreational Divers') %>%
    bind_rows(divers_to_add %>% mutate(new_org='Commercial Divers'))
  
  divers_to_add2 <- filter(data, org_name=="California Sea Urchin Commission: Grant Downie") %>%
    select(response_id)
  divers_to_add2 %<>% mutate(new_org="California Sea Urchin Commission: Grant Downie") %>%
    bind_rows(divers_to_add %>% mutate(new_org='Commercial Diver: Grant Downie'))
  #######################################
  out <- data %>% bind_rows(dises_to_add,pisco_to_add, divers_to_add) %>% select(response_id,new_org)
}

expand_org_names_sub <- function(x){
  if(x=="Opc SeaGrant cdfw + KRMP"){
    return("California Ocean Protection Council,California Sea Grant,CDFW Kelp Restoration and Management Plan")
  } else if(x=='PISCO'){
    return("Partnership for Interdisciplinary Studies of Coastal Oceans,Oregon State University,Stanford University - Hopkins Marine Station,University of California Santa Barbara,University of California Santa Cruz")
  } else if(x=='Kelp Restoration as an Integrated Socio-Ecological System'){
    return('University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System,University of California Davis: Marissa Baskett,University of California Davis: Tyler Scott,University of California Davis: Mike Springborn,University of California Davis - Bodega Marine Laboratory: John Largier,University of California Santa Cruz: Carrie Pomeroy,University of California Santa Cruz: Mark Carr,California State Polytechnic University Humboldt: Sean Craig,California State Polytechnic University Humboldt: Laurie Richmond')
  } else if(x=='Ocean Protection Council; Greater Farallones Assoc.'){
    return("California Ocean Protection Council,Greater Farallones Assoc and NMS")
  }else if(x=="MBNMS, GFNMS/GFA, TNC, MLML, RCCA, SSU, Watermen's Alliance, G2KR, OPC"){
    return("NOAA Monterey Bay National Marine Sanctuary,Greater Farallones Assoc and NMS,The Nature Conservancy,Moss Landing Marine Laboratories,Reef Check,Sonoma State University,Watermen's Alliance,Giant Giant Kelp Restoration Project,California Ocean Protection Council")
  }
}