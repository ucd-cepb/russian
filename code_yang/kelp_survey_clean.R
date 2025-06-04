library(readr)
library(tidyverse)
library(dplyr)

# File paths
input_file <- "data_yang/dat_kelp_survey.csv"
output_file <- "data_yang/kelp_survey_clean.csv"

# Configuration
REGION_MAPPINGS <- list(
  south = c("Ventura", "San Diego", "Orange", "Los Angeles"),
  central = c("Santa Barbara", "San Luis Obispo", "Monterey", "Santa Cruz"),
  north = c("Sonoma", "San Mateo", "San Francisco", "Mendocino", "Marin", "Humboldt", "Del Norte")
)

# Helper Functions
clean_involvement_level <- function(data) {
  data %>%
    mutate(
      involvement_level = case_when(
        Q1 == "Kelp forest-related issues are the primary thing I am involved in" ~ "Primary",
        Q1 == "I am routinely involved in kelp forest-related issues" ~ "Routine",
        Q1 == "I am occasionally involved in kelp forest-related issues" ~ "Occasional",
        Q1 == "I have no involvement in kelp forest-related issues" ~ "None",
        TRUE ~ NA_character_
      )
    )
}

clean_involvement_type <- function(data) {
  data %>%
    mutate(
      involvement_type = case_when(
        Q2 == "I am involved on my own" ~ "Own",
        Q2 == "I am involved with a single organization" ~ "Single",
        Q2 == "I am involved with multiple organizations" ~ "Multiple",
        TRUE ~ NA_character_
      )
    )
}

clean_involvement_activities <- function(data) {
  data %>%
    mutate(
      Q4_2 = case_when(
        ResponseId == "R_50sTP7MIe3Y7drH" ~ "Environmental management",
        ResponseId == "R_3CCIZ5j7PiHSIsp" ~ "Environmental management",
        ResponseId == "R_6MY7AcdWIwKItCE" ~ "Environmental management",
        ResponseId == "R_1DOJ10bxtj6sNhY" ~ "Environmental management",
        ResponseId == "R_3Hqxere4rGNArPa" ~ "Environmental management",
        ResponseId == "R_1eDbXLR4l7rE6wZ" ~ "Environmental management",
        ResponseId == "R_56yWtIgrU0bJwpX" ~ "Environmental management",
        ResponseId == "R_5p0hFxU61rtshee" ~ "Environmental management",
        TRUE ~ Q4_2
      ),
      Q4_3 = case_when(
        ResponseId == "R_73yGa8USaSXYR0T" ~ "People management",
        ResponseId == "R_3Hqxere4rGNArPa" ~ "People management",
        TRUE ~ Q4_3
      ),
      Q4_4 = case_when(
        ResponseId == "R_5dLYySNPGDpX0fo" ~ "Research",
        ResponseId == "R_3V7Q2T5Rvt15jcF" ~ "Research",
        ResponseId == "R_3H4Mz7SlkUD54nD" ~ "Research",
        ResponseId == "R_6aqSOPIVqzbD1KW" ~ "Research",
        TRUE ~ Q4_4
      ),
      Q4_5 = case_when(
        ResponseId == "R_7upUSPV19KS9FVh" ~ "Advocacy or outreach",
        ResponseId == "R_61dpMIWMs89lZLY" ~ "Advocacy or outreach",
        ResponseId == "R_70XAbTfFsJfV3ix" ~ "Advocacy or outreach",
        TRUE ~ Q4_5
      ),
      Q4_6 = case_when(
        ResponseId == "R_56D6mBrAXbvzaBX" ~ "Recreation or tourism",
        ResponseId == "R_3UWJ3j85Z8A7G8N" ~ "Recreation or tourism",
        TRUE ~ Q4_6
      )
    )
}

clean_county_region <- function(data) {
  data %>%
    mutate(
      # This code block determines the value of the 'counties' variable based on survey responses Q5_1 to Q5_16.
      # It assigns "All counties" if Q5_1 is "(all counties)".
      # Otherwise, it assigns the first non-missing value from Q5_1 to Q5_16.
      # Using 'coalesce' from the dplyr package simplifies the code and works as intended:
      counties = if_else(
        Q5_1 == "(all counties)", 
        "All counties", 
        dplyr::coalesce(Q5_1, Q5_2, Q5_3, Q5_4, Q5_5, Q5_6, Q5_7, Q5_8, Q5_9, Q5_10, Q5_11, Q5_12, Q5_13, Q5_14, Q5_15, Q5_16)
      ),
      region = case_when(
        counties %in% REGION_MAPPINGS$south ~ "South",
        counties %in% REGION_MAPPINGS$central ~ "Central",
        counties %in% REGION_MAPPINGS$north ~ "North",
        counties == "All counties" ~ "All regions",
        TRUE ~ NA_character_
      )
    )
}

clean_management_responses <- function(data) {
  data %>%
    rename(
      short_term_info = Q6_1,
      long_term_info = Q6_2,
      short_term_concern = Q7_1,
      long_term_concern = Q7_2,
      resist_management = Q8_1,
      accept_management = Q8_2,
      direct_management = Q8_3,
      resist_management_alt = Q8_4,
      direct_management_alt = Q8_5,
      low_risk_reward = Q8_6,
      high_risk_reward = Q8_7,
      learn_more = Q8_8
    )
}

clean_learning_methods <- function(data) {
  data %>%
    mutate(
      Q9_1 = if_else(!is.na(Q9_1), "Direct observation", Q9_1),
      Q9_2 = if_else(!is.na(Q9_2), "Meetings/workshops", Q9_2),
      Q9_3 = if_else(!is.na(Q9_3), "Talk to scientists", Q9_3),
      Q9_4 = if_else(!is.na(Q9_4), "Read publications", Q9_4),
      Q9_5 = if_else(!is.na(Q9_5), "Collect/analyze data", Q9_5),
      Q9_6 = if_else(!is.na(Q9_6), "News/social media", Q9_6),
      Q9_7 = if_else(!is.na(Q9_7), "Talk to managers", Q9_7),
      Q9_8 = if_else(!is.na(Q9_8), "Use websites/dashboards", Q9_8),
      Q9_9 = if_else(!is.na(Q9_9), "Talk to fishers", Q9_9)
    )
}

clean_integration_methods <- function(data) {
  data %>%
    mutate(
      Q10_1 = if_else(!is.na(Q10_1), "Direct relationships", Q10_1),
      Q10_2 = if_else(!is.na(Q10_2), "Policy participation", Q10_2),
      Q10_3 = if_else(!is.na(Q10_3), "Management-relevant research", Q10_3),
      Q10_4 = if_else(!is.na(Q10_4), "Simpler language", Q10_4),
      Q10_5 = if_else(!is.na(Q10_5), "Alternative science", Q10_5),
      Q10_6 = if_else(!is.na(Q10_6), "Public access", Q10_6),
      Q10_7 = if_else(!is.na(Q10_7), "Familiarity with tools", Q10_7)
    )
}

clean_partnership_data <- function(data) {
  data %>%
    mutate(
      Q14_1 = if_else(!is.na(Q14_1), "Limited resources", Q14_1),
      Q14_2 = if_else(!is.na(Q14_2), "Regulatory obstacles", Q14_2),
      Q14_3 = if_else(!is.na(Q14_3), "No overarching plan", Q14_3),
      Q14_4 = if_else(!is.na(Q14_4), "Lack of information", Q14_4),
      Q14_5 = if_else(!is.na(Q14_5), "Distrust", Q14_5),
      Q14_6 = if_else(!is.na(Q14_6), "Lack of support", Q14_6),
      Q14_7 = if_else(!is.na(Q14_7), "Lack of experience", Q14_7),
      Q14_8 = if_else(!is.na(Q14_8), "Lack of partners", Q14_8),
      # Now apply custom recodings to overwrite as needed
      Q14_1 = case_when(
        Q14_9_TEXT == "Funding that covers multiple collaborations, lack of funding flexiblity, diving regulations" ~ "Limited resources/staff time",
        ResponseId == "R_5dMGo8yjNaUViDk" ~ "Limited resources/staff time",
        Q14_9_TEXT == "Non-profit focuse on diver only solution to urchin removal priority,area selection and salary pay" ~ "Limited resources/staff time",
        Q14_9_TEXT == "Actual Distance to travel" ~ "Limited resources/staff time",
        TRUE ~ Q14_1
      ),
      Q14_3 = case_when(
        Q14_9_TEXT == "Fear of alienating shellfish fishers by discussion of sea otter reintroduction as a potential solution" ~ "Distrust among potential partners",
        TRUE ~ Q14_3
      ),
      Q14_4 = case_when(
        Q14_9_TEXT == "Lack of broader (beyond CA) knowledge" ~ "Lack of adequate scientific information",
        TRUE ~ Q14_4
      ),
      Q14_7 = case_when(
        Q14_9_TEXT == "Lack of urgency & public awareness" ~ "Lack of public support",
        TRUE ~ Q14_7
      ),
      Q14_8 = case_when(
        Q14_9_TEXT == "Lack of shared long term vision/goals" ~ "Lack of suitable partners",
        TRUE ~ Q14_8
      )
    )
}

generate_data_summary <- function(data) {
  summary <- list(
    total_responses = nrow(data),
    missing_values = colSums(is.na(data)),
    unique_values = sapply(data, function(x) length(unique(x))),
    region_distribution = table(data$region),
    involvement_level_distribution = table(data$involvement_level)
  )
  return(summary)
}

# Main execution
# Read and clean data
dat_survey <- read_csv(input_file)[-c(1,2),]

# Apply cleaning functions
clean_survey <- dat_survey %>%
  clean_involvement_level() %>%
  clean_involvement_type() %>%
  clean_involvement_activities() %>%
  clean_county_region() %>%
  clean_management_responses() %>%
  clean_learning_methods() %>%
  clean_integration_methods() %>%
  clean_partnership_data()

# Generate and print summary
summary <- generate_data_summary(clean_survey)
print("Data Summary:")
print(summary)

# Save cleaned data
write_csv(clean_survey, output_file)
print(sprintf("Cleaned data saved to: %s", output_file)) 