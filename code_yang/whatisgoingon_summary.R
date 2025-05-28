library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(paletteer)

dat_survey <- read_csv("data_yang/dat_kelp_survey.csv")[-c(1,2),]

# Question 1 -----------

question_1 <- dat_survey %>% 
  select(Q1)

question_1 <- question_1 %>%
  filter(!is.na(Q1))

question_1 <- question_1 %>%
  add_column(add_column = "%")

question_1 <- question_1 %>% 
  rename(
    label = add_column
  )

question_1 <- question_1 %>%
  count(Q1) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

question_1 %>%
  mutate(response = factor(Q1, c('Kelp forest-related issues are the primary thing I am involved in', 'I am routinely involved in kelp forest-related issues', 'I am occasionally involved in kelp forest-related issues', 'I have no involvement in kelp forest-related issues'))) %>%
  ggplot(., aes(x = fct_rev(response), y = n, fill = response)) +
  geom_col() +
  scale_x_discrete(labels = c('None', 'Occassional', 'Routine', 'Primary')) +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby") +
  guides(fill=guide_legend(title=" ")) +
  labs(
    title = "Question 1: Level of involvement in kelp forest-related issues",
    x = " ",
    y = "Number of Responses"
  ) +
  theme_minimal()


# Question 2 ------------

question_2 <- dat_survey %>% 
  select(Q2)

question_2 <- question_2 %>%
  filter(!is.na(Q2))

question_2 <- question_2 %>%
  add_column(add_column = "%")

question_2 <- question_2 %>% 
  rename(
    label = add_column
  )

question_2 <- question_2 %>%
  count(Q2) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

ggplot(question_2, aes(x = Q2, y = n, fill = Q2)) +
  geom_col() +
  scale_x_discrete(labels = c("Own", "Single", "Multiple")) +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby") +
  guides(fill=guide_legend(title=" ")) +
  labs(
    title = "Question 2: Involvement to who?",
    x = " ",
    y = "Number of Responses"
  ) +
  theme_minimal()


# Question 3 (in progress) ----------
nodelist <- read_csv("for_marissa/for_marissa - nodelist.csv")[-c(1,2),]

nodelist %>%
   count(organization_label) %>%
   mutate(percentage = round(n / sum(n) * 100, 1)) %>%
   ggplot(aes(x = reorder(organization_label, percentage), y = percentage, fill = organization_label)) +
   geom_col(show.legend = FALSE) +
   geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1) +
   coord_flip() +
   labs(
      title = "Percentage of Survey Responses by Organization Type",
      x = "Organization Type",
      y = "Percentage of Responses"
   ) +
   theme_minimal() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Question 4 --------
question_4 <- dat_survey %>%
  mutate(Q4_2 = case_when(
    ResponseId == "R_50sTP7MIe3Y7drH" ~ "Environmental management",
    ResponseId == "R_3CCIZ5j7PiHSIsp" ~ "Environmental management",
    ResponseId == "R_6MY7AcdWIwKItCE" ~ "Environmental management",
    ResponseId == "R_1DOJ10bxtj6sNhY" ~ "Environmental management",
    ResponseId == "R_3Hqxere4rGNArPa" ~ "Environmental management",
    ResponseId == "R_1eDbXLR4l7rE6wZ" ~ "Environmental management",
    ResponseId == "R_56yWtIgrU0bJwpX" ~ "Environmental management",
    ResponseId == "R_5p0hFxU61rtshee" ~ "Environmental management",
    TRUE ~ Q4_2))

question_4 <- question_4 %>%
  mutate(Q4_3 = case_when(
    ResponseId == "R_73yGa8USaSXYR0T" ~ "People management",
    ResponseId == "R_3Hqxere4rGNArPa" ~ "People management",
    TRUE ~ Q4_3)) 

question_4 <- question_4 %>%
  mutate(Q4_4 = case_when(
    ResponseId == "R_5dLYySNPGDpX0fo" ~ "Research",
    ResponseId == "R_3V7Q2T5Rvt15jcF" ~ "Research",
    ResponseId == "R_3H4Mz7SlkUD54nD" ~ "Research",
    ResponseId == "R_6aqSOPIVqzbD1KW" ~ "Research",
    TRUE ~ Q4_4))

question_4 <- question_4 %>%
  mutate(Q4_5 = case_when(
    ResponseId == "R_7upUSPV19KS9FVh" ~ "Advocacy or outreach",
    ResponseId == "R_61dpMIWMs89lZLY" ~ "Advocacy or outreach",
    ResponseId == "R_70XAbTfFsJfV3ix" ~ "Advocacy or outreach",
    TRUE ~ Q4_5))

question_4 <- question_4 %>%
  mutate(Q4_6 = case_when(
    ResponseId == "R_56D6mBrAXbvzaBX" ~ "Recreation or tourism",
    ResponseId == "R_3UWJ3j85Z8A7G8N" ~ "Recreation or tourism",
    TRUE ~ Q4_6))

question_4 <- question_4 %>% 
  select(starts_with("Q4"))

question_4 <- question_4 %>%
  pivot_longer(cols = -c(Q4_11, Q4_11_TEXT), names_to = "Q4", values_to = "choice")

question_4 <- question_4 %>%
  filter(!is.na(choice))

question_4 <- question_4 %>%
  count(choice) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

question_4 <- question_4 %>%
   add_column(add_column = "%")

question_4 <- question_4 %>% 
   rename(
      label = add_column
   )

question_4 %>%
  mutate(response = factor(choice, c('Research', 'Environmental management', 'Advocacy or outreach', 'Harvesting or fishing or growing', 'Recreation or tourism', 'Planning or permitting', 'People management', 'Business operations', 'Student'))) %>%
  ggplot(., aes(x = fct_rev(response), y = n, fill = response)) +
  geom_col() +
  geom_text(aes(label = paste0(percentage, "%")), vjust = 0, hjust = -0.1) +
  paletteer::scale_fill_paletteer_d("RColorBrewer::RdYlBu") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  guides(fill=guide_legend(title=" ")) +
  labs(
    title = "Question 4: Type of involvement",
    x = " ",
    y = "Number of Responses"
  ) +
  theme_minimal()

# How many people chose 2+ answers
dat_survey %>%
   mutate(num_selected = rowSums(!is.na(across(Q4_1:Q4_11_TEXT)))) %>%
   filter(num_selected >= 2) %>%
   summarise(count = n())
# ANSWER = 126 people worked in more than one county 

# Question 5 -------
question_5 <- dat_survey %>%
  mutate(Q5_3 = case_when(
    Q5_1 == "(all counties)" ~ "",
    TRUE ~ Q5_3))

question_5 <- question_5 %>%
  mutate(Q5_16 = case_when(
    Q5_1 == "(all counties)" ~ "",
    TRUE ~ Q5_16))

question_5 <- question_5 %>% 
  select(starts_with("Q5"))

question_5 <- question_5 %>%
  pivot_longer(cols = -c(Q5_17, Q5_17_TEXT), names_to = "Q5", values_to = "county")

question_5 <- question_5[!(is.na(question_5$county) | question_5$county==""), ]

question_5 <- question_5 %>%
   count(county) %>%
   mutate(percentage = round(n / sum(n) * 100, 1))

question_5 <- question_5 %>%
   add_column(add_column = "%")

question_5 <- question_5 %>% 
   rename(
      label = add_column
   )

question_5 <- question_5 %>%
  mutate(county = case_when(
    county == "Del Notre" ~ "Del Norte",
    TRUE ~ county))

question_5[ , 'region'] = ""

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Ventura", paste(region, "south"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "San Diego", paste(region, "south"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Orange", paste(region, "south"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Los Angeles", paste(region, "south"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Santa Barbara", paste(region, "central"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "San Luis Obispo", paste(region, "central"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Monterey", paste(region, "central"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Santa Cruz", paste(region, "central"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Sonoma", paste(region, "north"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "San Mateo", paste(region, "north"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "San Francisco", paste(region, "north"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Mendocino", paste(region, "north"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Marin", paste(region, "north"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Humboldt", paste(region, "north"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "Del Norte", paste(region, "north"), region))

question_5 <- question_5 %>%
  mutate(region = if_else(county == "(all counties)", paste(region, "all counties"), region))

# colour of county by region
question_5 %>%
   mutate(county = factor(county, c("Del Norte", "Humboldt", "Mendocino", "Sonoma", "Marin", "San Francisco", "San Mateo", "Santa Cruz", "Monterey", "San Luis Obispo", "Santa Barbara", "Ventura", "Los Angeles", "Orange", "San Diego", "(all counties)"))) %>%
   ggplot(., aes(x = fct_rev(county), y = n, fill = region)) +
   geom_col() +
   geom_text(aes(label = paste0(percentage, "%")), vjust = 0.5, hjust = -0.1) +
   paletteer::scale_fill_paletteer_d("ggthemes::excel_Median") +
   coord_flip() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   guides(fill=guide_legend(title=" ")) +
   labs(
      title = "Question 5: County affiliation",
      x = " ",
      y = "Number of Responses"
   ) +
   theme_minimal()

# binned by region
question_5.1 <- question_5 %>%
   group_by(region) %>%
   summarise(total_percentage = sum(percentage, na.rm = TRUE)) %>%
   ungroup()

question_5.1 <- question_5.1 %>%
   mutate(region = case_when(
      region == " north" ~ "north",
      TRUE ~ region))

question_5.1 <- question_5.1 %>%
   mutate(region = case_when(
      region == " central" ~ "central",
      TRUE ~ region))

question_5.1 <- question_5.1 %>%
   mutate(region = case_when(
      region == " south" ~ "south",
      TRUE ~ region))

question_5.1 <- question_5.1 %>%
   mutate(region = case_when(
      region == " all counties" ~ "all counties",
      TRUE ~ region))

   question_5.1 %>%
      mutate(region = factor(region, levels = c('north', 'central', 'south', 'all counties'))) %>%
      ggplot(aes(x = fct_rev(region), y = total_percentage, fill = region)) +
      geom_col() +
      geom_text(aes(label = paste0(total_percentage, "%")), vjust = 0.5, hjust = -0.1) +
      paletteer::scale_fill_paletteer_d("ggthemes::excel_Median") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      guides(fill = guide_legend(title = " ")) +
      labs(
         title = "Question 5: County affiliation",
         x = " ",
         y = "Proportion"
      ) +
      theme_minimal()
   
# How many people chose 2+ answers + "all counties"
dat_survey %>%
      mutate(num_selected = rowSums(!is.na(across(Q5_1:Q5_16)))) %>%
      filter(num_selected >= 2) %>%
      summarise(count = n())
 # 96
dat_survey %>%
   filter(if_any(Q5_1:Q5_16, ~ . == "(all counties)")) %>%
   summarise(count = n())
 # 26
# ANSWER = 122 people worked in more than one county 

# Question 6 -----
question_6 <- dat_survey %>% 
  select(starts_with("Q6"))

question_6 <- question_6 %>%
  filter(!is.na(Q6_1)) %>%
  filter(!is.na(Q6_2))

question_6 <- question_6 %>%
  pivot_longer(cols = c(Q6_1, Q6_2), names_to = "question", values_to = "response")

ggplot(question_6, aes(x = question, fill = response)) + 
  geom_bar() +
  guides(fill = guide_legend(title = "Legend")) +
  scale_x_discrete(labels=c('Short-term', 'Long-term')) +
  ggtitle("Question 6: How informed do you feel about the 
          short-term and long-term risks to kelp forests 
          in California?") +
  paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby") +
  xlab("") +
  ylab("") +
  guides(fill=guide_legend(title=""))
  

# Question 7 -----
question_7 <- dat_survey %>% 
  select(starts_with("Q7"))

question_7 <- question_7 %>%
  filter(!is.na(Q7_1)) %>%
  filter(!is.na(Q7_2))

question_7 <- question_7 %>%
  pivot_longer(cols = c(Q7_1, Q7_2), names_to = "question", values_to = "response")

ggplot(question_7, aes(x = question, fill = response)) + 
  geom_bar() +
  guides(fill = guide_legend(title = "Legend")) +
  scale_x_discrete(labels=c('Short-term', 'Long-term')) +
  ggtitle("Question 7: How concerned do you feel about the 
          short-term and long-term risks to kelp forests 
          in California?") +
  paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby") +
  xlab("") +
  ylab("") +
  guides(fill=guide_legend(title=""))

# Question 8 ------
question_8 <- dat_survey %>% 
  select(starts_with("Q8"))

question_8 <- question_8 %>%
  pivot_longer(cols = c(Q8_1, Q8_2, Q8_3, Q8_4, Q8_5, Q8_6, Q8_7, Q8_8), names_to = "question", values_to = "response")

question_8 <- question_8 %>%
  mutate(response = case_when(
    response == "Somewhat disgree" ~ "Somewhat disagree",
    TRUE ~ response))

question_8 <- question_8 %>%
  filter(!is.na(response))

question_8 <- question_8 %>%
   mutate(statement = case_when(
      question == "Q8_1" ~ "resist",
      question == "Q8_2" ~ "accept",
      question == "Q8_3" ~ "direct",
      question == "Q8_4" ~ "resist ",
      question == "Q8_5" ~ "direct ",
      question == "Q8_6" ~ "low risk low reward",
      question == "Q8_7" ~ "high risk high reward",
      question == "Q8_8" ~ "learn more"
   ))

# stacked barchart
question_8 %>%
  mutate(question = factor(question, c('Q8_8', 'Q8_7', 'Q8_6', 'Q8_5', 'Q8_3', 'Q8_2', 'Q8_4', 'Q8_1'))) %>%
  mutate(response = factor(response, c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree", "Don't know"))) %>%
  ggplot(., aes(x = question, fill = response)) + 
  geom_bar() + 
  guides(fill = guide_legend(title = "Legend")) +
  ggtitle("Question 8: How much you agree or disagree with the following statements 
                   about managing kelp forests") +
  paletteer::scale_fill_paletteer_d("LaCroixColoR::PeachPear", direction = 1) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0)) +
  scale_x_discrete(labels=c('Learn More', 'High Risk High Reward', 'Low Risk Low Reward', 'Direct', 'Direct', 'Accept', 'Resist', 'Resist')) +
  xlab("") +
  ylab("") +
  guides(fill=guide_legend(title=""))

# grouped barchart
question_8 <- question_8 %>%
   mutate(
      response = factor(response, levels = c(
         "Strongly agree", "Somewhat agree", "Neither agree nor disagree", 
         "Somewhat disagree", "Strongly disagree", "Don't know"
      )),
      statement = factor(statement, levels = c(
         "resist", "resist ", "accept", "direct", "direct ", "low risk low reward", 
         "high risk high reward", "learn more"
      ))
   )
  
ggplot(question_8, aes(x = statement, fill = response)) +
   geom_bar(position = "dodge") +
   labs(
      title = "Question 8: How much you agree or disagree with the following statements 
                   about managing kelp forests",
      x = " ",
      y = "Count",
      fill = "Response"
   ) +
   paletteer::scale_fill_paletteer_d("LaCroixColoR::PeachPear", direction = 1) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Question 9 ------
question_9 <- dat_survey %>% 
  select(starts_with("Q9"))

question_9 <- question_9 %>%
  mutate(Q9_6 = case_when(
    Q9_10_TEXT == "50 years experience, air lift opperator, active now" ~ "I directly observe conditions",
    Q9_10_TEXT == "I study my restoration projects" ~ "I directly observe conditions",
    TRUE ~ Q9_6))

question_9 <- question_9 %>%
  mutate(Q9_7 = case_when(
    Q9_10_TEXT == "I study my restoration projects" ~ "I collect or analyze data",
    TRUE ~ Q9_7))

question_9 <- question_9 %>%
  mutate(Q9_8 = case_when(
    Q9_10_TEXT == "I study my restoration projects" ~ "I read scientific publications",
    TRUE ~ Q9_8))

question_9 <- question_9 %>%
  mutate(Q9_9 = case_when(
    Q9_10_TEXT == "Kelp restoration groups" ~ "I talk to managers or regulators",
    TRUE ~ Q9_9))

question_9 <- question_9 %>%
  pivot_longer(cols = -c(Q9_10, Q9_10_TEXT), names_to = "question", values_to = "response")

question_9 <- question_9 %>%
   filter(!is.na(response))

question_9 <- question_9 %>%
   count(response) %>%
   mutate(percentage = round(n / sum(n) * 100, 1))

question_9 <- question_9 %>%
   add_column(add_column = "%")

question_9 <- question_9 %>% 
   rename(
      label = add_column
   )

group.colors.9 <- c('I directly observe conditions' = "#A44122", 'I attend meetings or workshops' = "#DB6725", 'I talk to scientists' ="#F28A32", 'I read scientific publications' = "#F0AE76", 'I collect or analyze data' = "#D6D4C9", 'I follow news, social media, or online forums' = "#A5BDCF", 'I talk to managers or regulators' = "#72A7CF", 'I use websites and data dashboards' = '#5C8FBB', 'I talk to fishers, harvesters, or growers' = "#3F709E")

question_9 %>%
   ggplot(aes(x = fct_reorder(response, n), y = n, fill = response)) +
   geom_col() +
   geom_text(aes(label = paste0(percentage, "%")), vjust = 0, hjust = -0.1) +
   scale_fill_manual(values=group.colors.9) +
   coord_flip() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   guides(fill = guide_legend(title = " ")) +
   labs(
      title = "Question 9: Main ways you learn about kelp forest-related issues",
      x = " ",
      y = "Number of Responses"
   ) +
   theme_minimal() +
   theme(legend.position = "none") 


# Question 10 ---------
question_10 <- dat_survey %>% 
  select(starts_with("Q10"))

question_10 <- question_10 %>%
  mutate(Q10_8 = case_when(
    Q10_10_TEXT == "improving natural history understanding of scientists" ~ "Promoting alternative forms of science (e.g. Indigenous knowledge)",
    Q10_10_TEXT == "Involve the 10K years of knowledge on the system held by tribes." ~ "Promoting alternative forms of science (e.g. Indigenous knowledge)",
    TRUE ~ Q10_8))

question_10 <- question_10 %>%
  pivot_longer(cols = -c(Q10_10, Q10_10_TEXT), names_to = "question", values_to = "response")

question_10 <- question_10 %>%
   filter(!is.na(response))

question_10 <- question_10 %>%
   count(response) %>%
   mutate(percentage = round(n / sum(n) * 100, 1))

question_10 <- question_10 %>%
   add_column(add_column = "%")

question_10 <- question_10 %>% 
   rename(
      label = add_column
   )

group.colors.10 <- c('Increasing direct relationships between scientists and non-scientists' = "#DB6725", 'Increasing the participation of scientists in policy efforts' ="#F28A32", 'Making research that is more management-relevant' = "#F0AE76", 'Communicating scientific results in simpler language' = "#D6D4C9", 'Promoting alternative forms of science (e.g. Indigenous knowledge)' = "#A5BDCF", 'Making it easier for the public to access scientific products' = "#72A7CF", 'Increasing public familiarity with scientific tools (e.g. electronic simulation maps)' = '#5C8FBB')

question_10 %>%
   ggplot(aes(x = fct_reorder(response, n), y = n, fill = response)) +
   geom_col() +
   geom_text(aes(label = paste0(percentage, "%")), vjust = 0, hjust = -0.1) +
   scale_fill_manual(values=group.colors.10) +
   coord_flip() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   guides(fill = guide_legend(title = " ")) +
   labs(
      title = "Question 10: Main ways to improve integration of science 
          and policy for managing kelp forests",
      x = " ",
      y = "Number of Responses"
   ) +
   theme_minimal() +
   theme(legend.position = "none")

# Question 14 -------
question_14 <- dat_survey %>% 
  select(starts_with("Q14"), starts_with("ResponseId"))

question_14 <- question_14 %>%
  mutate(Q14_1 = case_when(
    Q14_9_TEXT == "Funding that covers multiple collaborations, lack of funding flexiblity, diving regulations" ~ "Limited resources/staff time",
    TRUE ~ Q14_1))

question_14 <- question_14 %>%
  mutate(Q14_1 = case_when(
    ResponseId == "R_5dMGo8yjNaUViDk" ~ "Limited resources/staff time",
    TRUE ~ Q14_1))

question_14 <- question_14 %>%
  mutate(Q14_1 = case_when(
    Q14_9_TEXT == "Non-profit focuse on diver only solution to urchin removal priority,area selection and salary pay" ~ "Limited resources/staff time",
    TRUE ~ Q14_1))

question_14 <- question_14 %>%
  mutate(Q14_1 = case_when(
    Q14_9_TEXT == "Actual Distance to travel" ~ "Limited resources/staff time",
    TRUE ~ Q14_1))

question_14 <- question_14 %>%
  mutate(Q14_3 = case_when(
    Q14_9_TEXT == "Fear of alienating shellfish fishers by discussion of sea otter reintroduction as a potential solution" ~ "Distrust among potential partners",
    TRUE ~ Q14_3))

question_14 <- question_14 %>%
  mutate(Q14_4 = case_when(
    Q14_9_TEXT == "Lack of broader (beyond CA) knowledge" ~ "Lack of adequate scientific information",
    TRUE ~ Q14_4))

question_14 <- question_14 %>%
  mutate(Q14_7 = case_when(
    Q14_9_TEXT == "Lack of urgency & public awareness" ~ "Lack of public support",
    TRUE ~ Q14_7))

question_14 <- question_14 %>%
  mutate(Q14_8 = case_when(
    Q14_9_TEXT == "Lack of shared long term vision/goals" ~ "Lack of suitable partners",
    TRUE ~ Q14_8))

question_14 <- question_14 %>%
  pivot_longer(cols = -c(Q14_9, Q14_9_TEXT, ResponseId), names_to = "question", values_to = "response")

question_14 <- question_14[!(is.na(question_14$response) | question_14$response==""), ]

question_14 <- question_14 %>%
   count(response) %>%
   mutate(percentage = round(n / sum(n) * 100, 1))

question_14 <- question_14 %>%
   add_column(add_column = "%")

question_14 <- question_14 %>% 
   rename(
      label = add_column
   )

group.colors.14 <- c('Limited resources/staff time' = "#A44122", 'Regulatory obstacles' = "#DB6725", 'Lack of an overarching plan' ="#F28A32", 'Lack of adequate scientific information' = "#F0AE76", 'Distrust among potential partners' = "#A5BDCF", 'Lack of public support' = "#72A7CF", 'Lack of experience' = "#5C8FBB", 'Lack of suitable partners' = "#3F709E")

question_14 %>%
   ggplot(aes(x = fct_reorder(response, n), y = n, fill = response)) +
   geom_col() +
   geom_text(aes(label = paste0(percentage, "%")), vjust = 0, hjust = -0.1) +
   scale_fill_manual(values=group.colors.14) +
   coord_flip() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   guides(fill = guide_legend(title = " ")) +
   labs(
      title = "Question 14: Biggest barriers to partnering with others on kelp 
                     forest-related issues",
      x = " ",
      y = "Number of Responses"
   ) +
   theme_minimal() +
   theme(legend.position = "none")

# Question 15 --------
question_15 <- dat_survey %>% 
  select(starts_with("Q15"))

question_15 <- question_15 %>%
  pivot_longer(cols = -c(Q15_9, Q15_9_TEXT), names_to = "question", values_to = "response")

question_15 <- question_15 %>%
  filter(!is.na(response))

question_15 <- question_15 %>%
   count(response) %>%
   mutate(percentage = round(n / sum(n) * 100, 1))

question_15 <- question_15 %>%
   add_column(add_column = "%")

question_15 <- question_15 %>% 
   rename(
      label = add_column
   )

group.colors.15 <- c('Shared goals' = "#A44122", 'Reputation and trustworthiness' = "#DB6725", 'Prior experience' ="#F28A32", 'Funding opportunities' = "#F0AE76", 'Complementary activities' = "#A5BDCF", 'Authority to make decisions' = "#72A7CF", 'Ability to reach a broader network' = '#5C8FBB', 'Access to information' = "#3F709E")

question_15 %>%
   ggplot(aes(x = fct_reorder(response, n), y = n, fill = response)) +
   geom_col() +
   geom_text(aes(label = paste0(percentage, "%")), vjust = 0, hjust = -0.1) +
   scale_fill_manual(values=group.colors.15) +
   coord_flip() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   guides(fill = guide_legend(title = " ")) +
   labs(
      title = "Question 15: Most important factors when choosing partners",
      x = " ",
      y = "Number of Responses"
   ) +
   theme_minimal() +
   theme(legend.position = "none")

  
