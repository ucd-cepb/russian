library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(paletteer)

dat_survey <- read_csv("data_yang/dat_kelp_survey.csv")[-c(1,2),]

dat_survey$Q9_7
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
      title = "Types of Organizations Mentioned",
      x = "Organization Type",
      y = "Percentage of Responses"
   ) +
   theme_minimal() +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

nodelist %>%
   count(organization_label) %>%
   mutate(percentage = round(n / sum(n) * 100, 1)) %>%
   ggplot(aes(x = reorder(organization_label, n), y = n, fill = n)) + 
   geom_col(show.legend = FALSE) +
   geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1) +
   coord_flip() +
   labs(
      title = "Question 3: Types of organizations mentioned",
      x = "Organization Type",
      y = "Percentage of Responses"
   ) +
   scale_fill_gradient(low = "#cce5ff", high = "#004c99") +
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

# Question 4.1 Heatmap --------
question_4.1 <- dat_survey %>%
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

question_4.1 <- question_4.1 %>%
   mutate(Q4_3 = case_when(
      ResponseId == "R_73yGa8USaSXYR0T" ~ "People management",
      ResponseId == "R_3Hqxere4rGNArPa" ~ "People management",
      TRUE ~ Q4_3)) 

question_4.1 <- question_4.1 %>%
   mutate(Q4_4 = case_when(
      ResponseId == "R_5dLYySNPGDpX0fo" ~ "Research",
      ResponseId == "R_3V7Q2T5Rvt15jcF" ~ "Research",
      ResponseId == "R_3H4Mz7SlkUD54nD" ~ "Research",
      ResponseId == "R_6aqSOPIVqzbD1KW" ~ "Research",
      TRUE ~ Q4_4))

question_4.1 <- question_4.1 %>%
   mutate(Q4_5 = case_when(
      ResponseId == "R_7upUSPV19KS9FVh" ~ "Advocacy or outreach",
      ResponseId == "R_61dpMIWMs89lZLY" ~ "Advocacy or outreach",
      ResponseId == "R_70XAbTfFsJfV3ix" ~ "Advocacy or outreach",
      TRUE ~ Q4_5))

question_4.1 <- question_4.1 %>%
   mutate(Q4_6 = case_when(
      ResponseId == "R_56D6mBrAXbvzaBX" ~ "Recreation or tourism",
      ResponseId == "R_3UWJ3j85Z8A7G8N" ~ "Recreation or tourism",
      TRUE ~ Q4_6))


question_4.1 <- dat_survey %>%
   unite(col = "org_name", c(`Q3 Individual_1`, `Q3 Several_1`), sep = "", na.rm = TRUE, remove = FALSE)

question_4.1 <- question_4.1 %>%
   mutate(org_name = case_when(
      ResponseId == "R_7zc4m6dh9wc0YdK" ~ "California State Polytechnic University Humboldt",
      ResponseId == "R_1LLVDCfsYqnIq9H" ~ "Get Inspired",
      ResponseId == "R_7JW5UqsrkFfRVRL" ~ "California State University Long Beach",
      ResponseId == "R_7N1Uusw5TPiIBZ4" ~ "Reef Check",
      ResponseId == "R_5dnbJSY7qhuMdsB" ~ "University of California Santa Barbara",
      ResponseId == "R_38BizK7TnctWB0Z" ~ "Moss Landing Marine Laboratories",
      ResponseId == "R_6U4QGgoI5tIAT6r" ~ "Reef Check",
      ResponseId == "R_7DeczPHJmSjx6Tj" ~ "Reef Check",
      ResponseId == "R_1NqY8R9wV0B745S" ~ "Strategic Earth Consulting",
      ResponseId == "R_1OwaKPJzCpBm5QR" ~ "California State Polytechnic University Humboldt",
      ResponseId == "R_7oHQgfbxkVtFRfP" ~ "City College of San Francisco",
      ResponseId == "R_3jTauwChLV63T1f" ~ "Giant Giant Kelp Restoration Project",
      ResponseId == "R_5p0hFxU61rtshee" ~ "Giant Giant Kelp Restoration Project",
      ResponseId == "R_5DbUjzy5UEB6Bmv" ~ "Giant Giant Kelp Restoration Project",
      ResponseId == "R_1A45wn3R1qfgQo1" ~ "Individual",
      ResponseId == "R_1DIA6BHO7pKYU8x" ~ "Individual",
      ResponseId == "R_3REDy37W41gd9F7" ~ "Individual",
      TRUE ~ org_name))

question_4.1 <- question_4.1 %>%
   select(org_name, Q4_1, Q4_2, Q4_3, Q4_4, Q4_5, Q4_6, Q4_7, Q4_8, Q4_9, Q4_10)

question_4.1 <- question_4.1 %>%
   add_column(org_type = "")

question_4.1[question_4.1 == ""] <- NA 

question_4.1 <- question_4.1 %>%
   filter(!is.na(org_name))

# AltaSeads Conservancy
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Sergey Nuzhdin  USC/ Alta Seads spore bank" = "AltaSeads Conservancy: Sergey Nuzhdin"))

# Kelp Forest Alliance
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "The Kelp Forest Alliance (Eger)" = "Kelp Forest Alliance: Aaron Eger", "Aaron eger and many many others" = "Kelp Forest Alliance: Aaron Eger", "Kelp forest alliance" = "Kelp Forest Alliance"))

# Above/Below
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "New Agency / Above Below" = "Above/Below", "New Agency" = "Above/Below"))

# Sea Forest
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Alex Berger" = "Sea Forest: Alex Berger"))

# University of California Santa Cruz
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Ali Boutros (UCSC Coastal Science and Policy graduate program))" = "University of California Santa Cruz: Ali Boutros", "Andrea Paz Lacavex (UCSC)" = "University of California Santa Cruz: Andrea Paz Lacavex", "Calvin Munson (UCSC)" = "University of California Santa Cruz: Calvin Munson", "Carr Lab UCSC" = "University of California Santa Cruz - Raimondi-Carr Lab", "Carrie Pomeroy (UCSC)" = "University of California Santa Cruz: Carrie Pomeroy", "Dan Malone- UCSC" = "University of California Santa Cruz: Dan Malone", "Dr Pete raimondi" = "University of California Santa Cruz: Peter Raimondi", "Kristy Kroeker (UCSC)" = "University of California Santa Cruz: Kristy Kroeker", "Mark Carr- UCSC" = "University of California Santa Cruz: Mark Carr", "Mark Carr, UCSC" = "University of California Santa Cruz: Mark Carr", "Pete Raimondi (UCSC)" = "University of California Santa Cruz: Pete Raimondi", "UC Santa Cruz" = "University of California Santa Cruz", "UC Santa Cruz (Carr et al)" = "University of California Santa Cruz: Mark Carr", "UC Santa Cruz Long Marine Lab" = "Joseph M. Long Marine Laboratory", "UCSC" = "University of California Santa Cruz", "UCSC (Carr)" = "University of California Santa Cruz: Mark Carr", "University of California: Santa Cruz" = "University of California Santa Cruz", "US Santa Cruz" = "University of California Santa Cruz"))

# Amah Mutsun Land Trust
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Amah Mutsun" = "Amah Mutsun Land Trust"))

# Moss Landing Marine Laboratories
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Andrew kim" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim - MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim- MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim, MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Moss Landing Marine Labs Aquaculture Center" = "Moss Landing Marine Laboratories - Aquaculture Center", "Moss Landing marine labs" = "Moss Landing Marine Laboratories", "Moss landing marine labs" = "Moss Landing Marine Laboratories", "Moss Landing Marine Laboratory" = "Moss Landing Marine Laboratories", "Scott Hamilton, MLML Moss Landing Marine Laboratories" = "Moss Landing Marine Laboratories: Scott Hamilton", "Bennett Bugbee - MLML" = "Moss Landing Marine Laboratories: Bennett Bugbee", "Scott Hamilton (Moss Landing Marine Labs)" = "Moss Landing Marine Laboratories: Scott Hamilton", "Mike Graham scientist Moss Landing" = "Moss Landing Marine Laboratories: Mike Graham", "Moss Landing Marine Lab" = "Moss Landing Marine Laboratories", "SJSU Moss Landing Marine Labs - Scott Hamilton" = "Moss Landing Marine Laboratories: Scott Hamilton", "Moss landing" = "Moss Landing Marine Laboratories", "Scott Hamilton- MLML" = "Moss Landing Marine Laboratories: Scott Hamilton", "Bennet Bugbee- MLML" = "Moss Landing Marine Laboratories: Bennett Bugbee", "Bennett Bugbee, MLML" = "Moss Landing Marine Laboratories: Bennett Bugbee","Ashley Kidd" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory: Ashley Kidd", "https://www.sunflowerstarlab.org/" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Mike Graham, Moss Landing Marine Labs" = "Moss Landing Marine Labs: Mike Graham", "Scott hamilton" = "Moss Landing Marine Laboratories: Scott Hamilton", "SJSU Moss Landing Marine Labs - Scott Hamiltom" = "Moss Landing Marine Laboratories: Scott Hamilton", "Sunflower Sea Star Laboratory" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Sunflower Star Lab" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Vince Christian, Sunflower Star Laboratory, Moss Landing" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory: Vince Christian", "Sunflower Star Lab, Moss Landing" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Sunflower Star Laboratory" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "	
Moss Landing Marine Labs" = "Moss Landing Marine Laboratories")) 

# Florida State University
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Andrew Rassweiler, Florida State University" = "Florida State University: Andrew Rassweiler", "FSU" = "Florida State University")) 

# Greater Farallones National Marine Sanctuary
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "GFNMS" = "Greater Farallones National Marine Sanctuary", "GFNMS (Holman)" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "GFNMS Advisory Council" = "Greater Farallones National Marine Sanctuary - Greater Farallones National Marine Sanctuary Advisory Council", "Greater farallones" = "Greater Farallones National Marine Sanctuary", "Greater Farallones Sanctuary Advisory Council" = "Greater Farallones National Marine Sanctuary - Greater Farallones National Marine Sanctuary Advisory Council", "Greater Farrallones" = "Greater Farallones National Marine Sanctuary", "Greater Farralones" = "Greater Farallones National Marine Sanctuary", "Gulf of the Farallons National Marine Santuary, Angela Zepp" = "Greater Farallons National Marine Santuary: Angela Zepp", "Gulf of the Farralons National Marine Sanctuary" = "Greater Farallones National Marine Sanctuary", "NOAA Greater Farallones National Marine Sanctuary" = "Greater Farallones National Marine Sanctuary", "Rietta Holman - GFA" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Holman" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Holman - Greater Farallon Association" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Hoffman- GFA" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Hohman" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Hohman - Greater Farallon Association" = "Greater Farallones National Marine Sanctuary: Rietta Hohman"))

# Greater Farallones Association 
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "The Greater Farrallones Association" = "Greater Farallones Association", "The Greater Farallones Kelp Restoration Project (GFA)" = "Greater Farallones Association", "Angela Zepp- GFA" = "Greater Farallones Association: Angela Zepp", "Gina Contolini- GFA" = "Greater Farallones Association: Gina Contolini", "Greater Farallon Association-Rietta Hohman" = "Greater Farallon Association: Rietta Hohman", "Greater Farallones Association kelp team (Rietta Hohman)" = "Greater Farallon Association: Rietta Hohman", "Greater Farallones Association, Rietta Hohman" = "Greater Farallones Association: Rietta Hohman", "Greater Farrallons association" = "Greater Farallones Association", "Gulf of the Farallons Association" = "Greater Farallones Association", "Julieta Gomez- GFA" = "Greater Farallones Association: Julieta Gomez", "Kelp Restoration Team, Greater Farallones Association" = "Greater Farallones Association", "Gulf of the Farallones Association" = "Greater Farallones Association", "Tyler Mears- GFA" = "Greater Farallones Association: Tyler Mears"))

# University of California Santa Barbara
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Anita Giraldo Ospina (UCSB)" = "University of California Santa Barbara: Anita Giraldo Ospina", "Anita Giraldo-Ospino, UCSB" = "University of California Santa Barbara: Anita Giraldo Ospina", "Univ CA Santa Barbara" = "University of California Santa Barbara", "University of California, Santa Barbara" = "University of California Santa Barbara", "UCSB" = "University of California Santa Barbara", "Capt. Merit McCrea" = "University of California Santa Barbara: Merit McCrea", "Colleagues at UCSB and in New Zealand" = "University of California Santa Barbara", "Dan Reed (UCSB)" = "University of California Santa Barbara: Dan Reed", "Dr. Steve Schroder" = "University of California Santa Barbara: Stephen Schroeter", "Jennifer Caselle (UCSB)" = "University of California Santa Barbara: Jennifer Caselle", "Steve Schroeter, UCSB" = "University of California Santa Barbara: Steve Schroeter", "UC Santa Barbara" = "University of California Santa Barbara"))

# Reef Check
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Annie Bauer-Civiello" = "Reef Check: Annie Bauer-Civiello", "Annie Bauer-Civiello, Reef Check California" = "Reef Check: Annie Bauer-Civiello", "Annie Bauer-Civiello, ReefCheck" = "Reef Check: Annie Bauer-Civiello", "Reef" = "Reef Check", "reef check" = "Reef Check", "Reef check" = "Reef Check", "Reef Check California" = "Reef Check", "Reef Check Central California" = "Reef Check", "Reef Check Foundation" = "Reef Check", "Reef Check Ian Norton" = "Reef Check: Ian Norton", "Reef Check in Sonoma & Mendocino as volunteer" = "Reef Check", "Reef Check Kelp Forest Program" = "Reef Check", "Reef Check KFM" = "Reef Check", "Reef Check Morgan Murphy Cannella" = "Reef Check: Morgan Murphy-Cannella", "Dan Abbot, Reef Check" = "Reef Check: Dan Abbott", "Dan Abbott" = "Reef Check: Dan Abbott", "Jan freiwald" = "Reef Check: Jan Freiwald", "Jan Freiwald" = "Reef Check: Jan Freiwald", "Morgan Murphy-Canella Reefcheck" = "Reef Check: Morgan Murphy-Cannella", "RCCA Ian Norton- northern ca. coordinator" = "Reef Check: Ian Norton", "RCCA Morgan Murphy Canola- northrn CA coordinator" = "Reef Check: Morgan Murphy-Cannella", "Reef Check of California" = "Reef Check", "Reef Check So Cal region" = "Reef Check", "Reef Check Southern California" =  "Reef Check", "Reef check staff in Sonoma, Mendocino, and Monterey" = "Reef Check", "Reef Check, Dr. Jan Friewald" = "Reef Check: Jan Freiwald", "Reef Check, Jan Friewald" = "Reef Check: Jan Freiwald", "Reef Check, Morgan Murphy-Cannella" = "Reef Check: Morgan Murphy-Cannella", "Reefcheck" = "Reef Check", "ReefCheck" = "Reef Check", "Reefcheck CA"= "Reef Check", "ReefCheck CA" = "Reef Check", "ReefCheck; Jan Friewald" = "Reef Check: Jan Freiwald", "reef check ca." = "Reef Check", "Reef check so cal" = "Reef Check", "Reef Check, Jan Freiwald" = "Reef Check: Jan Freiwald", "Reef Check CA" = "Reef Check"))

# California Department of Fish and Wildlife
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Anthony Shiao, California Department of Fish and Wildlife" = "California Department of Fish and Wildlife: Anthony Shiao", "CDFW - Not sure region, dove off the R/V Garibaldi with Reef Check" = "California Department of Fish and Wildlife", "CA DFW" = "California Department of Fish and Wildlife", "CDFW" = "California Department of Fish and Wildlife", "Calif. Dept. of Fish and Wildlife" = "California Department of Fish and Wildlife", "California Department of Fish and Wildlife Marine Region" = "California Department of Fish and Wildlife - Marine Region", "California Department of Fisheries and Wildlife" = "California Department of Fish and Wildlife", "California Fish and Wildlife" = "California Department of Fish and Wildlife", "CDFW Marine Region" = "California Department of Fish and Wildlife - Marine Region", "CDFW out of San Pedro" = "California Department of Fish and Wildlife", "CDFW Region 7" = "California Department of Fish and Wildlife - Marine Region", "CDFW, Laura Rogers-Bennett" = "California Department of Fish and Wildlife: Laura Rogers-Bennett", "Dr. Laura Rogers-Bennett" = "California Department of Fish and Wildlife: Laura Rogers-Bennett", "Fish and Wildlife" = "California Department of Fish and Wildlife", "Ian Kelmartin, CDFW" = "California Department of Fish and Wildlife: Ian Kelmartin", "Kirsten Ramey, CDFW" = "California Department of Fish and Wildlife: Kirsten Ramey", "Kristen Elsmore" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore  DFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore - CDFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore (CDFW--kelp)" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore (CDFW)" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore, CDFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristin Elsmore, CDFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Mike Prall, CDFW" = "California Department of Fish and Wildlife: Mike Prall", "The California Department of Fish and Wildlife" = "California Department of Fish and Wildlife", "CALIFORNIA DEPARTMENT OF FISH AND WILDLIFE" = "California Department of Fish and Wildlife"))

# Montrose Settlements Restoration Program
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "NOAA Montrose Settlements Restoration Program" = "Montrose Settlements Restoration Program"))

# Santa Barbara Coastal Long Term Ecological Research
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "SBC LTER" = "Santa Barbara Coastal Long Term Ecological Research"))

# Anthropocene Institute
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Anthropogene Institute" = "Anthropocene Institute"))

# Monterey Bay Aquarium
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "April Ridlon (Monterey Bay Aquarium)" = "Monterey Bay Aquarium: April Ridlon", "Josh Smith (Monterey Bay Aquarium)" = "Monterey Bay Aquarium: Josh Smith", "Josh Smith, Monterey Bay Aquarium" = "Monterey Bay Aquarium: Josh Smith", "MB Aquarium" = "Monterey Bay Aquarium", "Monterey Bay Aquarium - Joshua Smith" = "Monterey Bay Aquarium: Josh Smith", "The Monterey Bay Aquarium" = "Monterey Bay Aquarium"))

# Monterey Bay National Marine Sanctuary
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "MBNMS" = "Monterey Bay National Marine Sanctuary", "MBNMS (Lonhart)" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "NOAA MBNMS" = "Monterey Bay National Marine Sanctuary", "NOAA MBNMS Sanctuary Advisory Council (I spoke recently at a meeting about kelp)" = "Monterey Bay National Marine Sanctuary - Monterey Bay National Marine Sanctuary Advisory Council", "Steve Lonhart" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "Steve Lonhart - Monterey Bay National Marine Sanctuary" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "Steve Lonhart (MBNMS)" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "Steve Lonhart, MBNMS" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "NOAA Monterey Bay National Marine Sanctuary" = "Monterey Bay National Marine Sanctuary", "Monterey Bay National Marine Sanctuary - Steve Lonhart" = "Monterey Bay National Marine Sanctuary: Steve Lonhart"))

# Monterey Bay Mermaid
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Monterey Bay Mermaid # Alison Smith" = "Monterey Bay Mermaid: Alison Smith"))

# Monterey Abalone Company
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Art Seavey, Monterey Abalone Company" = "Monterey Abalone Company: Art Seavey"))

# Monterey County Weekly
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "MC Weekly" = "Monterey County Weekly"))

# National Oceanic and Atmospheric Administration
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Bay Net" = "National Oceanic and Atmospheric Administration - Bay Net", "CA coastal national marine sanctuaries" = "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries - West Coast Region", "Cameron Spier, SWFSC" = "National Oceanic and Atmospheric Administration - Southwest Fisheries Science Center: Cameron Spier", "NOAA" = "National Oceanic and Atmospheric Administration", "NOAA NCCOS" = "National Oceanic and Atmospheric Administration - National Centers for Coastal Ocean Science", "Office of National Marine Sanctuaries - Multiple" = "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries", "Zach Gold, National Oceanic and Atmospheric Administration" = "National Oceanic and Atmospheric Administration: Zach Gold", "NOAA Office of National Marine Sanctuaries" = "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries"))

# California State University Long Beach
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Bengt Allen, CSU Long Beach" = "California State University Long Beach: Bengt Allen", "CSU Long Beach" = "California State University Long Beach", "CSU Long Beach" = "California State University Long Beach", "CSULB" = "California State University Long Beach"))

# The Nature Conservancy
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Benjaman.grimes@tnc" = "The Nature Conservancy: Ben Grime", "Benjamin Grime, TNC" = "The Nature Conservancy: Ben Grime", "everyone on the Eastern Pacific Kelp Restoration forum via The Nature Conservancy (BC to Baja)" = "The Nature Conservancy - Eastern Pacific Kelp Restoration Forum", "Jono Wilson, The Nature Conservancy" = "The Nature Conservancy: Jono Wilson", "Nature conservancy" = "The Nature Conservancy", "Nature Conservancy" = "The Nature Conservancy", "Norah Eddy, TNC CA Oceans Program" = "The Nature Conservancy: Norah Eddy", "The Nature Conservancey CA" = "The Nature Conservancy", "The nature conservancy" = "The Nature Conservancy", "The Nature Conservancy Australia and Tas" = "The Nature Conservancy", "The Nature Conservancy of California" = "The Nature Conservancy", "The Nature Conservancy, Tristin McHugh" = "The Nature Conservancy: Tristin McHugh", "The Nature Conservency- Tristen McHugh" = "The Nature Conservancy: Tristin McHugh", "TNC" = "The Nature Conservancy", "TNC - Tristin McHugh" = "The Nature Conservancy: Tristin McHugh", "TNC Australia" = "The Nature Conservancy", "TNC California" = "The Nature Conservancy", "TNC New Zealand" = "The Nature Conservancy", "Tom Dempsey, TNC" = "The Nature Conservancy: Tom Dempsey", "Tristan McHugh, The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh - The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh (The Nature Conservancy)" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh with TNC" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh- TNC" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, Kelp Forest Alliance" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, TNC" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, TNC CA Oceans Program / Reef Check" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh - Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh (Nature Conservancy)" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh Nature Conservnacy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh"))

# University of California San Diego
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Birch Aquarium" = "University of California San Diego - Scripps Institution of Oceanography - Birch Aquarium", "Ed Parnell, Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography: Ed Parnell", "Ed Parnell, UCSD" = "University of California San Diego - Scripps Institution of Oceanography: Ed Parnell", "Jen Smith at SCRIPS" = "University of California San Diego - Scripps Institution of Oceanography: Jen Smith", "Kristin Riser, Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography: Kristin Riser", "Paul Dayton, Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography: Paul Daytona", "Scripps" = "University of California San Diego - Scripps Institution of Oceanography", "Scripps Instituion of Oceanography; Ed Parnell" = "University of California San Diego - Scripps Institution of Oceanography: Ed Parnell", "Scripps Institute of Oceanography: Masters of Advanced Studies in Marine Biodiversity & Conservation (MAS MBC)" = "University of California San Diego - Scripps Institution of Oceanography", "Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography", "SIO" = "University of California San Diego - Scripps Institution of Oceanography", "UC San Diego" = "University of California San Diego", "UCSD Scripps Institute of Oceanography - Jim Leichter" = "University of California San Diego - Scripps Institution of Oceanography: Jim Leichter", "Scripps Inst. of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography"))

# San Diego State University
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Dr Mathew Edwards" = "San Diego State University: Matthew Edwards", "Matt Edwards, San Diego State University" = "San Diego State University: Matthew Edwards", "Matthew Edwards - San Diego State University" = "San Diego State University: Matthew Edwards", "Matthew Edwards, San Diego State University" = "San Diego State University: Matthew Edwards", "Shelby Penn SDSU" = "San Diego State University: Shelby Penn", "San Diego state university" = "San Diego State University")) 

# Blue Harmony
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Blue Harmony Foundation" = "Blue Harmony"))

# University of California Davis
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "BML" = "University of California Davis - Bodega Marine Laboratory", "Bodega Marine Labs" = "University of California Davis - Bodega Marine Laboratory", "Bodega Bay Marine Labs" = "University of California Davis - Bodega Marine Laboratory", "DISES" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "DiSES ppl" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Gabby Yang" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Gabby Yang", "Kelp RISES personnel" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Kelp RISES project personnel, students at UCD, UCSC, Cal Poly Humboldt" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Kelp RISES Team, UC Davis" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Marissa Baskett, UCD" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Marissa Baskett (UCD)" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Marissa Baskett - Kelp RISES" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Marissa Baskett, uc davis" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Michael Springborn (UCD)" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Mike springborn" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Mike springborn, uc davis" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Mike Springborn, UCD" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Rachael Bay, UC Davis" = "University of California Davis: Rachael Bay", "The University of California, Davis" = "University of California Davis", "UC Davis" = "University of California Davis", "UC Davis (Baskett et al)" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "UC Davis Bodega Marine Lab - John Largier" = "University of California Davis - Bodega Marine Laboratory: John Largier", "UC Davis Bodega Marine Lab Boating and Diving Safety Office" = "University of California Davis - Bodega Marine Laboratory - Boating and Diving Safety Office", "UC Davis bodega marine labs" = "University of California Davis - Bodega Marine Laboratory", "UC Davis Department of Environmental Science and Policy" = "University of California Davis", "UC Davis- BML" = "University of California Davis - Bodega Marine Laboratory", "Uc davis" = "University of California Davis", "UC Davis, DiSES" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "UC Davis, Kelp RISES team" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "University of California Davis Kelp RISES Project" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "University of California, Davis" = "University of California Davis", "University of California, Davis Coastal & Marine Sciences Institute" = "University of California Davis", "uc davis" = "University of California Davis"))

# Bureau of Ocean Energy Management
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "BOEM" = "Bureau of Ocean Energy Management"))

# Sonoma State University
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Rachael Karm- SSU" = "Sonoma State University: Rachael Karm", "Brent Hughes - Sonoma State Univ." = "Sonoma State University: Brent Hughes", "Brent Hughes (Sonoma State University)" = "Sonoma State University: Brent Hughes", "Sonoma State" = "Sonoma State University", "Hughes Lab, Sonoma State University" = "Sonoma State University - Hughes Lab", "Sonoma State University - Brent Hughes" = "Sonoma State University: Brent Hughes", "Brent Hughes, Sonoma State University" = "Sonoma State University: Brent Hughes", "SSU" = "Sonoma State University", "Brent Hughes (Sonoma State)" = "Sonoma State University: Brent Hughes", "Brent Hughes- SSU" = "Sonoma State University: Brent Hughes", "Brent Hughes, Sonoma State" = "Sonoma State University: Brent Hughes", "Rachel Karm SSU" = "Sonoma State University: Rachael Karm", "Somona State University (Hughes lab)" = "Sonoma State University - Hughes Lab", "Sonoma State University: Rachel Karm" = "Sonoma State University: Rachael Karm"))

# California Coastal Commission
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "CA Coastal Commission" = "California Coastal Commission", "Coastal Commission" = "California Coastal Commission"))

# California State Parks 
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "CA Dept. of Parks & Recreation - Santa Cruz" = "California State Parks", "CA DPR" = "California State Parks", "CA State Parks - Fort Ross" = "California State Park - Fort Ross Conservancy", "California State Parks Interpretation & Education Division" = "California State Parks - Interpretation and Education Division", "California State Parks Natural Resources Division" = "California State Parks - Natural Resources Division", "California State Parks - Interpretation & Education Division" = "California State Parks - Interpretation and Education Division", "State Parks" = "California State Parks", "The Fort Ross Conservancy" = "California State Parks - Fort Ross Conservancy"))

# California Diving News
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "CA DIving News" = "California Diving News"))

# California Academy of Sciences
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Steinhart Aquarium (California Academy of Sciences)" = "California Academy of Sciences"))

# California Fish and Game Commission
question_4.1 <-  question_4.1 %>%
   mutate_all(~ recode(., "CA Fish and Game Commission" = "California Fish and Game Commission", "CA Fish and Game Commission - Susan Ashcraft" = "California Fish and Game Commission: Susan Ashcraft", "CFGC" = "California Fish and Game Commission", "Fish & Game" = "California Fish and Game Commission"))

# California Ocean Science Trust
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Ocean Science Trust" = "California Ocean Science Trust", "CA Ocean Science Trust" = "California Ocean Science Trust", "Ocean Science Trust staff" = "California Ocean Science Trust"))

# California Ocean Protection Council
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "CA OPC" = "California Ocean Protection Council", "Calif Ocean Protection Council" = "California Ocean Protection Council", "Mike Esgro, Ocean Protection Council" = "California Ocean Protection Council: Mike Esgro", "ocean protection council" = "California Ocean Protection Council", "Ocean Protection Council" = "California Ocean Protection Council", "Ocean Protection Council- Michael Esgro" = "California Ocean Protection Council: Mike Esgro", "Ocean Protection Council; Pike Spector" = "California Ocean Protection Council: Pike Spector", "OPC" = "California Ocean Protection Council", "Pike Spector, OPC" = "California Ocean Protection Council: Pike Spector"))

# California Sea Grant
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "CA Sea Grant" = "California Sea Grant", "CA SeaGrant" = "California Sea Grant", "CA Seagrant (Oh)" = "California Sea Grant: Shauna Oh", "Cal seagrant" = "California Sea Grant", "california sea grant" = "California Sea Grant", "Jami Miller, Sea Grant / City of Fort Bragg" = "California Sea Grant: Jami Miller", "Sea grant" = "California Sea Grant", "Sea Grant" = "California Sea Grant", "Shauna Oh, California SeaGrant" = "California Sea Grant: Shauna Oh"))

# California Sea Urchin Commission
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "California Sea Urchin Commission (CSUC)" = "California Sea Urchin Commission", "California sea urchin commission" = "California Sea Urchin Commission", "CA Sea Urchin Commission" = "California Sea Urchin Commission", "Sea urchin commission" = "California Sea Urchin Commission", "California Sea Urchin Commission, David Goldenberg" = "California Sea Urchin Commission: David Goldenberg", "David Goldenburg" = "California Sea Urchin Commission: David Goldenberg", "David Goldenburg  California sea urchin commission" = "California Sea Urchin Commission: David Goldenberg", "The Urchin Commission" = "California Sea Urchin Commission", "Grant Downey Californina sea urchin commission" = "California Sea Urchin Commission: Grant Downie", "Cal Urchin Commission" = "California Sea Urchin Commission", "California Sea Urchin Commision- David Goldenberg" = "California Sea Urchin Commission: David Goldenberg", "California Sea Urchin Commission - David Goldenberg" = "California Sea Urchin Commission: David Goldenberg", "Grant Downie, The California Sea Urchin Commission" = "California Sea Urchin Commission: Grant Downie", "California Sea Urchin Comission" = "California Sea Urchin Commission", "California Sea Urchin Commission: Grant Downey" = "California Sea Urchin Commission: Grant Downie", "URCHIN COMMISSION" = "California Sea Urchin Commission"))

# California State Lands Commission
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "CA State Lands Commission" = "California State Lands Commission"))

# California State Polytechnic University Humboldt
question_4.1 <- question_4.1 %>%
   mutate_all(~recode(., "Cal Poly Humboldt - Biology department (Sean Craig)" = "California State Polytechnic University Humboldt: Sean Craig", "Cal Poly Humboldt - Fisheries department/Aquaculture" = "California State Polytechnic University Humboldt", "Cal Poly Humboldt - Oceanography department (Christine Cass)" = "California State Polytechnic University Humboldt: Christine Cass", "Cal Poly Humboldt, Food Sovereignty Lab, Dr. Cutcha Risling Baldy" = "California State Polytechnic University Humboldt - Food Sovereignty Lab: Cutucha Risling Baldy", "Cal Poly Humboldt, Jody Martinez" = "California State Polytechnic University Humboldt: Jody Martinez", "CalPoly Humboldt" = "California State Polytechnic University Humboldt", "Duncan Jackson" = "California State Polytechnic University Humboldt: Duncan Jackson", "Frankie Moitoza (Cal Poly Humboldt)" = "California State Polytechnic University Humboldt: Frankie Moitoza", "HSU" = "California State Polytechnic University Humboldt", "HSU (Craig)" = "California State Polytechnic University Humboldt: Sean Craig", "HSU Rick Alvarez" = "California State Polytechnic University Humboldt: Rick Alvarez", "Humbolt state university" = "California State Polytechnic University Humboldt", "NEREO, Cal Poly Humboldt" = "California State Polytechnic University Humboldt: Northcoast Evaluation of Reef Ecosystems Organization", "Paul Bourdeau" = "California State Polytechnic University Humboldt: Paul Bourdeau", "Rafael Cuevas Uribe" = "California State Polytechnic University Humboldt: Rafael Cuevas Uribe", "Rafael Cuveus-Uribe, Cal Poly Humboldt" = "California State Polytechnic University Humboldt: Rafael Cuevas Uribe", "Sean Craig" = "California State Polytechnic University Humboldt: Sean Craig", "Sean Craig (Cal Poly Humboldt)" = "California State Polytechnic University Humboldt: Sean Craig", "Telonicher Marine Lab" = "California State Polytechnic University Humboldt - Telonicher Marine Lab", "Cal Poly Humboldt" = "California State Polytechnic University Humboldt", "Cal Poly Humboldt-NEREO" = "California State Polytechnic University Humboldt: Northcoast Evaluation of Reef Ecosystems Organization", "NEREO" = "California State Polytechnic University Humboldt: Northcoast Evaluation of Reef Ecosystems Organization"))

# California State Polytechnic University Pomona
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Cal Poly Pomona" = "California State Polytechnic University Pomona"))

# California State University Northridge
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Cal State Northridge" = "California State University Northridge", "Janet Kubler scientist CSUN" = "California State University Northridge: Janet Kubler"))

# Marine Protected Area Collaborative Network
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Calla Allison and the MPA Collaborative Network" = "Marine Protected Area Collaborative Network: Calla Allison", "Monterey MPACN" = "Marine Protected Area Collaborative Network", "MPA CN" = "Marine Protected Area Collaborative Network", "MPA Collaborative Network" = "Marine Protected Area Collaborative Network", "MPA Collaborative Network, Humboldt Co." = "Marine Protected Area Collaborative Network", "MPA Collaboratives" = "Marine Protected Area Collaborative Network", "MPA Long Term Monitoring Project" = "Marine Protected Area Collaborative Network"))

# Giant Giant Kelp Restoration Project
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Caspar Cove kelp restoration project Jon Holcomb" = "Giant Giant Kelp Restoration Project - Caspar Cove Project: Jon Holcomb", "G 2 Kelp Restoration Keith Rootseart" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "G2KR" = "Giant Giant Kelp Restoration Project", "g2kr.com" = "Giant Giant Kelp Restoration Project", "Giant Giant Kelp - Keith Rootsaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Giant giant kelp restoration" = "Giant Giant Kelp Restoration Project", "Giant Giant Kelp Restoration" = "Giant Giant Kelp Restoration Project", "Giant Giant Kelp Restoration - Keith Rootsaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Giant Giant Kelp Restoration Project - Caspar Cove" = "Giant Giant Kelp Restoration Project - Caspar Cove Project", "Giant Giant Kelp Restoration Project (G2KR)" = "Giant Giant Kelp Restoration Project", "giant giant kep restoration" = "Giant Giant Kelp Restoration Project", "Giant Kelp Restoration Project" = "Giant Giant Kelp Restoration Project", "Great Great Kelp Restoration Project" = "Giant Giant Kelp Restoration Project", "Great great kelp restoration project (Monterey)" = "Giant Giant Kelp Restoration Project", "Keith  Rootsart" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rooseart and the Giant Giant Kelp Restoration Project" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rooseart" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rooseart - G2KR" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rootseart, Giant giant kelp restoration project" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rootsaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rootsaert - G2KR" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "The Giant Giant Kelp restoration project" = "Giant Giant Kelp Restoration Project", "Great Great Kelp Restoration" = "Giant Giant Kelp Restoration Project", "https://www.facebook.com/UrchinsKelpOtters" = "Giant Giant Kelp Restoration Project", "Keith Roostaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "g2kr" = "Giant Giant Kelp Restoration Project"))

# Watermen's Alliance
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Caspar Cove Project, Waterman's Alliance" = "Watermen's Alliance - Caspar Cove Project", "Josh Russo Waterman's alliance" = "Watermen's Alliance: Josh Russo", "The Waterman's Alliance, Josh Russo" = "Watermen's Alliance: Josh Russo", "watermans alliance" = "Watermen's Alliance", "Watermans Alliance" = "Watermen's Alliance", "Watermans Alliance Joshua Russo" = "Watermen's Alliance: Josh Russo", "Watermen's Alliance" = "Watermen's Alliance", "Watermen's Alliance Urchin Culling events Caspar Cove as urchin culler" = "Watermen's Alliance - Caspar Cove Project", "Watermen's Alliance Urchin Removal - Josh Russo" = "Watermen's Alliance: Josh Russo", "The Watermen's Alliance" = "Watermen's Alliance", "Waterman's Alliance" = "Watermen's Alliance"))

# Catalina Island Marine Institute
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Catalina island marine institute" = "Catalina Island Marine Institute", "Deidre Sullivan  - Catalina Island Marine Institute" = "Catalina Island Marine Institute: Deidre Sullivan"))

# Cordell Bank National Marine Sanctuary
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "CBNMS Advisory Council" = "Cordell Bank National Marine Sanctuary - Cordell Bank National Marine Sanctuary Advisory Council"))

# Channel Islands National Marine Sanctuary
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Channel Islands National Marine Sanctuary - Ryan Freedman" = "Channel Islands National Marine Sanctuary: Ryan Freedman", "Channel Islands National Marine Sanctuary Advisory Council" = "Channel Islands National Marine Sanctuary - Channel Islands National Marine Sanctuary Advisory Council", "Channel Islands National Park" = "Channel Islands National Marine Sanctuary", "CINMS (Freedman)" = "Channel Islands National Marine Sanctuary: Ryan Freedman", "NOAA CINMS" = "Channel Islands National Marine Sanctuary", "Scott Gabara Channel Islands National Park" = "Channel Islands National Marine Sanctuary: Scott Gabara", "Scott Gabara, Channel Islands National Parks" = "Channel Islands National Marine Sanctuary: Scott Gabara", "NOAA Channel Islands National Marine Sanctuary" = "Channel Islands National Marine Sanctuary"))

# Fish Reef Project 
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Chris Goldblatt of Fish Reef Project (fishreef.org)" = "Fish Reef Project: Chris Goldblatt"))

# City of Fort Bragg
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "City of fort bragg" = "City of Fort Bragg", "City of Ft Bragg" = "City of Fort Bragg", "Sarah McCormick, City of Fort Bragg" = "City of Fort Bragg: Sarah McCormick"))

# Redwood Elementary
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Redwood Elementary School, Fort Bragg" = "Redwood Elementary", "Redwood Elementary, Fort Bragg Unified School District" = "Redwood Elementary"))

# Coastal Conservation Association California
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Coastal Conservation Association - California Chapter" = "Coastal Conservation Association California", "Hank Goebel" = "Coastal Conservation Association California: Hank Goebel"))

# The Sea Ranch Association 
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Coastal Stewardship Task Force, The Sea Ranch Association" = "The Sea Ranch Association: Coastal Stewardship Task Force"))

# United States Air Force
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Col. David Lopez, USAF (ret.)" = "United States Air Force: David Lopez"))

# California State University Agricultural Research Institute
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "CSU Agricultural Research Institute" = "California State University Agricultural Research Institute"))

# California State University Monterey Bay
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "CSU Monterey Bay" = "California State University Monterey Bay", "CSUMB" = "California State University Monterey Bay"))

# California State University Chico
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "CSUC" = "California State University Chico"))

# University of California Berkeley
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Dan Okamoto (UCB)" = "University of California Berkeley: Dan Okamoto", "Dan Okamoto- UC Berkeley" = "University of California Berkeley: Dan Okamoto", "Dr. Dan Okamoto" = "University of California Berkeley: Dan Okamoto", "Maya Munstermann, expert scientist" = "University of California Berkeley: Maya Munstermann", "UC Berkeley" = "University of California Berkeley", "UC Berkeley Dan Okamoto" = "University of California Berkeley: Dan Okamoto", "University of Californa" = "University of California Berkeley", "University of California" = "University of California Berkeley"))

# Occidental College
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Dan Pondella (Occidental College)" = "Occidental College - Vantuna Research Group: Dan Pondella", "Occidental VRG (Pondella)" = "Occidental College - Vantuna Research Group: Dan Pondella", "Occidental College: Dan Pondella" = "Occidental College - Vantuna Research Group: Dan Pondella", "Vantuna Research Group, Occidental College" = "Occidental College - Vantuna Research Group"))

# Kashia Band of Pomo Indians
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Dan sweezy" = "Kashia Band of Pomo Indians: Dan Swezey", "Dr. Dan Sweazy" = "Kashia Band of Pomo Indians: Dan Swezey", "KASHIA" = "Kashia Band of Pomo Indians", "Kashia Band of Pomo Indians (Dan Sweezy)" = "Kashia Band of Pomo Indians: Dan Swezey", "Kashia Band of Pomo Indians, Dan Sweezy" = "Kashia Band of Pomo Indians: Dan Swezey"))

# Dandy Fish Company
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Dandy Fish Company (fish processor)" = "Dandy Fish Company"))

# DeeperBlue
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Deeperblue.com" = "DeeperBlue"))

# Ocean Rainforest
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Doug Bush, Ocean Rainforest" = "Ocean Rainforest: Doug Bush", "Ocean Rainforest, Inc." = "Ocean Rainforest"))

# The Cultured Abalone Farm
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Doug Bush, The Cultured Abalone" = "The Cultured Abalone Farm: Doug Bush", "The Cultured Abalone Farm Rick Gutierrez" = "The Cultured Abalone Farm: Rick Gutierrez", "The Cultured Abalone Farm LLC" = "The Cultured Abalone Farm"))

# Noyo Center for Marine Science
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Elizabeth Carpenter & NOYO Center - outreach webinar for Sea Otter Awareness Week 2023" = "Noyo Center for Marine Science: Elizabeth Carpenter", "Noyo Center" = "Noyo Center for Marine Science", "NOYO CENTER" = "Noyo Center for Marine Science", "Noyo Marine Science Center" = "Noyo Center For Marine Science", "Noyo Marine Science Center (worked with directly)" = "Noyo Center For Marine Science", "Noyo Ocean Center" = "Noyo Center For Marine Science", "Noyo Science Center" = "Noyo Center For Marine Science", "Sheila Semans, NOYO Center" = "Noyo Center For Marine Science: Sheila Semans", "Sheila Semans, Noyo Center" = "Noyo Center For Marine Science: Sheila Semans", "Sheila Semans" = "Noyo Center For Marine Science: Sheila Semans"))

# University of Washington
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Friday Harbor Lab" = "University of Washington - Friday Harbor Laboratories", "Friday Harbor Lab, University of Washington (Hodin lab)" = "University of Washington - Friday Harbor Laboratories - Hodin Lab", "University of Washington, Friday Harbor Labs" = "University of Washington - Friday Harbor Laboratories"))

# Washington State Department of Natural Resources
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "WA DNR; Helen Berry" = "Washington State Department of Natural Resources: Helen Berry"))

# Hog Island Oyster Company
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Gary Fleener, Hog Island Oyster Company" = "Hog Island Oyster Company: Gary Fleener"))

# Elkhorn Slough Ecological Reserve
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "ESNERR" = "Elkhorn Slough Ecological Reserve"))

# Get Inspired
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Get Inspired, Nancy Caruso" = "Get Inspired: Nancy Caruso", "My own kelp monitoring program (Get Inspired ), Nancy Caruso" = "Get Inspired"))

# Girl Scouts of America
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Girl Scouts of America - Cheryl Kingman" = "Girl Scouts of America: Cheryl Kingman"))

# Gwaii Haanas National Park Reserve
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "GWAII HAANAS" = "Gwaii Haanas National Park Reserve"))

# Council of the Haida Nation
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "HAIDA FISHERIES" = "Council of the Haida Nation -  Haida Fisheries Program"))

# California Marine Sanctuary Foundation
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Hallie Brown, CA Marine Sanctuary Foundation" = "California Marine Sanctuary Foundation: Hallie Brown"))

# The Bay Foundation
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Heather Burdick Bay Foundation" = "The Bay Foundation: Heather Burdick"))

# Surfrider Foundation
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Humboldt Surfrider" = "Surfrider Foundation - Surfrider Humboldt"))

# InterTribal Sinkyone Wilderness Council
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Inter Tribal Sinkyone Wilderness Council" = "InterTribal Sinkyone Wilderness Council"))

# University of Massachusetts Boston
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Jarrett Byrnes, University of Massachusetts Boston" = "University of Massachusetts Boston: Jarrett Byrnes"))

# University of Nevada Reno
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Jeremy McFarland, University of Nevada, Reno" = "University of Nevada Reno: Jeremy McFarland"))

# Trinidad Rancheria
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Jessica Gravelle, Trinidad Rancheria" = "Trinidad Rancheria: Jessica Gravelle", "The Cher-Ae Heights Indian Community of the Trinidad Rancheria" = "Trinidad Rancheria"))

# Marine Conservation Institute
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Jorge Arroyo-Esquival & Marine Conservation Institute - outreach webinar for Sea Otter Awareness Week 2023" = "Marine Conservation Institute: Jorge Arroyo Esquival"))

# Kelp Restoration and Management Plan Scientific Advisory Team
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Kelp restoration management plan - CA" = "Kelp Restoration and Management Plan Scientific Advisory Team", "Kelp Restoration Management Plan Scientific Advisory Team" = "Kelp Restoration and Management Plan Scientific Advisory Team"))

# Kelp Restoration and Management Plan Community Working Group
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "KRMP" = "Kelp Restoration and Management Plan Community Working Group", "KRMP (CDFW)" = "Kelp Restoration and Management Plan Community Working Group", "California Kelp Restoration and Management Community Group" = "Kelp Restoration and Management Plan Community Working Group", "Members of the KRMP stakeholder group" = "Kelp Restoration and Management Plan Community Working Group"))

# LAWaterkeeper
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "LA Waterkeeper - Michael Quill" = "LAWaterkeeper: Michael Quill"))

# University of California Merced
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Lauren Schiebelhut" = "University of California Merced: Lauren Schiebelhut", "Michael Dawson" = "University of California Merced: Michael Dawson", "Mike Dawson, UC Merced" = "University of California Merced: Michael Dawson", "UC Merced" = "University of California Merced", "UC Merced (Dawson lab)" = "University of California Merced - Dawson Lab"))

# Sepia Lux 
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Loren Crotty (private fundraising event)" = "Sepia Lux: Loren Crotty"))

# National Fish and Wildlife Foundation
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "National Fish and Wildlife Foundation - Multiple" = "National Fish and Wildlife Foundation"))

# National Center for Ecological Analysis and Synthesis
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "NCEAS kelp working group (lead is Eger)" = "National Center for Ecological Analysis and Synthesis"))

# National Marine Sanctuary Foundation
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "NMSF" = "National Marine Sanctuary Foundation", "NOAA NMSF" = "National Marine Sanctuary Foundation", "NOAA NMS" = "National Marine Sanctuary Foundation", "NOAA Sanctuaries" = "National Marine Sanctuary Foundation", "NOAA NMFS" = "National Marine Sanctuary Foundation"))

# Noozhawk
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "noozhawk.com (online news service)" = "Noozark"))

# Oregon Department of Fish and Wildlife
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "OR DFW; Scott Marion" = "Oregon Department of Fish and Wildlife: Scott Marion", "Oregon Department of Fisheries and Wildlife" = "Oregon Department of Fish and Wildlife"))

# Oregon Kelp Alliance
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Oregon kelp alliance" = "Oregon Kelp Alliance", "Oregon Kelp Alliance: Thomas Calvanes" = "Oregon Kelp Alliance: Tom Calvanes", "Oregon Kelp Forest Alliance" = "Oregon Kelp Alliance", "ORKA, Tom Calvanese" = "Oregon Kelp Alliance: Tom Calvanes"))

# San Jose State University
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "others at SJSU" = "San Jose State University"))

# Port of San Diego
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Paula Sylvia at the Port of San Diego" = "Port of San Diego: Paula Sylvia"))

# Port of Los Angeles
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Port of LA" = "Port of Los Angeles"))

# Puget Sound Restoration Fund
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Puget sound restoration fund" = "Puget Sound Restoration Fund", "Puget Sound Restoration Fund in seattle washington" = "Puget Sound Restoration Fund"))

# Partnership for Interdisciplinary Studies of Coastal Oceans
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "PISCO" = "Partnership for Interdisciplinary Studies of Coastal Oceans"))

# Pycnopodia Recovery Working Group
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Pycnopodia Restoration Group" = "Pycnopodia Recovery Working Group"))

# GreenWave
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Green Wave" = "GreenWave"))

# Reef Environmental Education Foundation
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "REEF,  Janna Nichols" = "Reef Environmental Education Foundation: Janna Nichols"))

# United States Congress
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Rep. Huffman (district 2)" = "United States Congress: Jared Huffman"))

# United States Congress
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Rep. Huffman (district 2)" = "United States Congress: Jared Huffman"))

# Universidad Autonoma de Baja California Mexico
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Rodrigo Beas-Lunas (ABC Ensenada)" = "Universidad Autonoma de Baja California Mexico: Rodrigo Beas-Lunas"))

# Tolowa Dee-ni' Nation
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Rosa Laucci, Tolowa Dee-ni' Nation" = "Tolowa Dee-ni' Nation: Rosa Laucci", "Tolowa Dee ni' Nation" = "Tolowa Dee-ni' Nation", "Tolowa Dee-Ni' Nation" = "Tolowa Dee-ni' Nation"))

# Salesforce
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "SalesForce" = "Salesforce"))

# San Diego Association of Governments
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "SANDAG" = "San Diego Association of Governments"))

# Save Our Shores
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Save our Shores" = "Save Our Shores"))

# Southern California Coastal Ocean Observing System
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "SCCOOS" = "Southern California Coastal Ocean Observing System"))

# Southern California Coastal Water Research Project
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "SCCWRP" = "Southern California Coastal Water Research Project"))

# Sherwood Valley Band of Pomo Indians
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Sherwood Valley Band of Pomo" = "Sherwood Valley Band of Pomo Indians"))

# Stanford University
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Stanford" = "Stanford University", "Stanford University - Steven Monismith" = "Stanford University: Steven Monismith"))

# California State Lands Commission
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "State Lands" = "California State Lands Commission"))

# Strategic Earth Consulting
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Strategic Earth" = "Strategic Earth Consulting", "Strategic Earth Inc." = "Strategic Earth Consulting"))

# Sunken Seaweed
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Sunken Seaweed LLC, Torre Polizzi" = "Sunken Seaweed: Torre Polizzi", "Torre Polizzi, Sunken Seaweed" = "Sunken Seaweed: Torre Polizzi", "Torre pollozi" = "Sunken Seaweed: Torre Polizzi"))

# The Jetlagged
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "The Jet Lagged (photographers)" = "The Jetlagged"))

# University of Southern California
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "U Southern California" = "University of Southern California"))

# United States Navy
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "U.S. Navy" = "United States Navy", "US Navy" = "United States Navy"))

# United States Geological Survey
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "USGS" = "United States Geological Survey", "U.S. Geological Survey" = "United States Geological Survey"))

# University of California Los Angeles
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "UCLA" = "University of California Los Angeles", "UCLA - Kyle Cavanaugh" = "University of California Los Angeles: Kyle Cavanaugh", "UCLA; Kyle Cavanuagh" = "University of California Los Angeles: Kyle Cavanaugh"))

# University of Miami 
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "University of Miami Rosenstiel School of Marine, Atmospheric, and Earth Science - Claire Paris" = "University of Miami: Claire Paris"))

# University of Oregon
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "UO OIMB Biology department - Aaron Galloway" = "University of Oregon: Aaron Galloway"))

# Pacific Urchin Harvesters Association
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "URCHIN HARVESTORS ASSOCIATION" = "Pacific Urchin Harvesters Association"))

# United States Fish and Wildlife Service
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "US Fish and Wildlife Service" = "United States Fish and Wildlife Service", "USFWS" = "United States Fish and Wildlife Service", "US Fish and Wildlife" = "United States Fish and Wildlife Service"))

# Woods Hole Oceanographic Institution
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "WHOI; Tom Bell" = "Woods Hole Oceanographic Institution: Tom Bell", "WHOI (Bell)" = "Woods Hole Oceanographic Institution: Tom Bell", "WHOI - Tom Bell" = "Woods Hole Oceanographic Institution: Tom Bell"))

# Earth Equity
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Earth equty" = "Earth Equity"))

# WSP
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "WSP (env consulting)" = "WSP"))

# West Coast Ocean Alliance
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "West Coast Ocean alliance" = "West Coast Ocean Alliance"))

# Individuals 
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Byron Kohler (urchin diver" = "Urchin Diver: Byron Kohler", "Gary Trumper- urchin diver" = "Urchin Diver: Gary Trumper", "Mickey Kitahara- urchin diver" = "Urchin Diver: Mickey Kitahara", "Urchin removal diver" = "Urchin Diver"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Many Kelp Restoration Divers" = "Divers"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "commercial fishermen in San Diego" = "Fishers"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Urchin removal" = "Urchin Removers"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Dale Glanz" = "Dale Glanz"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Caspar Cove kelp restoration project Jon Holcomb" = "Commerical Fisher: Jon Holcomb"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Erik Owen (commercial fisher)" = "Commerical Fisher: Erik Owen"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Ernest (Tad) Thompson (commercial fisher)" = "Commercial Fisher: Ernest Thompson"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Jeffrey Gritsch (commercial fisher)" = "Commerical Fisher: Jeffrey Gritsch"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Mark Gmeiner (commercial fisher)" = "Commerical Fisher: Mark Gmeiner", " Commerical Fisher: Mark Gmeiner" = "Commerical Fisher: Mark Gmeiner"))

# Grant Downie is on the urchin commission, urchin diver, kelp restoration specialist, etc
question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Grant Downey" = "Grant Downie", "Grant downie" = "Grant Downie", "Grant Downie (commercial fisher)" = "Fisher: Grant Downie", "Grant Downie (Commercial urchin diver, mendocino)" = "Urchin Diver: Grant Downie", "Grant Downie- urchin diver" = "Urchin Diver: Grant Downie"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Pat Downie (commercial fisher)" = "Fisher: Pat Downie"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Jeremy George Petty (commercial fisher)" = "Fisher: Jeremy George Petty"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Josie Iselin (kelp artist and educator)" = "Josie Iselin", "Josie Iselin, Author/Artist" = "Josie Iselin"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Kevin Quider (commercial fisher)" = "Fisher: Kevin Quider"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Patrick Webster, science communicator and underwater photographer" = "Photographer: Pat Webster"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Roger Carlson, Roger Carlson Photography" = "Photographer: Roger Carlson"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Ron McPeak" = "Photographer: Ron McPeak"))

question_4.1 <- question_4.1 %>%
   mutate_all(~ recode(., "Shaun Wolfe, Shaun Wolfe Photography" = "Photographer: Shaun Wolfe"))

question_4.1 <- question_4.1[question_4.1$org_name != "Decline to state", ]
question_4.1 <- question_4.1[question_4.1$org_name != "CA Dept of Fish and Wildlife", ]

question_4.1$org_name <- str_remove(question_4.1$org_name, " -.*")
question_4.1$org_name <- str_remove(question_4.1$org_name, ":.*")

question_4.1 <- question_4.1 %>%
   mutate(org_type = case_when(
      org_name == "Above/Below" ~ "NGO",
      org_name == "AltaSeads Conservancy" ~ "NGO",
      org_name == "Amah Mutsun Land Trust" ~ "Tribal",
      org_name == "Anthropocene Institute" ~ "Consulting",
      org_name == "Aquarium of the Pacific" ~ "Research",
      org_name == "Artist 1" ~ "Individual",
      org_name == "Artist 2" ~ "Individual",
      org_name == "Artist 3" ~ "Individual",
      org_name == "Artist 4" ~ "Individual",
      org_name == "Artist 5" ~ "Individual",
      org_name == "Blue Evolution" ~ "NGO",
      org_name == "Blue Harmony" ~ "NGO",
      org_name == "Bureau of Ocean Energy Management" ~ "Federal",
      org_name == "California Academy of Sciences" ~ "Research",
      org_name == "California Coastal Commission" ~ "State",
      org_name == "California Department of Fish and Wildlife" ~ "State",
      org_name == "California Diving News" ~ "Local",
      org_name == "California Fish and Game Commission" ~ "State",
      org_name == "California Marine Sanctuary Foundation" ~ "NGO",
      org_name == "California Ocean Protection Council" ~ "State",
      org_name == "California Ocean Science Trust" ~ "NGO",
      org_name == "California Sea Grant" ~ "Research",
      org_name == "California Sea Urchin Commission" ~ "NGO",
      org_name == "California Seaweed Festival" ~ "Local",
      org_name == "California State Lands Commission" ~ "State",
      org_name == "California State Parks" ~ "State",
      org_name == "California State Polytechnic University Humboldt" ~ "Research",
      org_name == "California State Polytechnic University Pomona" ~ "Research",
      org_name == "California State University Agricultural Research Institute" ~ "Research",
      org_name == "California State University Chico" ~ "Research",
      org_name == "California State University Long Beach" ~ "Research",
      org_name == "California State University Monterey Bay" ~ "Research",
      org_name == "California State University Northridge" ~ "Research",
      org_name == "Catalina Island Marine Institute" ~ "Research",
      org_name == "CCCoP" ~ "Other",
      org_name == "Channel Islands National Marine Sanctuary" ~ "Regional",
      org_name == "City College of San Francisco" ~ "Research",
      org_name == "City of Fort Bragg" ~ "Local",
      org_name == "City of Long Beach" ~ "Local",
      org_name == "City of San Diego" ~ "Local",
      org_name == "Coastal Conservation Association California" ~ "NGO",
      org_name == "Coastal Environments" ~ "Consulting",
      org_name == "Cordell Bank National Marine Sanctuary" ~ "Regional",
      org_name == "Council of the Haida Nation" ~ "Tribal",
      org_name == "Dandy Fish Company" ~ "Local",
      org_name == "DeeperBlue" ~ "Local",
      org_name == "Defenders of Wildlife" ~ "NGO",
      org_name == "Divers" ~ "Individual",
      org_name == "Earth Equity" ~ "NGO",
      org_name == "Edges of Earth" ~ "NGO",
      org_name == "Elkhorn Slough Ecological Reserve" ~ "Local",
      org_name == "Elkhorn Slough National Estuarine Research Reserve" ~ "Research",
      org_name == "Environment California" ~ "NGO",
      org_name == "Environmental Defense Fund" ~ "NGO",
      org_name == "Fish Reef Project" ~ "NGO",
      org_name == "FISHBIO" ~ "Consulting",
      org_name == "Fisher 2" ~ "Individual",
      org_name == "Fisher 3" ~ "Individual",
      org_name == "Fisher 4" ~ "Individual",
      org_name == "Fisher 5" ~ "Individual",
      org_name == "Fisher 6" ~ "Individual",
      org_name == "Fisher 7" ~ "Individual",
      org_name == "Fisher 8" ~ "Individual",
      org_name == "Fisher 9" ~ "Individual",
      org_name == "Fisher 10" ~ "Individual",
      org_name == "Fisher 11" ~ "Individual",
      org_name == "Fishers" ~ "Individual",
      org_name == "Florida State University" ~ "Research",
      org_name == "Friends of the Dunes" ~ "NGO",
      org_name == "Garden Club of America" ~ "NGO",
      org_name == "Get Inspired" ~ "NGO",
      org_name == "Giant Giant Kelp Restoration Project" ~ "NGO",
      org_name == "Girl Scouts of America" ~ "NGO",
      org_name == "Golden Gate Collaborative" ~ "Research",
      org_name == "Greater Farallones Association" ~ "NGO",
      org_name == "Greater Farallones National Marine Sanctuary" ~ "Regional",
      org_name == "GreenWave" ~ "NGO",
      org_name == "Gwaii Haanas National Park Reserve" ~ "Regional",
      org_name == "Hakai Institute" ~ "Research",
      org_name == "Hog Island Oyster Company" ~ "Local",
      org_name == "Individual 1" ~ "Individual",
      org_name == "Individual 2" ~ "Individual",
      org_name == "Individual 3" ~ "Individual",
      org_name == "Individual 4" ~ "Individual",
      org_name == "InterTribal Sinkyone Wilderness Council" ~ "Tribal",
      org_name == "Joseph M. Long Marine Laboratory" ~ "Research",
      org_name == "Kashia Band of Pomo Indians" ~ "Tribal",
      org_name == "Kelp Farmers" ~ "Individual",
      org_name == "Kelp Forest Alliance" ~ "NGO",
      org_name == "Kelp Restoration and Management Plan Community Working Group" ~ "State",
      org_name == "Kelp Restoration and Management Plan Scientific Advisory Team" ~ "State",
      org_name == "LAWaterkeeper" ~ "NGO",
      org_name == "Marine Conservation Institute" ~ "NGO",
      org_name == "Marine Protected Area Collaborative Network" ~ "Research",
      org_name == "Middlebury Institute of International Studies at Monterey" ~ "Research",
      org_name == "Monterey Abalone Company" ~ "Local",
      org_name == "Monterey Bay Aquarium" ~ "Research",
      org_name == "Monterey Bay Fisheries Trust" ~ "NGO",
      org_name == "Monterey Bay Foundation" ~ "NGO",
      org_name == "Monterey Bay Mermaid" ~ "Local",
      org_name == "Monterey Bay National Marine Sanctuary" ~ "Regional",
      org_name == "Monterey Bay Seaweeds" ~ "Local",
      org_name == "Monterey County Weekly" ~ "Local",
      org_name == "Monterey Herald" ~ "Local",
      org_name == "Montrose Settlements Restoration Program" ~ "Regional",
      org_name == "Moss Landing Marine Laboratories" ~ "Research",
      org_name == "National Center for Ecological Analysis and Synthesis" ~ "Research",
      org_name == "National Fish and Wildlife Foundation" ~ "NGO",
      org_name == "National Marine Sanctuary Foundation" ~ "NGO",
      org_name == "National Oceanic and Atmospheric Administration" ~ "Federal",
      org_name == "National Park Service" ~ "NGO",
      org_name == "Noozark" ~ "Local",
      org_name == "Northcoast Environmental Center" ~ "Research",
      org_name == "Noyo Center for Marine Science" ~ "Research",
      org_name == "Noyo Harbor District" ~ "Local",
      org_name == "Noyo Ocean Collective" ~ "NGO",
      org_name == "Occidental College" ~ "Research",
      org_name == "Ocean Rainforest" ~ "NGO",
      org_name == "Ocean Visions" ~ "NGO",
      org_name == "Oregon Coast Aquarium" ~ "Research",
      org_name == "Oregon Department of Fish and Wildlife" ~ "State",
      org_name == "Oregon Kelp Alliance" ~ "NGO",
      org_name == "Oregon State University" ~ "Research",
      org_name == "Pacific Coast Sportfishing Magazine" ~ "Local",
      org_name == "Pacific States Marine Fisheries Commission" ~ "State",
      org_name == "Pacific Urchin Harvesters Association" ~ "NGO",
      org_name == "Partnership for Interdisciplinary Studies of Coastal Oceans" ~ "Research",
      org_name == "Paua Marine Research Group" ~ "Research",
      org_name == "Port of Long Beach" ~ "Local",
      org_name == "Port of Los Angeles" ~ "Local",
      org_name == "Port of San Diego" ~ "Local",
      org_name == "Puget Sound Restoration Fund" ~ "NGO",
      org_name == "Pycnopodia Recovery Working Group" ~ "Research",
      org_name == "Redwood Elementary" ~ "Local",
      org_name == "Reef Check" ~ "NGO",
      org_name == "Reef Environmental Education Foundation" ~ "NGO",
      org_name == "Running Tide" ~ "NGO",
      org_name == "SAFE" ~ "Other",
      org_name == "Salesforce" ~ "Other",
      org_name == "San Diego Association of Governments" ~ "Local",
      org_name == "San Diego State University" ~ "Research",
      org_name == "San Jose State University" ~ "Research",
      org_name == "Santa Monica Bay Restoration Commission" ~ "Local",
      org_name == "Save Our Shores" ~ "NGO",
      org_name == "Sea Forest" ~ "NGO",
      org_name == "Sea Otter Savvy" ~ "NGO",
      org_name == "Selkie Land + Sea" ~ "Local",
      org_name == "Sepia Lux" ~ "Local",
      org_name == "Sherwood Valley Band of Pomo Indians" ~ "Tribal",
      org_name == "Sonoma State University" ~ "Research",
      org_name == "Southern California Coastal Ocean Observing System" ~ "Research",
      org_name == "Southern California Coastal Water Research Project" ~ "Research",
      org_name == "Southern California Edison" ~ "Regional",
      org_name == "Southern California Marine Institute" ~ "Research",
      org_name == "Stanford University" ~ "Research",
      org_name == "Strategic Earth Consulting" ~ "Consulting",
      org_name == "Sunken Seaweed" ~ "NGO",
      org_name == "Surfrider Foundation" ~ "NGO",
      org_name == "Sustainable Ocean Alliance" ~ "NGO",
      org_name == "Tableau" ~ "Other",
      org_name == "The Bay Foundation" ~ "NGO",
      org_name == "The Cultured Abalone Farm" ~ "Local",
      org_name == "The Jetlagged" ~ "NGO",
      org_name == "The Mercury News" ~ "Local",
      org_name == "The Nature Conservancy" ~ "NGO",
      org_name == "The Sea Ranch Association" ~ "NGO",
      org_name == "Tolowa Dee ni' Nation" ~ "Tribal",
      org_name == "Tribal Government" ~ "Tribal",
      org_name == "Tribal Marine Stewards Network" ~ "Tribal",
      org_name == "Trinidad Coastal Land Trust" ~ "Tribal",
      org_name == "Trinidad Rancheria" ~ "Tribal",
      org_name == "United States Air Force" ~ "Federal",
      org_name == "United States Congress" ~ "Federal",
      org_name == "United States Fish and Wildlife Service" ~ "Federal",
      org_name == "United States Geological Survey" ~ "Federal",
      org_name == "United States Air Force" ~ "Federal",
      org_name == "United States Navy" ~ "Federal",
      org_name == "University of Alaska Fairbanks" ~ "Research",
      org_name == "University of California Berkeley" ~ "Research",
      org_name == "University of California Davis" ~ "Research",
      org_name == "University of California Irvine" ~ "Research",
      org_name == "University of California Los Angeles" ~ "Research",
      org_name == "University of California Merced" ~ "Research",
      org_name == "University of California San Diego" ~ "Research",
      org_name == "University of California Santa Barbara" ~ "Research",
      org_name == "University of California Santa Cruz" ~ "Research",
      org_name == "University of Massachusetts Boston" ~ "Research",
      org_name == "University of Miami" ~ "Research",
      org_name == "University of Nevada Reno" ~ "Research",
      org_name == "University of Oregon" ~ "Research",
      org_name == "University of Southern California" ~ "Research",
      org_name == "University of Washington" ~ "Research",
      org_name == "University of Wisconsin" ~ "Research",
      org_name == "University of Wisconsin Milwaukee" ~ "Research",
      org_name == "Washington State Department of Natural Resources" ~ "State",
      org_name == "Watermen's Alliance" ~ "NGO",
      org_name == "West Coast Ocean Alliance" ~ "NGO",
      org_name == "West Marin Compost Company" ~ "Local",
      org_name == "Wishtoyo Chumash Foundation" ~ "Tribal",
      org_name == "Wiyot Tribe" ~ "Tribal",
      org_name == "Woods Hole Oceanographic Institution" ~ "Research",
      org_name == "WSP" ~ "Consulting",
      org_name == "Yurok Tribe" ~ "Tribal",
      TRUE ~ org_type))

question_4.1 <- question_4.1 %>%
   pivot_longer(cols = starts_with("Q4_"),
                names_to = "question",
                values_to = "activity") %>%
   filter(!is.na(activity))

question_4.1 <- question_4.1 %>%
   group_by(org_type, activity) %>%
   summarise(count = n()) %>%
   ungroup()

question_4.1 <- question_4.1 %>%
   pivot_wider(names_from = activity, values_from = count, values_fill = 0)

question_4.1 <- question_4.1 %>%
   pivot_longer(-org_type, names_to = "activity", values_to = "count")

ggplot(question_4.1, aes(x = activity, y = org_type, fill = count)) +
   geom_tile(color = "white") +
   scale_fill_gradient(low = "white", high = "steelblue") +
   theme_minimal() +
   labs(title = "Activity Involvement by Org Type", x = "Activity", y = "Org Type") +
   theme(axis.text.x = element_text(angle = 30, hjust = 1))

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

  
