library(ergm)
library(sna) 

library(readr)
library(tidyverse) 
library(dplyr) 
library(tidyr) 

kelp.net <- read_csv("Desktop/R_stuff/kelp_restoration/data_files/kelp_jan.9.25.csv")[-c(1,2),]

# What does the restoration network look like first cut 
# wkwxewe

# Org Names ---------------------------------------------------

kelp.net <- kelp.net %>%
  unite(col = "org_name", c(`Q3 Individual_1`, `Q3 Several_1`), sep = "", na.rm = TRUE, remove = FALSE)

# Edge List ---------------------------------------------------

kelp.edge <- kelp.net %>% 
  select(ResponseId, org_name, starts_with("Q11"), starts_with("Q12"), starts_with("Q13"))

kelp.edge <- kelp.edge %>%
  pivot_longer(cols = -c(ResponseId, org_name), names_to = "type", values_to = "alter")

kelp.edge <- kelp.edge %>%
  mutate(edge_type = case_when(
    str_detect(type, regex("^Q11")) ~ "collaboration", 
    str_detect(type, regex("^Q12")) ~ "coordination",
    str_detect(type, regex("^Q13")) ~ "communication"
  ))

kelp.edge <- kelp.edge %>%
  filter(!is.na(alter))

# renaming affiliated org_names 
kelp.edge <- kelp.edge %>%
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
    ResponseId == "R_3jTauwChLV63T1f" ~ "Giant Giant Kelp Forest Restoration Project",
    ResponseId == "R_5p0hFxU61rtshee" ~ "Giant Giant Kelp Forest Restoration Project",
    ResponseId == "R_5DbUjzy5UEB6Bmv" ~ "Giant Giant Kelp Forest Restoration Project",
    ResponseId == "R_1A45wn3R1qfgQo1" ~ "Individual",
    ResponseId == "R_1DIA6BHO7pKYU8x" ~ "Individual",
    TRUE ~ org_name))

# create roster later = node list 


# Alter Renaming ----------------------------------------------

# - Org name at a different level
# : Specific individual named

# AltaSeads Conservancy
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Sergey Nuzhdin  USC/ Alta Seads spore bank" = "AltaSeads Conservancy: Sergey Nuzhdin"))

# Kelp Forest Alliance
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "The Kelp Forest Alliance (Eger)" = "Kelp Forest Alliance: Aaron Eger", "Aaron eger and many many others" = "Kelp Forest Alliance: Aaron Eger", "Kelp forest alliance" = "Kelp Forest Alliance"))

# Above/Below
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "New Agency / Above Below" = "Above/Below", "New Agency" = "Above/Below"))

# Sea Forest
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Alex Berger" = "Sea Forest: Alex Berger"))

# University of California Santa Cruz
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Ali Boutros (UCSC Coastal Science and Policy graduate program))" = "University of California Santa Cruz: Ali Boutros", "Andrea Paz Lacavex (UCSC)" = "University of California Santa Cruz: Andrea Paz Lacavex", "Calvin Munson (UCSC)" = "University of California Santa Cruz: Calvin Munson", "Carr Lab UCSC" = "University of California Santa Cruz - Raimondi-Carr Lab", "Carrie Pomeroy (UCSC)" = "University of California Santa Cruz: Carrie Pomeroy", "Dan Malone- UCSC" = "University of California Santa Cruz: Dan Malone", "Dr Pete raimondi" = "University of California Santa Cruz: Peter Raimondi", "Kristy Kroeker (UCSC)" = "University of California Santa Cruz: Kristy Kroeker", "Mark Carr- UCSC" = "University of California Santa Cruz: Mark Carr", "Mark Carr, UCSC" = "University of California Santa Cruz: Mark Carr", "Pete Raimondi (UCSC)" = "University of California Santa Cruz: Pete Raimondi", "UC Santa Cruz" = "University of California Santa Cruz", "UC Santa Cruz (Carr et al)" = "University of California Santa Cruz: Mark Carr", "UC Santa Cruz Long Marine Lab" = "Joseph M. Long Marine Laboratory", "UCSC" = "University of California Santa Cruz", "UCSC (Carr)" = "University of California Santa Cruz: Mark Carr", "University of California: Santa Cruz" = "University of California Santa Cruz", "US Santa Cruz" = "University of California Santa Cruz"))

# Amah Mutsun Land Trust
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Amah Mutsun" = "Amah Mutsun Land Trust"))

# Moss Landing Marine Laboratories
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Andrew kim" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim - MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim- MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim, MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Moss Landing Marine Labs Aquaculture Center" = "Moss Landing Marine Laboratories - Aquaculture Center", "Moss Landing marine labs" = "Moss Landing Marine Laboratories", "Moss landing marine labs" = "Moss Landing Marine Laboratories", "Moss Landing Marine Laboratory" = "Moss Landing Marine Laboratories", "Scott Hamilton, MLML Moss Landing Marine Laboratories" = "Moss Landing Marine Laboratories: Scott Hamilton", "Bennett Bugbee - MLML" = "Moss Landing Marine Laboratories: Bennett Bugbee", "Scott Hamilton (Moss Landing Marine Labs)" = "Moss Landing Marine Laboratories: Scott Hamilton", "Mike Graham scientist Moss Landing" = "Moss Landing Marine Laboratories: Mike Graham", "Moss Landing Marine Lab" = "Moss Landing Marine Laboratories", "SJSU Moss Landing Marine Labs - Scott Hamilton" = "Moss Landing Marine Laboratories: Scott Hamilton", "Moss landing" = "Moss Landing Marine Laboratories", "Scott Hamilton- MLML" = "Moss Landing Marine Laboratories: Scott Hamilton", "Bennet Bugbee- MLML" = "Moss Landing Marine Laboratories: Bennett Bugbee", "Bennett Bugbee, MLML" = "Moss Landing Marine Laboratories: Bennett Bugbee","Ashley Kidd" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory: Ashley Kidd", "https://www.sunflowerstarlab.org/" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Mike Graham, Moss Landing Marine Labs" = "Moss Landing Marine Labs: Mike Graham", "Scott hamilton" = "Moss Landing Marine Laboratories: Scott Hamilton", "SJSU Moss Landing Marine Labs - Scott Hamiltom" = "Moss Landing Marine Laboratories: Scott Hamilton", "Sunflower Sea Star Laboratory" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Sunflower Star Lab" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Vince Christian, Sunflower Star Laboratory, Moss Landing" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory: Vince Christian", "Sunflower Star Lab, Moss Landing" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Sunflower Star Laboratory" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory")) 

# Florida State University
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Andrew Rassweiler, Florida State University" = "Florida State University: Andrew Rassweiler", "FSU" = "Florida State University")) 

# Greater Farallones National Marine Sanctuary
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "GFNMS" = "Greater Farallones National Marine Sanctuary", "GFNMS (Holman)" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "GFNMS Advisory Council" = "Greater Farallones National Marine Sanctuary - Greater Farallones National Marine Sanctuary Advisory Council", "Greater farallones" = "Greater Farallones National Marine Sanctuary", "Greater Farallones Sanctuary Advisory Council" = "Greater Farallones National Marine Sanctuary - Greater Farallones National Marine Sanctuary Advisory Council", "Greater Farrallones" = "Greater Farallones National Marine Sanctuary", "Greater Farralones" = "Greater Farallones National Marine Sanctuary", "Gulf of the Farallons National Marine Santuary, Angela Zepp" = "Greater Farallons National Marine Santuary: Angela Zepp", "Gulf of the Farralons National Marine Sanctuary" = "Greater Farallones National Marine Sanctuary", "NOAA Greater Farallones National Marine Sanctuary" = "Greater Farallones National Marine Sanctuary", "Rietta Holman - GFA" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Holman" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Holman - Greater Farallon Association" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Hoffman- GFA" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Hohman" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Hohman - Greater Farallon Association" = "Greater Farallones National Marine Sanctuary: Rietta Hohman"))

# Greater Farallones Association 
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "The Greater Farrallones Association" = "Greater Farallones Association", "The Greater Farallones Kelp Restoration Project (GFA)" = "Greater Farallones Association", "Angela Zepp- GFA" = "Greater Farallones Association: Angela Zepp", "Gina Contolini- GFA" = "Greater Farallones Association: Gina Contolini", "Greater Farallon Association-Rietta Hohman" = "Greater Farallon Association: Rietta Hohman", "Greater Farallones Association kelp team (Rietta Hohman)" = "Greater Farallon Association: Rietta Hohman", "Greater Farallones Association, Rietta Hohman" = "Greater Farallones Association: Rietta Hohman", "Greater Farrallons association" = "Greater Farallones Association", "Gulf of the Farallons Association" = "Greater Farallones Association", "Julieta Gomez- GFA" = "Greater Farallones Association: Julieta Gomez", "Kelp Restoration Team, Greater Farallones Association" = "Greater Farallones Association", "Gulf of the Farallones Association" = "Greater Farallones Association", "Tyler Mears- GFA" = "Greater Farallones Association: Tyler Mears"))

# University of California Santa Barbara
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Anita Giraldo Ospina (UCSB)" = "University of California Santa Barbara: Anita Giraldo Ospina", "Anita Giraldo-Ospino, UCSB" = "University of California Santa Barbara: Anita Giraldo Ospina", "Univ CA Santa Barbara" = "University of California Santa Barbara", "University of California, Santa Barbara" = "University of California Santa Barbara", "UCSB" = "University of California Santa Barbara", "Capt. Merit McCrea" = "University of California Santa Barbara: Merit McCrea", "Colleagues at UCSB and in New Zealand" = "University of California Santa Barbara", "Dan Reed (UCSB)" = "University of California Santa Barbara: Dan Reed", "Dr. Steve Schroder" = "University of California Santa Barbara: Stephen Schroeter", "Jennifer Caselle (UCSB)" = "University of California Santa Barbara: Jennifer Caselle", "Steve Schroeter, UCSB" = "University of California Santa Barbara: Steve Schroeter", "UC Santa Barbara" = "University of California Santa Barbara"))

# Reef Check
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Annie Bauer-Civiello" = "Reef Check: Annie Bauer-Civiello", "Annie Bauer-Civiello, Reef Check California" = "Reef Check: Annie Bauer-Civiello", "Annie Bauer-Civiello, ReefCheck" = "Reef Check: Annie Bauer-Civiello", "Reef" = "Reef Check", "reef check" = "Reef Check", "Reef check" = "Reef Check", "Reef Check California" = "Reef Check", "Reef Check Central California" = "Reef Check", "Reef Check Foundation" = "Reef Check", "Reef Check Ian Norton" = "Reef Check: Ian Norton", "Reef Check in Sonoma & Mendocino as volunteer" = "Reef Check", "Reef Check Kelp Forest Program" = "Reef Check", "Reef Check KFM" = "Reef Check", "Reef Check Morgan Murphy Cannella" = "Reef Check: Morgan Murphy-Cannella", "Dan Abbot, Reef Check" = "Reef Check: Dan Abbott", "Dan Abbott" = "Reef Check: Dan Abbott", "Jan freiwald" = "Reef Check: Jan Freiwald", "Jan Freiwald" = "Reef Check: Jan Freiwald", "Morgan Murphy-Canella Reefcheck" = "Reef Check: Morgan Murphy-Cannella", "RCCA Ian Norton- northern ca. coordinator" = "Reef Check: Ian Norton", "RCCA Morgan Murphy Canola- northrn CA coordinator" = "Reef Check: Morgan Murphy-Cannella", "Reef Check of California" = "Reef Check", "Reef Check So Cal region" = "Reef Check", "Reef Check Southern California" =  "Reef Check", "Reef check staff in Sonoma, Mendocino, and Monterey" = "Reef Check", "Reef Check, Dr. Jan Friewald" = "Reef Check: Jan Freiwald", "Reef Check, Jan Friewald" = "Reef Check: Jan Freiwald", "Reef Check, Morgan Murphy-Cannella" = "Reef Check: Morgan Murphy-Cannella", "Reefcheck" = "Reef Check", "ReefCheck" = "Reef Check", "Reefcheck CA"= "Reef Check", "ReefCheck CA" = "Reef Check", "ReefCheck; Jan Friewald" = "Reef Check: Jan Freiwald", "reef check ca." = "Reef Check", "Reef check so cal" = "Reef Check", "Reef Check, Jan Freiwald" = "Reef Check: Jan Freiwald"))

# California Department of Fish and Wildlife
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Anthony Shiao, California Department of Fish and Wildlife" = "California Department of Fish and Wildlife: Anthony Shiao", "CDFW - Not sure region, dove off the R/V Garibaldi with Reef Check" = "California Department of Fish and Wildlife", "CA DFW" = "California Department of Fish and Wildlife", "CDFW" = "California Department of Fish and Wildlife", "Calif. Dept. of Fish and Wildlife" = "California Department of Fish and Wildlife", "California Department of Fish and Wildlife Marine Region" = "California Department of Fish and Wildlife - Marine Region", "California Department of Fisheries and Wildlife" = "California Department of Fish and Wildlife", "California Fish and Wildlife" = "California Department of Fish and Wildlife", "CDFW Marine Region" = "California Department of Fish and Wildlife - Marine Region", "CDFW out of San Pedro" = "California Department of Fish and Wildlife", "CDFW Region 7" = "California Department of Fish and Wildlife - Marine Region", "CDFW, Laura Rogers-Bennett" = "California Department of Fish and Wildlife: Laura Rogers-Bennett", "Dr. Laura Rogers-Bennett" = "California Department of Fish and Wildlife: Laura Rogers-Bennett", "Fish and Wildlife" = "California Department of Fish and Wildlife", "Ian Kelmartin, CDFW" = "California Department of Fish and Wildlife: Ian Kelmartin", "Kirsten Ramey, CDFW" = "California Department of Fish and Wildlife: Kirsten Ramey", "Kristen Elsmore" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore  DFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore - CDFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore (CDFW--kelp)" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore (CDFW)" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore, CDFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristin Elsmore, CDFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Mike Prall, CDFW" = "California Department of Fish and Wildlife: Mike Prall", "The California Department of Fish and Wildlife" = "California Department of Fish and Wildlife", "CALIFORNIA DEPARTMENT OF FISH AND WILDLIFE" = "California Department of Fish and Wildlife"))

# Montrose Settlements Restoration Program
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "NOAA Montrose Settlements Restoration Program" = "Montrose Settlements Restoration Program"))

# Anthropocene Institute
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Anthropogene Institute" = "Anthropocene Institute"))

# Monterey Bay Aquarium
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "April Ridlon (Monterey Bay Aquarium)" = "Monterey Bay Aquarium: April Ridlon", "Josh Smith (Monterey Bay Aquarium)" = "Monterey Bay Aquarium: Josh Smith", "Josh Smith, Monterey Bay Aquarium" = "Monterey Bay Aquarium: Josh Smith", "MB Aquarium" = "Monterey Bay Aquarium", "Monterey Bay Aquarium - Joshua Smith" = "Monterey Bay Aquarium: Josh Smith", "The Monterey Bay Aquarium" = "Monterey Bay Aquarium"))

# Monterey Bay National Marine Sanctuary
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "MBNMS" = "Monterey Bay National Marine Sanctuary", "MBNMS (Lonhart)" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "NOAA MBNMS" = "Monterey Bay National Marine Sanctuary", "NOAA MBNMS Sanctuary Advisory Council (I spoke recently at a meeting about kelp)" = "Monterey Bay National Marine Sanctuary - Monterey Bay National Marine Sanctuary Advisory Council", "Steve Lonhart" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "Steve Lonhart - Monterey Bay National Marine Sanctuary" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "Steve Lonhart (MBNMS)" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "Steve Lonhart, MBNMS" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "NOAA Monterey Bay National Marine Sanctuary" = "Monterey Bay National Marine Sanctuary", "Monterey Bay National Marine Sanctuary - Steve Lonhart" = "Monterey Bay National Marine Sanctuary: Steve Lonhart"))

# Monterey Bay Mermaid
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Monterey Bay Mermaid # Alison Smith" = "Monterey Bay Mermaid: Alison Smith"))

# Monterey Abalone Company
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Art Seavey, Monterey Abalone Company" = "Monterey Abalone Company: Art Seavey"))

# Monterey County Weekly
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "MC Weekly" = "Monterey County Weekly"))

# National Oceanic and Atmospheric Administration
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Bay Net" = "National Oceanic and Atmospheric Administration - Bay Net", "CA coastal national marine sanctuaries" = "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries - West Coast Region", "Cameron Spier, SWFSC" = "National Oceanic and Atmospheric Administration - Southwest Fisheries Science Center: Cameron Spier", "NOAA" = "National Oceanic and Atmospheric Administration", "NOAA NCCOS" = "National Oceanic and Atmospheric Administration - National Centers for Coastal Ocean Science", "Office of National Marine Sanctuaries - Multiple" = "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries", "Zach Gold, National Oceanic and Atmospheric Administration" = "National Oceanic and Atmospheric Administration: Zach Gold", "NOAA Office of National Marine Sanctuaries" = "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries"))

# California State University Long Beach
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Bengt Allen, CSU Long Beach" = "California State University Long Beach: Bengt Allen", "CSU Long Beach" = "California State University Long Beach", "CSU Long Beach" = "California State University Long Beach", "CSULB" = "California State University Long Beach"))

# The Nature Conservancy
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Benjaman.grimes@tnc" = "The Nature Conservancy: Ben Grime", "Benjamin Grime, TNC" = "The Nature Conservancy: Ben Grime", "everyone on the Eastern Pacific Kelp Restoration forum via The Nature Conservancy (BC to Baja)" = "The Nature Conservancy - Eastern Pacific Kelp Restoration Forum", "Jono Wilson, The Nature Conservancy" = "The Nature Conservancy: Jono Wilson", "Nature conservancy" = "The Nature Conservancy", "Nature Conservancy" = "The Nature Conservancy", "Norah Eddy, TNC CA Oceans Program" = "The Nature Conservancy: Norah Eddy", "The Nature Conservancey CA" = "The Nature Conservancy", "The nature conservancy" = "The Nature Conservancy", "The Nature Conservancy Australia and Tas" = "The Nature Conservancy", "The Nature Conservancy of California" = "The Nature Conservancy", "The Nature Conservancy, Tristin McHugh" = "The Nature Conservancy: Tristin McHugh", "The Nature Conservency- Tristen McHugh" = "The Nature Conservancy: Tristin McHugh", "TNC" = "The Nature Conservancy", "TNC - Tristin McHugh" = "The Nature Conservancy: Tristin McHugh", "TNC Australia" = "The Nature Conservancy", "TNC California" = "The Nature Conservancy", "TNC New Zealand" = "The Nature Conservancy", "Tom Dempsey, TNC" = "The Nature Conservancy: Tom Dempsey", "Tristan McHugh, The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh - The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh (The Nature Conservancy)" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh with TNC" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh- TNC" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, Kelp Forest Alliance" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, TNC" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, TNC CA Oceans Program / Reef Check" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh - Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh (Nature Conservancy)" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh Nature Conservnacy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh"))

# University of California San Diego
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Birch Aquarium" = "University of California San Diego - Scripps Institution of Oceanography - Birch Aquarium", "Ed Parnell, Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography: Ed Parnell", "Ed Parnell, UCSD" = "University of California San Diego - Scripps Institution of Oceanography: Ed Parnell", "Jen Smith at SCRIPS" = "University of California San Diego - Scripps Institution of Oceanography: Jen Smith", "Kristin Riser, Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography: Kristin Riser", "Paul Dayton, Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography: Paul Daytona", "Scripps" = "University of California San Diego - Scripps Institution of Oceanography", "Scripps Instituion of Oceanography; Ed Parnell" = "University of California San Diego - Scripps Institution of Oceanography: Ed Parnell", "Scripps Institute of Oceanography: Masters of Advanced Studies in Marine Biodiversity & Conservation (MAS MBC)" = "University of California San Diego - Scripps Institution of Oceanography", "Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography", "SIO" = "University of California San Diego - Scripps Institution of Oceanography", "UC San Diego" = "University of California San Diego", "UCSD Scripps Institute of Oceanography - Jim Leichter" = "University of California San Diego - Scripps Institution of Oceanography: Jim Leichter", "Scripps Inst. of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography"))

# San Diego State University
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Dr Mathew Edwards" = "San Diego State University: Matthew Edwards", "Matt Edwards, San Diego State University" = "San Diego State University: Matthew Edwards", "Matthew Edwards - San Diego State University" = "San Diego State University: Matthew Edwards", "Matthew Edwards, San Diego State University" = "San Diego State University: Matthew Edwards", "Shelby Penn SDSU" = "San Diego State University: Shelby Penn", "San Diego state university" = "San Diego State University")) 

# Blue Harmony
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Blue Harmony Foundation" = "Blue Harmony"))

# University of California Davis
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "BML" = "University of California Davis - Bodega Marine Laboratory", "Bodega Marine Labs" = "University of California Davis - Bodega Marine Laboratory", "Bodega Bay Marine Labs" = "University of California Davis - Bodega Marine Laboratory", "DISES" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "DiSES ppl" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Gabby Yang" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Gabby Yang", "Kelp RISES personnel" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Kelp RISES project personnel, students at UCD, UCSC, Cal Poly Humboldt" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Kelp RISES Team, UC Davis" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Marissa Baskett, UCD" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Marissa Baskett (UCD)" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Marissa Baskett - Kelp RISES" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Marissa Baskett, uc davis" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Michael Springborn (UCD)" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Mike springborn" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Mike springborn, uc davis" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Mike Springborn, UCD" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Rachael Bay, UC Davis" = "University of California Davis: Rachael Bay", "The University of California, Davis" = "University of California Davis", "UC Davis" = "University of California Davis", "UC Davis (Baskett et al)" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "UC Davis Bodega Marine Lab - John Largier" = "University of California Davis - Bodega Marine Laboratory: John Largier", "UC Davis Bodega Marine Lab Boating and Diving Safety Office" = "University of California Davis - Bodega Marine Laboratory - Boating and Diving Safety Office", "UC Davis bodega marine labs" = "University of California Davis - Bodega Marine Laboratory", "UC Davis Department of Environmental Science and Policy" = "University of California Davis", "UC Davis- BML" = "University of California Davis - Bodega Marine Laboratory", "Uc davis" = "University of California Davis", "UC Davis, DiSES" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "UC Davis, Kelp RISES team" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "University of California Davis Kelp RISES Project" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "University of California, Davis" = "University of California Davis", "University of California, Davis Coastal & Marine Sciences Institute" = "University of California Davis"))

# Bureau of Ocean Energy Management
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "BOEM" = "Bureau of Ocean Energy Management"))

# Sonoma State University
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Rachael Karm- SSU" = "Sonoma State University: Rachael Karm", "Brent Hughes - Sonoma State Univ." = "Sonoma State University: Brent Hughes", "Brent Hughes (Sonoma State University)" = "Sonoma State University: Brent Hughes", "Sonoma State" = "Sonoma State University", "Hughes Lab, Sonoma State University" = "Sonoma State University - Hughes Lab", "Sonoma State University - Brent Hughes" = "Sonoma State University: Brent Hughes", "Brent Hughes, Sonoma State University" = "Sonoma State University: Brent Hughes", "SSU" = "Sonoma State University", "Brent Hughes (Sonoma State)" = "Sonoma State University: Brent Hughes", "Brent Hughes- SSU" = "Sonoma State University: Brent Hughes", "Brent Hughes, Sonoma State" = "Sonoma State University: Brent Hughes", "Rachel Karm SSU" = "Sonoma State University: Rachael Karm", "Somona State University (Hughes lab)" = "Sonoma State University - Hughes Lab", "Sonoma State University: Rachel Karm" = "Sonoma State University: Rachael Karm"))

# California Coastal Commission
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CA Coastal Commission" = "California Coastal Commission", "Coastal Commission" = "California Coastal Commission"))

# California State Parks 
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CA Dept. of Parks & Recreation - Santa Cruz" = "California State Parks", "CA DPR" = "California State Parks", "CA State Parks - Fort Ross" = "California State Park - Fort Ross Conservancy", "California State Parks Interpretation & Education Division" = "California State Parks - Interpretation and Education Division", "California State Parks Natural Resources Division" = "California State Parks - Natural Resources Division", "California State Parks - Interpretation & Education Division" = "California State Parks - Interpretation and Education Division", "State Parks" = "California State Parks", "The Fort Ross Conservancy" = "California State Parks - Fort Ross Conservancy"))

# California Diving News
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CA DIving News" = "California Diving News"))

# California Academy of Sciences
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Steinhart Aquarium (California Academy of Sciences)" = "California Academy of Sciences"))

# California Fish and Game Commission
kelp.edge <-  kelp.edge %>%
  mutate_all(~ recode(., "CA Fish and Game Commission" = "California Fish and Game Commission", "CA Fish and Game Commission - Susan Ashcraft" = "California Fish and Game Commission: Susan Ashcraft", "CFGC" = "California Fish and Game Commission", "Fish & Game" = "California Fish and Game Commission"))

# California Ocean Science Trust
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Ocean Science Trust" = "California Ocean Science Trust", "CA Ocean Science Trust" = "California Ocean Science Trust", "Ocean Science Trust staff" = "California Ocean Science Trust"))

# California Ocean Protection Council
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CA OPC" = "California Ocean Protection Council", "Calif Ocean Protection Council" = "California Ocean Protection Council", "Mike Esgro, Ocean Protection Council" = "California Ocean Protection Council: Mike Esgro", "ocean protection council" = "California Ocean Protection Council", "Ocean Protection Council" = "California Ocean Protection Council", "Ocean Protection Council- Michael Esgro" = "California Ocean Protection Council: Mike Esgro", "Ocean Protection Council; Pike Spector" = "California Ocean Protection Council: Pike Spector", "OPC" = "California Ocean Protection Council", "Pike Spector, OPC" = "California Ocean Protection Council: Pike Spector"))

# California Sea Grant
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CA Sea Grant" = "California Sea Grant", "CA SeaGrant" = "California Sea Grant", "CA Seagrant (Oh)" = "California Sea Grant: Shauna Oh", "Cal seagrant" = "California Sea Grant", "california sea grant" = "California Sea Grant", "Jami Miller, Sea Grant / City of Fort Bragg" = "California Sea Grant: Jami Miller", "Sea grant" = "California Sea Grant", "Sea Grant" = "California Sea Grant", "Shauna Oh, California SeaGrant" = "California Sea Grant: Shauna Oh"))

# California Sea Urchin Commission
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "California Sea Urchin Commission (CSUC)" = "California Sea Urchin Commission", "California sea urchin commission" = "California Sea Urchin Commission", "CA Sea Urchin Commission" = "California Sea Urchin Commission", "Sea urchin commission" = "California Sea Urchin Commission", "California Sea Urchin Commission, David Goldenberg" = "California Sea Urchin Commission: David Goldenberg", "David Goldenburg" = "California Sea Urchin Commission: David Goldenberg", "David Goldenburg  California sea urchin commission" = "California Sea Urchin Commission: David Goldenberg", "The Urchin Commission" = "California Sea Urchin Commission", "Grant Downey Californina sea urchin commission" = "California Sea Urchin Commission: Grant Downie", "Cal Urchin Commission" = "California Sea Urchin Commission", "California Sea Urchin Commision- David Goldenberg" = "California Sea Urchin Commission: David Goldenberg", "California Sea Urchin Commission - David Goldenberg" = "California Sea Urchin Commission: David Goldenberg", "Grant Downie, The California Sea Urchin Commission" = "California Sea Urchin Commission: Grant Downie", "California Sea Urchin Comission" = "California Sea Urchin Commission", "California Sea Urchin Commission: Grant Downey" = "California Sea Urchin Commission: Grant Downie", "URCHIN COMMISSION" = "California Sea Urchin Commission"))

# California State Lands Commission
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CA State Lands Commission" = "California State Lands Commission"))

# California State Polytechnic University Humboldt
kelp.edge <- kelp.edge %>%
  mutate_all(~recode(., "Cal Poly Humboldt - Biology department (Sean Craig)" = "California State Polytechnic University Humboldt: Sean Craig", "Cal Poly Humboldt - Fisheries department/Aquaculture" = "California State Polytechnic University Humboldt", "Cal Poly Humboldt - Oceanography department (Christine Cass)" = "California State Polytechnic University Humboldt: Christine Cass", "Cal Poly Humboldt, Food Sovereignty Lab, Dr. Cutcha Risling Baldy" = "California State Polytechnic University Humboldt - Food Sovereignty Lab: Cutucha Risling Baldy", "Cal Poly Humboldt, Jody Martinez" = "California State Polytechnic University Humboldt: Jody Martinez", "CalPoly Humboldt" = "California State Polytechnic University Humboldt", "Duncan Jackson" = "California State Polytechnic University Humboldt: Duncan Jackson", "Frankie Moitoza (Cal Poly Humboldt)" = "California State Polytechnic University Humboldt: Frankie Moitoza", "HSU" = "California State Polytechnic University Humboldt", "HSU (Craig)" = "California State Polytechnic University Humboldt: Sean Craig", "HSU Rick Alvarez" = "California State Polytechnic University Humboldt: Rick Alvarez", "Humbolt state university" = "California State Polytechnic University Humboldt", "NEREO, Cal Poly Humboldt" = "California State Polytechnic University Humboldt: Northcoast Evaluation of Reef Ecosystems Organization", "Paul Bourdeau" = "California State Polytechnic University Humboldt: Paul Bourdeau", "Rafael Cuevas Uribe" = "California State Polytechnic University Humboldt: Rafael Cuevas Uribe", "Rafael Cuveus-Uribe, Cal Poly Humboldt" = "California State Polytechnic University Humboldt: Rafael Cuevas Uribe", "Sean Craig" = "California State Polytechnic University Humboldt: Sean Craig", "Sean Craig (Cal Poly Humboldt)" = "California State Polytechnic University Humboldt: Sean Craig", "Telonicher Marine Lab" = "California State Polytechnic University Humboldt - Telonicher Marine Lab", "Cal Poly Humboldt" = "California State Polytechnic University Humboldt", "Cal Poly Humboldt-NEREO" = "California State Polytechnic University Humboldt: Northcoast Evaluation of Reef Ecosystems Organization", "NEREO" = "California State Polytechnic University Humboldt: Northcoast Evaluation of Reef Ecosystems Organization"))

# California State Polytechnic University Pomona
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Cal Poly Pomona" = "California State Polytechnic University Pomona"))

# California State University Northridge
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Cal State Northridge" = "California State University Northridge", "Janet Kubler scientist CSUN" = "California State University Northridge: Janet Kubler"))

# Marine Protected Area Collaborative Network
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Calla Allison and the MPA Collaborative Network" = "Marine Protected Area Collaborative Network: Calla Allison", "Monterey MPACN" = "Marine Protected Area Collaborative Network", "MPA CN" = "Marine Protected Area Collaborative Network", "MPA Collaborative Network" = "Marine Protected Area Collaborative Network", "MPA Collaborative Network, Humboldt Co." = "Marine Protected Area Collaborative Network", "MPA Collaboratives" = "Marine Protected Area Collaborative Network", "MPA Long Term Monitoring Project" = "Marine Protected Area Collaborative Network"))

# Giant Giant Kelp Restoration Project
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Caspar Cove kelp restoration project Jon Holcomb" = "Giant Giant Kelp Restoration Project - Caspar Cove Project: Jon Holcomb", "G 2 Kelp Restoration Keith Rootseart" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "G2KR" = "Giant Giant Kelp Restoration Project", "g2kr.com" = "Giant Giant Kelp Restoration Project", "Giant Giant Kelp - Keith Rootsaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Giant giant kelp restoration" = "Giant Giant Kelp Restoration Project", "Giant Giant Kelp Restoration" = "Giant Giant Kelp Restoration Project", "Giant Giant Kelp Restoration - Keith Rootsaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Giant Giant Kelp Restoration Project - Caspar Cove" = "Giant Giant Kelp Restoration Project - Caspar Cove Project", "Giant Giant Kelp Restoration Project (G2KR)" = "Giant Giant Kelp Restoration Project", "giant giant kep restoration" = "Giant Giant Kelp Restoration Project", "Giant Kelp Restoration Project" = "Giant Giant Kelp Restoration Project", "Great Great Kelp Restoration Project" = "Giant Giant Kelp Restoration Project", "Great great kelp restoration project (Monterey)" = "Giant Giant Kelp Restoration Project", "Keith  Rootsart" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rooseart and the Giant Giant Kelp Restoration Project" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rooseart" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rooseart - G2KR" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rootseart, Giant giant kelp restoration project" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rootsaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rootsaert - G2KR" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "The Giant Giant Kelp restoration project" = "Giant Giant Kelp Restoration Project", "Great Great Kelp Restoration" = "Giant Giant Kelp Restoration Project", "https://www.facebook.com/UrchinsKelpOtters" = "Giant Giant Kelp Restoration Project", "Keith Roostaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert"))

# Watermen's Alliance
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Caspar Cove Project, Waterman's Alliance" = "Watermen's Alliance - Caspar Cove Project", "Josh Russo Waterman's alliance" = "Watermen's Alliance: Josh Russo", "The Waterman's Alliance, Josh Russo" = "Watermen's Alliance: Josh Russo", "watermans alliance" = "Watermen's Alliance", "Watermans Alliance" = "Watermen's Alliance", "Watermans Alliance Joshua Russo" = "Watermen's Alliance: Josh Russo", "Watermen's Alliance" = "Watermen's Alliance", "Watermen's Alliance Urchin Culling events Caspar Cove as urchin culler" = "Watermen's Alliance - Caspar Cove Project", "Watermen's Alliance Urchin Removal - Josh Russo" = "Watermen's Alliance: Josh Russo", "The Watermen's Alliance" = "Watermen's Alliance", "Waterman's Alliance" = "Watermen's Alliance"))

# Catalina Island Marine Institute
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Catalina island marine institute" = "Catalina Island Marine Institute", "Deidre Sullivan  - Catalina Island Marine Institute" = "Catalina Island Marine Institute: Deidre Sullivan"))

# Cordell Bank National Marine Sanctuary
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CBNMS Advisory Council" = "Cordell Bank National Marine Sanctuary - Cordell Bank National Marine Sanctuary Advisory Council"))

# Channel Islands National Marine Sanctuary
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Channel Islands National Marine Sanctuary - Ryan Freedman" = "Channel Islands National Marine Sanctuary: Ryan Freedman", "Channel Islands National Marine Sanctuary Advisory Council" = "Channel Islands National Marine Sanctuary - Channel Islands National Marine Sanctuary Advisory Council", "Channel Islands National Park" = "Channel Islands National Marine Sanctuary", "CINMS (Freedman)" = "Channel Islands National Marine Sanctuary: Ryan Freedman", "NOAA CINMS" = "Channel Islands National Marine Sanctuary", "Scott Gabara Channel Islands National Park" = "Channel Islands National Marine Sanctuary: Scott Gabara", "Scott Gabara, Channel Islands National Parks" = "Channel Islands National Marine Sanctuary: Scott Gabara", "NOAA Channel Islands National Marine Sanctuary" = "Channel Islands National Marine Sanctuary"))

# Fish Reef Project 
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Chris Goldblatt of Fish Reef Project (fishreef.org)" = "Fish Reef Project: Chris Goldblatt"))

# City of Fort Bragg
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "City of fort bragg" = "City of Fort Bragg", "City of Ft Bragg" = "City of Fort Bragg", "Sarah McCormick, City of Fort Bragg" = "City of Fort Bragg: Sarah McCormick"))

# Redwood Elementary
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Redwood Elementary School, Fort Bragg" = "Redwood Elementary", "Redwood Elementary, Fort Bragg Unified School District" = "Redwood Elementary"))

# Coastal Conservation Association California
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Coastal Conservation Association - California Chapter" = "Coastal Conservation Association California", "Hank Goebel" = "Coastal Conservation Association California: Hank Goebel"))

# The Sea Ranch Association 
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Coastal Stewardship Task Force, The Sea Ranch Association" = "The Sea Ranch Association: Coastal Stewardship Task Force"))

# United States Air Force
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Col. David Lopez, USAF (ret.)" = "United States Air Force: David Lopez"))

# California State University Agricultural Research Institute
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CSU Agricultural Research Institute" = "California State University Agricultural Research Institute"))

# California State University Monterey Bay
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CSU Monterey Bay" = "California State University Monterey Bay", "CSUMB" = "California State University Monterey Bay"))

# California State University Chico
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CSUC" = "California State University Chico"))

# University of California Berkeley
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Dan Okamoto (UCB)" = "University of California Berkeley: Dan Okamoto", "Dan Okamoto- UC Berkeley" = "University of California Berkeley: Dan Okamoto", "Dr. Dan Okamoto" = "University of California Berkeley: Dan Okamoto", "Maya Munstermann, expert scientist" = "University of California Berkeley: Maya Munstermann", "UC Berkeley" = "University of California Berkeley", "UC Berkeley Dan Okamoto" = "University of California Berkeley: Dan Okamoto", "University of Californa" = "University of California Berkeley", "University of California" = "University of California Berkeley"))

# Occidental College
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Dan Pondella (Occidental College)" = "Occidental College - Vantuna Research Group: Dan Pondella", "Occidental VRG (Pondella)" = "Occidental College - Vantuna Research Group: Dan Pondella", "Occidental College: Dan Pondella" = "Occidental College - Vantuna Research Group: Dan Pondella", "Vantuna Research Group, Occidental College" = "Occidental College - Vantuna Research Group"))

# Kashia Band of Pomo Indians
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Dan sweezy" = "Kashia Band of Pomo Indians: Dan Swezey", "Dr. Dan Sweazy" = "Kashia Band of Pomo Indians: Dan Swezey", "KASHIA" = "Kashia Band of Pomo Indians", "Kashia Band of Pomo Indians (Dan Sweezy)" = "Kashia Band of Pomo Indians: Dan Swezey", "Kashia Band of Pomo Indians, Dan Sweezy" = "Kashia Band of Pomo Indians: Dan Swezey"))

# Dandy Fish Company
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Dandy Fish Company (fish processor)" = "Dandy Fish Company"))

# DeeperBlue
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Deeperblue.com" = "DeeperBlue"))

# Ocean Rainforest
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Doug Bush, Ocean Rainforest" = "Ocean Rainforest: Doug Bush", "Ocean Rainforest, Inc." = "Ocean Rainforest"))

# The Cultured Abalone Farm
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Doug Bush, The Cultured Abalone" = "The Cultured Abalone Farm: Doug Bush", "The Cultured Abalone Farm Rick Gutierrez" = "The Cultured Abalone Farm: Rick Gutierrez", "The Cultured Abalone Farm LLC" = "The Cultured Abalone Farm"))

# Noyo Center for Marine Science
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Elizabeth Carpenter & NOYO Center - outreach webinar for Sea Otter Awareness Week 2023" = "Noyo Center for Marine Science: Elizabeth Carpenter", "Noyo Center" = "Noyo Center for Marine Science", "NOYO CENTER" = "Noyo Center for Marine Science", "Noyo Marine Science Center" = "Noyo Center For Marine Science", "Noyo Marine Science Center (worked with directly)" = "Noyo Center For Marine Science", "Noyo Ocean Center" = "Noyo Center For Marine Science", "Noyo Science Center" = "Noyo Center For Marine Science", "Sheila Semans, NOYO Center" = "Noyo Center For Marine Science: Sheila Semans", "Sheila Semans, Noyo Center" = "Noyo Center For Marine Science: Sheila Semans", "Sheila Semans" = "Noyo Center For Marine Science: Sheila Semans"))

# University of Washington
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Friday Harbor Lab" = "University of Washington - Friday Harbor Laboratories", "Friday Harbor Lab, University of Washington (Hodin lab)" = "University of Washington - Friday Harbor Laboratories - Hodin Lab", "University of Washington, Friday Harbor Labs" = "University of Washington - Friday Harbor Laboratories"))

# Washington State Department of Natural Resources
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "WA DNR; Helen Berry" = "Washington State Department of Natural Resources: Helen Berry"))

# Hog Island Oyster Company
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Gary Fleener, Hog Island Oyster Company" = "Hog Island Oyster Company: Gary Fleener"))

# Elkhorn Slough Ecological Reserve
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "ESNERR" = "Elkhorn Slough Ecological Reserve"))

# Get Inspired
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Get Inspired, Nancy Caruso" = "Get Inspired: Nancy Caruso", "My own kelp monitoring program (Get Inspired ), Nancy Caruso" = "Get Inspired"))

# Girl Scouts of America
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Girl Scouts of America - Cheryl Kingman" = "Girl Scouts of America: Cheryl Kingman"))

# Gwaii Haanas National Park Reserve
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "GWAII HAANAS" = "Gwaii Haanas National Park Reserve"))

# Council of the Haida Nation
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "HAIDA FISHERIES" = "Council of the Haida Nation -  Haida Fisheries Program"))

# California Marine Sanctuary Foundation
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Hallie Brown, CA Marine Sanctuary Foundation" = "California Marine Sanctuary Foundation: Hallie Brown"))

# The Bay Foundation
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Heather Burdick Bay Foundation" = "The Bay Foundation: Heather Burdick"))

# Surfrider Foundation
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Humboldt Surfrider" = "Surfrider Foundation - Surfrider Humboldt"))

# InterTribal Sinkyone Wilderness Council
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Inter Tribal Sinkyone Wilderness Council" = "InterTribal Sinkyone Wilderness Council"))

# University of Massachusetts Boston
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Jarrett Byrnes, University of Massachusetts Boston" = "University of Massachusetts Boston: Jarrett Byrnes"))

# University of Nevada Reno
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Jeremy McFarland, University of Nevada, Reno" = "University of Nevada Reno: Jeremy McFarland"))

# Trinidad Rancheria
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Jessica Gravelle, Trinidad Rancheria" = "Trinidad Rancheria: Jessica Gravelle", "The Cher-Ae Heights Indian Community of the Trinidad Rancheria" = "Trinidad Rancheria"))

# Marine Conservation Institute
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Jorge Arroyo-Esquival & Marine Conservation Institute - outreach webinar for Sea Otter Awareness Week 2023" = "Marine Conservation Institute: Jorge Arroyo Esquival"))

# Kelp Restoration and Management Plan Scientific Advisory Team
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Kelp restoration management plan - CA" = "Kelp Restoration and Management Plan Scientific Advisory Team", "Kelp Restoration Management Plan Scientific Advisory Team" = "Kelp Restoration and Management Plan Scientific Advisory Team"))

# Kelp Restoration and Management Plan Community Working Group
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "KRMP" = "Kelp Restoration and Management Plan Community Working Group", "KRMP (CDFW)" = "Kelp Restoration and Management Plan Community Working Group", "California Kelp Restoration and Management Community Group" = "Kelp Restoration and Management Plan Community Working Group", "Members of the KRMP stakeholder group" = "Kelp Restoration and Management Plan Community Working Group"))

# LAWaterkeeper
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "LA Waterkeeper - Michael Quill" = "LAWaterkeeper: Michael Quill"))

# University of California Merced
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Lauren Schiebelhut" = "University of California Merced: Lauren Schiebelhut", "Michael Dawson" = "University of California Merced: Michael Dawson", "Mike Dawson, UC Merced" = "University of California Merced: Michael Dawson", "UC Merced" = "University of California Merced", "UC Merced (Dawson lab)" = "University of California Merced - Dawson Lab"))

# Sepia Lux 
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Loren Crotty (private fundraising event)" = "Sepia Lux: Loren Crotty"))

# National Fish and Wildlife Foundation
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "National Fish and Wildlife Foundation - Multiple" = "National Fish and Wildlife Foundation"))

# National Center for Ecological Analysis and Synthesis
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "NCEAS kelp working group (lead is Eger)" = "National Center for Ecological Analysis and Synthesis"))

# National Marine Sanctuary Foundation
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "NMSF" = "National Marine Sanctuary Foundation", "NOAA NMSF" = "National Marine Sanctuary Foundation", "NOAA NMS" = "National Marine Sanctuary Foundation", "NOAA Sanctuaries" = "National Marine Sanctuary Foundation", "NOAA NMFS" = "National Marine Sanctuary Foundation"))

# Noozhawk
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "noozhawk.com (online news service)" = "Noozark"))

# Oregon Department of Fish and Wildlife
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "OR DFW; Scott Marion" = "Oregon Department of Fish and Wildlife: Scott Marion", "Oregon Department of Fisheries and Wildlife" = "Oregon Department of Fish and Wildlife"))

# Oregon Kelp Alliance
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Oregon kelp alliance" = "Oregon Kelp Alliance", "Oregon Kelp Alliance: Thomas Calvanes" = "Oregon Kelp Alliance: Tom Calvanes", "Oregon Kelp Forest Alliance" = "Oregon Kelp Alliance", "ORKA, Tom Calvanese" = "Oregon Kelp Alliance: Tom Calvanes"))

# San Jose State University
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "others at SJSU" = "San Jose State University"))

# Port of San Diego
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Paula Sylvia at the Port of San Diego" = "Port of San Diego: Paula Sylvia"))

# Port of Los Angeles
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Port of LA" = "Port of Los Angeles"))

# Puget Sound Restoration Fund
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Puget sound restoration fund" = "Puget Sound Restoration Fund", "Puget Sound Restoration Fund in seattle washington" = "Puget Sound Restoration Fund"))

# Partnership for Interdisciplinary Studies of Coastal Oceans
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "PISCO" = "Partnership for Interdisciplinary Studies of Coastal Oceans"))

# Pycnopodia Recovery Working Group
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Pycnopodia Restoration Group" = "Pycnopodia Recovery Working Group"))

# GreenWave
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Green Wave" = "GreenWave"))

# Reef Environmental Education Foundation
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "REEF,  Janna Nichols" = "Reef Environmental Education Foundation: Janna Nichols"))

# United States Congress
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Rep. Huffman (district 2)" = "United States Congress: Jared Huffman"))

# United States Congress
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Rep. Huffman (district 2)" = "United States Congress: Jared Huffman"))

# Universidad Autonoma de Baja California Mexico
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Rodrigo Beas-Lunas (ABC Ensenada)" = "Universidad Autonoma de Baja California Mexico: Rodrigo Beas-Lunas"))

# Tolowa Dee-ni' Nation
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Rosa Laucci, Tolowa Dee-ni' Nation" = "Tolowa Dee-ni' Nation: Rosa Laucci", "Tolowa Dee ni' Nation" = "Tolowa Dee-ni' Nation", "Tolowa Dee-Ni' Nation" = "Tolowa Dee-ni' Nation"))

# Salesforce
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "SalesForce" = "Salesforce"))

# San Diego Association of Governments
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "SANDAG" = "San Diego Association of Governments"))

# Save Our Shores
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Save our Shores" = "Save Our Shores"))

# Southern California Coastal Ocean Observing System
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "SCCOOS" = "Southern California Coastal Ocean Observing System"))

# Southern California Coastal Water Research Project
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "SCCWRP" = "Southern California Coastal Water Research Project"))

# Sherwood Valley Band of Pomo Indians
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Sherwood Valley Band of Pomo" = "Sherwood Valley Band of Pomo Indians"))

# Stanford University
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Stanford" = "Stanford University", "Stanford University - Steven Monismith" = "Stanford University: Steven Monismith"))

# California State Lands Commission
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "State Lands" = "California State Lands Commission"))

# Strategic Earth Consulting
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Strategic Earth" = "Strategic Earth Consulting", "Strategic Earth Inc." = "Strategic Earth Consulting"))

# Sunken Seaweed
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Sunken Seaweed LLC, Torre Polizzi" = "Sunken Seaweed: Torre Polizzi", "Torre Polizzi, Sunken Seaweed" = "Sunken Seaweed: Torre Polizzi", "Torre pollozi" = "Sunken Seaweed: Torre Polizzi"))

# The Jetlagged
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "The Jet Lagged (photographers)" = "The Jetlagged"))

# University of Southern California
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "U Southern California" = "University of Southern California"))

# United States Navy
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "U.S. Navy" = "United States Navy", "US Navy" = "United States Navy"))

# United States Geological Survey
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "USGS" = "United States Geological Survey", "U.S. Geological Survey" = "United States Geological Survey"))

# University of California Los Angeles
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "UCLA" = "University of California Los Angeles", "UCLA - Kyle Cavanaugh" = "University of California Los Angeles: Kyle Cavanaugh", "UCLA; Kyle Cavanuagh" = "University of California Los Angeles: Kyle Cavanaugh"))

# University of Miami 
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "University of Miami Rosenstiel School of Marine, Atmospheric, and Earth Science - Claire Paris" = "University of Miami: Claire Paris"))

# University of Oregon
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "UO OIMB Biology department - Aaron Galloway" = "University of Oregon: Aaron Galloway"))

# Pacific Urchin Harvesters Association
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "URCHIN HARVESTORS ASSOCIATION" = "Pacific Urchin Harvesters Association"))

# United States Fish and Wildlife Service
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "US Fish and Wildlife Service" = "United States Fish and Wildlife Service", "USFWS" = "United States Fish and Wildlife Service", "US Fish and Wildlife" = "United States Fish and Wildlife Service"))

# Woods Hole Oceanographic Institution
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "WHOI; Tom Bell" = "Woods Hole Oceanographic Institution: Tom Bell", "WHOI (Bell)" = "Woods Hole Oceanographic Institution: Tom Bell", "WHOI - Tom Bell" = "Woods Hole Oceanographic Institution: Tom Bell"))

# Earth Equity
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Earth equty" = "Earth Equity"))

# WSP
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "WSP (env consulting)" = "WSP"))

# West Coast Ocean Alliance
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "West Coast Ocean alliance" = "West Coast Ocean Alliance"))

# Individuals ---------------------------------------------------
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Byron Kohler (urchin diver" = "Urchin Diver: Byron Kohler", "Gary Trumper- urchin diver" = "Urchin Diver: Gary Trumper", "Mickey Kitahara- urchin diver" = "Urchin Diver: Mickey Kitahara", "Urchin removal diver" = "Urchin Diver"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Many Kelp Restoration Divers" = "Divers"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "commercial fishermen in San Diego" = "Fishers"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Urchin removal" = "Urchin Removers"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Dale Glanz" = "Dale Glanz"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Caspar Cove kelp restoration project Jon Holcomb" = "Commerical Fisher: Jon Holcomb"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Erik Owen (commercial fisher)" = "Commerical Fisher: Erik Owen"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Ernest (Tad) Thompson (commercial fisher)" = "Commercial Fisher: Ernest Thompson"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Jeffrey Gritsch (commercial fisher)" = "Commerical Fisher: Jeffrey Gritsch"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Mark Gmeiner (commercial fisher)" = "Commerical Fisher: Mark Gmeiner", " Commerical Fisher: Mark Gmeiner" = "Commerical Fisher: Mark Gmeiner"))

# Grant Downie is on the urchin commission, urchin diver, kelp restoration specialist, etc
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Grant Downey" = "Grant Downie", "Grant downie" = "Grant Downie", "Grant Downie (commercial fisher)" = "Fisher: Grant Downie", "Grant Downie (Commercial urchin diver, mendocino)" = "Urchin Diver: Grant Downie", "Grant Downie- urchin diver" = "Urchin Diver: Grant Downie"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Pat Downie (commercial fisher)" = "Fisher: Pat Downie"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Jeremy George Petty (commercial fisher)" = "Fisher: Jeremy George Petty"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Josie Iselin (kelp artist and educator)" = "Josie Iselin", "Josie Iselin, Author/Artist" = "Josie Iselin"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Kevin Quider (commercial fisher)" = "Fisher: Kevin Quider"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Patrick Webster, science communicator and underwater photographer" = "Photographer: Pat Webster"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Roger Carlson, Roger Carlson Photography" = "Photographer: Roger Carlson"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Ron McPeak" = "Photographer: Ron McPeak"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Shaun Wolfe, Shaun Wolfe Photography" = "Photographer: Shaun Wolfe"))

# Cleaning all the unnecessary stuff --------------------------------------

kelp.edge <- kelp.edge[kelp.edge$alter != "none", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "Multiple media outlets", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "And many others..", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "Its not clear to me how this is different from 'worked directly'", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "Keepers of the Kelp Forest documentary film", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "Many many others", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "Other credible kelp scientists", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "Same as above", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "Same as Above", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "Students and youth programs", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "too many to list", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "Tribes in North Coast", ]
kelp.edge <- kelp.edge[kelp.edge$alter != "And many others...", ]

# splitting up all the comma responses 
kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "Reef Check Dan Abbott, Maxwell Seal" = "Reef Check: Dan Abbott, Reef Check: Maxwell Seale", "CDFW Kelp management team (Elsmore, Ramey)" = "California Department of Fish and Wildlife: Kristen Elsmore, California Department of Fish and Wildlife: Kristen Ramey", "CA OPC (Esgro, Spector, Eckerle, Lewis)" = "California Ocean Protection Council: Mike Esgro, California Ocean Protection Council: Pike Spector, California Ocean Protection Council: Jen Eckerle, California Ocean Protection Council: Staci Lewis", "TNC CA (Eddy, Wilson, McHugh, Grimes)" = "The Nature Conservancy: Norah Eddy, The Nature Conservancy: Jono Wilson, The Nature Conservancy: Tristin McHugh, The Nature Conservancy: Ben Grimes", "UCSB (Ospina, J. Smith)" = "University of Santa Barbara: Anita Giraldo Ospina, University of Santa Barbara: Jason Smith", "UCLA (Kyle Cavanaugh, Kate Cavanaugh)" = "University of California Los Angeles: Kyle Cavanaugh, University of California Los Angeles: Kate Cavanaugh", "Mont Bay Aq (Ridlon, J. Smith)" = "Monterey Bay Aquarium: April Ridlon, Monterey Bay Aquarium: Josh Smith", "CSU schools - Sonoma, SJSU, MLML, Monterey" = "Sonoma State University, San Jose State University, Moss Landing Marine Laboratories, California State University Monterey Bay", "UC schools - Davis, BML, UCSC, UCSB, UCSD, Scripps" = "University of California Davis - Bodega Marine Laboratory, University of California Santa Cruz, University of California Santa Barbara, University of California San Diego - Scripps Institution of Oceanography", "Cal Poly Humboldt (Craig, Richmond & students)" = "California State Polytechnic University Humboldt: Sean Craig, California State Polytechnic University Humboldt: Laurie Richmond", "Greater Farallones Association (Hohman, Contolini)" = "Greater Farallones Association: Rietta Hohman, Greater Farallones Association: Gina Contolini", "Bay Net, Thom and Kim Akeman, Marge Brigadier" = "National Oceanic and Atmospheric Administration - Bay Net: Thom Akeman, National Oceanic and Atmospheric Administration - Bay Net: Kim Akeman, National Oceanic and Atmospheric Administration - Bay Net: Marge Brigadier", "MBNMS, GFNMS/GFA, TNC, MLML, RCCA, SSU, Watermen's Alliance, G2KR, OPC" = "Monterey Bay National Marine Sanctuary, Greater Farallones National Marine Sanctuary, Greater Farallones Association, The Nature Conservancy, Moss Landing Marine Laboratories, Reef Check, Sonoma State University, Watermen's Alliance, Giant Giant Kelp Restoration Project, California Ocean Protection Council", "Greater Farallones & Cordell Bank National Marine Sanctuaries" = "Greater Farallones National Marine Sanctuary, Cordell Bank National Marine Sanctuary", "Greater Farallones and Cordell Bank National Marine Sanctuaries" = "Greater Farallones National Marine Sanctuary, Cordell Bank National Marine Sanctuary", "Kashia Band of Pomo Indians - Nina Hapner and Dan Swezey" = "Kashia Band of Pomo Indians: Nina Hapner, Kashia Band of Pomo Indians: Dan Swezey", "Office of Habitat Conservation - Natalie C-Manning and Julia Royster" = "National Oceanic and Atmospheric Administration: Natalie Cosentino-Manning, National Oceanic and Atmospheric Administration: Julia Royster", "Opc SeaGrant cdfw + KRMP" = "California Ocean Protection Council, Kelp Restoration and Management Plan Community Working Group, California Sea Grant, California Department of Fish and Wildlife", "Morgan and Ian (North Coast Reef Check)" = "Reef Check: Morgan Murphy-Cannella, Reef Check: Ian Norton", "MBNMS Superintendent and Advisory Council" = "Monterey Bay National Marine Sanctuary: Lisa Wooninck, Monterey Bay National Marine Sanctuary: Dawn Hayes, Monterey Bay National Marine Sanctuary - Monterey Bay National Marine Sanctuary Advisory Council", "Norah Eddy/Tristan Hughes TNC" = "The Nature Conservancy: Norah Eddy, The Nature Conservancy: Tristin McHugh", "Jen Smith /Mohammad Sederat/Kalani Ortiz UCSC" = "University of California Santa Cruz: Jen Smith, University of California Santa Cruz: Mohammad Sederat, University of California Santa Cruz: Kalani Ortiz"))

kelp.edge <- kelp.edge %>%
  mutate_all(~ recode(., "CASG, UCSC, UCD/BML, The Bay Foundation, Woods Hole Oceanographic Inst., University of Wisconsin, Strategic Earth, CA State Parks," = "California Sea Grant, University of California Santa Cruz, University of California Davis - Bodega Marine Laboratory, The Bay Foundation, Woods Hole Oceanographic Institution, University of Wisconsin, Strategic Earth, California State Parks", "HSU, Noyo Science Center, CA. Sea Urchin Commission, CSU Monterey Bay, UCSD/SIO, UCLA, SDSU, FishBio, Monterey Bay Seaweeds, USC, Stanford, UCLA, UCI," = "California State Polytechnic University Humboldt, Noyo Center for Marine Science, California Sea Urchin Commission, California State University Monterey Bay, University of California San Diego - Scripps Institution of Oceanography, University of California Los Angeles, San Diego State University, FISHBIO, Monterey Bay Seaweeds, University of Southern California, Stanford University, University of California Irvine", "University of California Santa Cruz - Mark Carr, Carrie Pomeroy" = "University of California Santa Cruz: Mark Carr, University of California Santa Cruz: Carrie Pomeroy", "CDFW; Reef Check CA; G2KR; NMFS Seattle" = "California Department of Fish and Wildlife, Reef Check, Giant Giant Kelp Restoration Project, National Oceanic and Atmospheric Administration - National Marine Fisheries Service Northwest Regional Office", "Ocean Protection Council; Greater Farallones Assoc." = "California Ocean Protection Council, Greater Farallones Assocication", "USGS; MLML; Kashia Band of Pomo Indians;" = "United States Geological Survey, Moss Landing Marine Laboratories, Kashia Band of Pomo Indians", "Garden Club of America; Edges of Earth; Middlebury Institute of IS" = "Garden Club of America, Edges of Earth, Middlebury Institute of International Studies at Monterey", "Tribes - Trinidad Rancheria, Yurok, Tolowa Dee'ni, Wiyot" = "Trinidad Rancheria, Yurok Tribe, Tolowa Dee-ni' Nation, Wiyot Tribe", "Cal. Fish & Game Derek Stein/ Sonke Mastrup/ Kristen Elsmore" = "California Department of Fish and Wildlife: Derek Stein, California Department of Fish and Wildlife: Snoke Mastrup, California Department of Fish and Wildlife: Kristen Elsmore", "CA DFW Rebecca Flores Miller, Kristen Ellsmore" = "California Department of Fish and Wildlife: Kristen Elsmore, California Department of Fish and Wildlife: Rebecca Flores Miller", "California Department of Fish and Wildlife - Kristen Elsmore and Shelby Kawana" = "California Department of Fish and Wildlife: Kristen Elsmore, California Department of Fish and Wildlife: Shelby Kawana", "Marianna lueschel and Josie iselin" = "Above/Below: Marianna Leuschel, Josie Iselin", "Sarah McCormick and Anna neumann" = "City of Fort Bragg: Sarah McCormick, Noyo Harbor District: Anna Neumann", "Ocean Science Trust-Carrie Pomeroy" = "California Ocean Science Trust, University of California Santa Cruz: Carrie Pomeroy", "UCSC (mark carr and PISCO)" = "University of California Santa Cruz: Mark Carr, Partnership for Interdisciplinary Studies of Coastal Oceans"))

kelp.edge <- kelp.edge %>% 
  mutate(alter = strsplit(as.character(alter), ", ")) %>% 
  unnest(alter)

kelp.edge$num_id <- c(1:1013)
kelp.edge <- kelp.edge %>% relocate(num_id)

kelp.edge <- kelp.edge[kelp.edge$num_id != "408", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "290", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "295", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "291", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "293", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "292", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "294", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "453", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "733", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "618", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "879", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "245", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "239", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "242", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "237", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "240", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "241", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "243", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "238", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "244", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "419", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "595", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "366", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "365", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "138", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "702", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "700", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "703", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "699", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "701", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "608", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "537", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "538", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "532", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "836", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "427", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "49", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "54", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "48", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "50", ]
kelp.edge <- kelp.edge[kelp.edge$num_id != "907", ]

# Confused  ---------------------------------------------------
# CCCoP, SAFE, Selkie Land + Sea
# Doug Bush noted in both Ocean Rainforest and The Cultured Abalone Farm, similar w/ Jon Holcomb, grant downie 

# 
