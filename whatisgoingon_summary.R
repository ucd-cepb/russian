library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(paletteer)

dat_survey <- read_csv("dat_survey.csv")[-c(1,2),]

# Organization renaming hell -------

dat_survey <- dat_survey %>%
  unite(col = "org_name", c(`Q3 Individual_1`, `Q3 Several_1`), sep = "", na.rm = TRUE, remove = FALSE)

dat_survey <- dat_survey %>%
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
    ResponseId == "R_1A45wn3R1qfgQo1" ~ "Individual 1",
    ResponseId == "R_1DIA6BHO7pKYU8x" ~ "Individual 2",
    TRUE ~ org_name))

# - Org name at a different level
# : Specific individual named

# AltaSeads Conservancy
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Sergey Nuzhdin  USC/ Alta Seads spore bank" = "AltaSeads Conservancy: Sergey Nuzhdin"))

# Kelp Forest Alliance
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "The Kelp Forest Alliance (Eger)" = "Kelp Forest Alliance: Aaron Eger", "Aaron eger and many many others" = "Kelp Forest Alliance: Aaron Eger", "Kelp forest alliance" = "Kelp Forest Alliance"))

# Above/Below
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "New Agency / Above Below" = "Above/Below", "New Agency" = "Above/Below"))

# Sea Forest
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Alex Berger" = "Sea Forest: Alex Berger"))

# University of California Santa Cruz
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Ali Boutros (UCSC Coastal Science and Policy graduate program))" = "University of California Santa Cruz: Ali Boutros", "Andrea Paz Lacavex (UCSC)" = "University of California Santa Cruz: Andrea Paz Lacavex", "Calvin Munson (UCSC)" = "University of California Santa Cruz: Calvin Munson", "Carr Lab UCSC" = "University of California Santa Cruz - Raimondi-Carr Lab", "Carrie Pomeroy (UCSC)" = "University of California Santa Cruz: Carrie Pomeroy", "Dan Malone- UCSC" = "University of California Santa Cruz: Dan Malone", "Dr Pete raimondi" = "University of California Santa Cruz: Peter Raimondi", "Kristy Kroeker (UCSC)" = "University of California Santa Cruz: Kristy Kroeker", "Mark Carr- UCSC" = "University of California Santa Cruz: Mark Carr", "Mark Carr, UCSC" = "University of California Santa Cruz: Mark Carr", "Pete Raimondi (UCSC)" = "University of California Santa Cruz: Pete Raimondi", "UC Santa Cruz" = "University of California Santa Cruz", "UC Santa Cruz (Carr et al)" = "University of California Santa Cruz: Mark Carr", "UC Santa Cruz Long Marine Lab" = "Joseph M. Long Marine Laboratory", "UCSC" = "University of California Santa Cruz", "UCSC (Carr)" = "University of California Santa Cruz: Mark Carr", "University of California: Santa Cruz" = "University of California Santa Cruz", "US Santa Cruz" = "University of California Santa Cruz"))

# Amah Mutsun Land Trust
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Amah Mutsun" = "Amah Mutsun Land Trust"))

# Moss Landing Marine Laboratories
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Andrew kim" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim - MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim- MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Andrew Kim, MLML" = "Moss Landing Marine Laboratories: Andrew Kim", "Moss Landing Marine Labs Aquaculture Center" = "Moss Landing Marine Laboratories - Aquaculture Center", "Moss Landing marine labs" = "Moss Landing Marine Laboratories", "Moss landing marine labs" = "Moss Landing Marine Laboratories", "Moss Landing Marine Laboratory" = "Moss Landing Marine Laboratories", "Scott Hamilton, MLML Moss Landing Marine Laboratories" = "Moss Landing Marine Laboratories: Scott Hamilton", "Bennett Bugbee - MLML" = "Moss Landing Marine Laboratories: Bennett Bugbee", "Scott Hamilton (Moss Landing Marine Labs)" = "Moss Landing Marine Laboratories: Scott Hamilton", "Mike Graham scientist Moss Landing" = "Moss Landing Marine Laboratories: Mike Graham", "Moss Landing Marine Lab" = "Moss Landing Marine Laboratories", "SJSU Moss Landing Marine Labs - Scott Hamilton" = "Moss Landing Marine Laboratories: Scott Hamilton", "Moss landing" = "Moss Landing Marine Laboratories", "Scott Hamilton- MLML" = "Moss Landing Marine Laboratories: Scott Hamilton", "Bennet Bugbee- MLML" = "Moss Landing Marine Laboratories: Bennett Bugbee", "Bennett Bugbee, MLML" = "Moss Landing Marine Laboratories: Bennett Bugbee","Ashley Kidd" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory: Ashley Kidd", "https://www.sunflowerstarlab.org/" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Mike Graham, Moss Landing Marine Labs" = "Moss Landing Marine Labs: Mike Graham", "Scott hamilton" = "Moss Landing Marine Laboratories: Scott Hamilton", "SJSU Moss Landing Marine Labs - Scott Hamiltom" = "Moss Landing Marine Laboratories: Scott Hamilton", "Sunflower Sea Star Laboratory" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Sunflower Star Lab" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Vince Christian, Sunflower Star Laboratory, Moss Landing" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory: Vince Christian", "Sunflower Star Lab, Moss Landing" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory", "Sunflower Star Laboratory" = "Moss Landing Marine Laboratories - Sunflower Star Laboratory")) 

# Florida State University
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Andrew Rassweiler, Florida State University" = "Florida State University: Andrew Rassweiler", "FSU" = "Florida State University")) 

# Greater Farallones National Marine Sanctuary
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "GFNMS" = "Greater Farallones National Marine Sanctuary", "GFNMS (Holman)" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "GFNMS Advisory Council" = "Greater Farallones National Marine Sanctuary - Greater Farallones National Marine Sanctuary Advisory Council", "Greater farallones" = "Greater Farallones National Marine Sanctuary", "Greater Farallones Sanctuary Advisory Council" = "Greater Farallones National Marine Sanctuary - Greater Farallones National Marine Sanctuary Advisory Council", "Greater Farrallones" = "Greater Farallones National Marine Sanctuary", "Greater Farralones" = "Greater Farallones National Marine Sanctuary", "Gulf of the Farallons National Marine Santuary, Angela Zepp" = "Greater Farallons National Marine Santuary: Angela Zepp", "Gulf of the Farralons National Marine Sanctuary" = "Greater Farallones National Marine Sanctuary", "NOAA Greater Farallones National Marine Sanctuary" = "Greater Farallones National Marine Sanctuary", "Rietta Holman - GFA" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Holman" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Holman - Greater Farallon Association" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Hoffman- GFA" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Hohman" = "Greater Farallones National Marine Sanctuary: Rietta Hohman", "Rietta Hohman - Greater Farallon Association" = "Greater Farallones National Marine Sanctuary: Rietta Hohman"))

# Greater Farallones Association 
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "The Greater Farrallones Association" = "Greater Farallones Association", "The Greater Farallones Kelp Restoration Project (GFA)" = "Greater Farallones Association", "Angela Zepp- GFA" = "Greater Farallones Association: Angela Zepp", "Gina Contolini- GFA" = "Greater Farallones Association: Gina Contolini", "Greater Farallon Association-Rietta Hohman" = "Greater Farallon Association: Rietta Hohman", "Greater Farallones Association kelp team (Rietta Hohman)" = "Greater Farallon Association: Rietta Hohman", "Greater Farallones Association, Rietta Hohman" = "Greater Farallones Association: Rietta Hohman", "Greater Farrallons association" = "Greater Farallones Association", "Gulf of the Farallons Association" = "Greater Farallones Association", "Julieta Gomez- GFA" = "Greater Farallones Association: Julieta Gomez", "Kelp Restoration Team, Greater Farallones Association" = "Greater Farallones Association", "Gulf of the Farallones Association" = "Greater Farallones Association", "Tyler Mears- GFA" = "Greater Farallones Association: Tyler Mears"))

# University of California Santa Barbara
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Anita Giraldo Ospina (UCSB)" = "University of California Santa Barbara: Anita Giraldo Ospina", "Anita Giraldo-Ospino, UCSB" = "University of California Santa Barbara: Anita Giraldo Ospina", "Univ CA Santa Barbara" = "University of California Santa Barbara", "University of California, Santa Barbara" = "University of California Santa Barbara", "UCSB" = "University of California Santa Barbara", "Capt. Merit McCrea" = "University of California Santa Barbara: Merit McCrea", "Colleagues at UCSB and in New Zealand" = "University of California Santa Barbara", "Dan Reed (UCSB)" = "University of California Santa Barbara: Dan Reed", "Dr. Steve Schroder" = "University of California Santa Barbara: Stephen Schroeter", "Jennifer Caselle (UCSB)" = "University of California Santa Barbara: Jennifer Caselle", "Steve Schroeter, UCSB" = "University of California Santa Barbara: Steve Schroeter", "UC Santa Barbara" = "University of California Santa Barbara"))

# Reef Check
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Annie Bauer-Civiello" = "Reef Check: Annie Bauer-Civiello", "Annie Bauer-Civiello, Reef Check California" = "Reef Check: Annie Bauer-Civiello", "Annie Bauer-Civiello, ReefCheck" = "Reef Check: Annie Bauer-Civiello", "Reef" = "Reef Check", "reef check" = "Reef Check", "Reef check" = "Reef Check", "Reef Check California" = "Reef Check", "Reef Check Central California" = "Reef Check", "Reef Check Foundation" = "Reef Check", "Reef Check Ian Norton" = "Reef Check: Ian Norton", "Reef Check in Sonoma & Mendocino as volunteer" = "Reef Check", "Reef Check Kelp Forest Program" = "Reef Check", "Reef Check KFM" = "Reef Check", "Reef Check Morgan Murphy Cannella" = "Reef Check: Morgan Murphy-Cannella", "Dan Abbot, Reef Check" = "Reef Check: Dan Abbott", "Dan Abbott" = "Reef Check: Dan Abbott", "Jan freiwald" = "Reef Check: Jan Freiwald", "Jan Freiwald" = "Reef Check: Jan Freiwald", "Morgan Murphy-Canella Reefcheck" = "Reef Check: Morgan Murphy-Cannella", "RCCA Ian Norton- northern ca. coordinator" = "Reef Check: Ian Norton", "RCCA Morgan Murphy Canola- northrn CA coordinator" = "Reef Check: Morgan Murphy-Cannella", "Reef Check of California" = "Reef Check", "Reef Check So Cal region" = "Reef Check", "Reef Check Southern California" =  "Reef Check", "Reef check staff in Sonoma, Mendocino, and Monterey" = "Reef Check", "Reef Check, Dr. Jan Friewald" = "Reef Check: Jan Freiwald", "Reef Check, Jan Friewald" = "Reef Check: Jan Freiwald", "Reef Check, Morgan Murphy-Cannella" = "Reef Check: Morgan Murphy-Cannella", "Reefcheck" = "Reef Check", "ReefCheck" = "Reef Check", "Reefcheck CA"= "Reef Check", "ReefCheck CA" = "Reef Check", "ReefCheck; Jan Friewald" = "Reef Check: Jan Freiwald", "reef check ca." = "Reef Check", "Reef check so cal" = "Reef Check", "Reef Check, Jan Freiwald" = "Reef Check: Jan Freiwald"))

# California Department of Fish and Wildlife
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Anthony Shiao, California Department of Fish and Wildlife" = "California Department of Fish and Wildlife: Anthony Shiao", "CDFW - Not sure region, dove off the R/V Garibaldi with Reef Check" = "California Department of Fish and Wildlife", "CA DFW" = "California Department of Fish and Wildlife", "CDFW" = "California Department of Fish and Wildlife", "Calif. Dept. of Fish and Wildlife" = "California Department of Fish and Wildlife", "California Department of Fish and Wildlife Marine Region" = "California Department of Fish and Wildlife - Marine Region", "California Department of Fisheries and Wildlife" = "California Department of Fish and Wildlife", "California Fish and Wildlife" = "California Department of Fish and Wildlife", "CDFW Marine Region" = "California Department of Fish and Wildlife - Marine Region", "CDFW out of San Pedro" = "California Department of Fish and Wildlife", "CDFW Region 7" = "California Department of Fish and Wildlife - Marine Region", "CDFW, Laura Rogers-Bennett" = "California Department of Fish and Wildlife: Laura Rogers-Bennett", "Dr. Laura Rogers-Bennett" = "California Department of Fish and Wildlife: Laura Rogers-Bennett", "Fish and Wildlife" = "California Department of Fish and Wildlife", "Ian Kelmartin, CDFW" = "California Department of Fish and Wildlife: Ian Kelmartin", "Kirsten Ramey, CDFW" = "California Department of Fish and Wildlife: Kirsten Ramey", "Kristen Elsmore" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore  DFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore - CDFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore (CDFW--kelp)" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore (CDFW)" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristen Elsmore, CDFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Kristin Elsmore, CDFW" = "California Department of Fish and Wildlife: Kristen Elsmore", "Mike Prall, CDFW" = "California Department of Fish and Wildlife: Mike Prall", "The California Department of Fish and Wildlife" = "California Department of Fish and Wildlife", "CALIFORNIA DEPARTMENT OF FISH AND WILDLIFE" = "California Department of Fish and Wildlife"))

# Montrose Settlements Restoration Program
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "NOAA Montrose Settlements Restoration Program" = "Montrose Settlements Restoration Program"))

# Anthropocene Institute
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Anthropogene Institute" = "Anthropocene Institute"))

# Monterey Bay Aquarium
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "April Ridlon (Monterey Bay Aquarium)" = "Monterey Bay Aquarium: April Ridlon", "Josh Smith (Monterey Bay Aquarium)" = "Monterey Bay Aquarium: Josh Smith", "Josh Smith, Monterey Bay Aquarium" = "Monterey Bay Aquarium: Josh Smith", "MB Aquarium" = "Monterey Bay Aquarium", "Monterey Bay Aquarium - Joshua Smith" = "Monterey Bay Aquarium: Josh Smith", "The Monterey Bay Aquarium" = "Monterey Bay Aquarium"))

# Monterey Bay National Marine Sanctuary
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "MBNMS" = "Monterey Bay National Marine Sanctuary", "MBNMS (Lonhart)" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "NOAA MBNMS" = "Monterey Bay National Marine Sanctuary", "NOAA MBNMS Sanctuary Advisory Council (I spoke recently at a meeting about kelp)" = "Monterey Bay National Marine Sanctuary - Monterey Bay National Marine Sanctuary Advisory Council", "Steve Lonhart" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "Steve Lonhart - Monterey Bay National Marine Sanctuary" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "Steve Lonhart (MBNMS)" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "Steve Lonhart, MBNMS" = "Monterey Bay National Marine Sanctuary: Steve Lonhart", "NOAA Monterey Bay National Marine Sanctuary" = "Monterey Bay National Marine Sanctuary", "Monterey Bay National Marine Sanctuary - Steve Lonhart" = "Monterey Bay National Marine Sanctuary: Steve Lonhart"))

# Monterey Bay Mermaid
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Monterey Bay Mermaid # Alison Smith" = "Monterey Bay Mermaid: Alison Smith"))

# Monterey Abalone Company
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Art Seavey, Monterey Abalone Company" = "Monterey Abalone Company: Art Seavey"))

# Monterey County Weekly
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "MC Weekly" = "Monterey County Weekly"))

# National Oceanic and Atmospheric Administration
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Bay Net" = "National Oceanic and Atmospheric Administration - Bay Net", "CA coastal national marine sanctuaries" = "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries - West Coast Region", "Cameron Spier, SWFSC" = "National Oceanic and Atmospheric Administration - Southwest Fisheries Science Center: Cameron Spier", "NOAA" = "National Oceanic and Atmospheric Administration", "NOAA NCCOS" = "National Oceanic and Atmospheric Administration - National Centers for Coastal Ocean Science", "Office of National Marine Sanctuaries - Multiple" = "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries", "Zach Gold, National Oceanic and Atmospheric Administration" = "National Oceanic and Atmospheric Administration: Zach Gold", "NOAA Office of National Marine Sanctuaries" = "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries"))

# California State University Long Beach
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Bengt Allen, CSU Long Beach" = "California State University Long Beach: Bengt Allen", "CSU Long Beach" = "California State University Long Beach", "CSU Long Beach" = "California State University Long Beach", "CSULB" = "California State University Long Beach"))

# The Nature Conservancy
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Benjaman.grimes@tnc" = "The Nature Conservancy: Ben Grime", "Benjamin Grime, TNC" = "The Nature Conservancy: Ben Grime", "everyone on the Eastern Pacific Kelp Restoration forum via The Nature Conservancy (BC to Baja)" = "The Nature Conservancy - Eastern Pacific Kelp Restoration Forum", "Jono Wilson, The Nature Conservancy" = "The Nature Conservancy: Jono Wilson", "Nature conservancy" = "The Nature Conservancy", "Nature Conservancy" = "The Nature Conservancy", "Norah Eddy, TNC CA Oceans Program" = "The Nature Conservancy: Norah Eddy", "The Nature Conservancey CA" = "The Nature Conservancy", "The nature conservancy" = "The Nature Conservancy", "The Nature Conservancy Australia and Tas" = "The Nature Conservancy", "The Nature Conservancy of California" = "The Nature Conservancy", "The Nature Conservancy, Tristin McHugh" = "The Nature Conservancy: Tristin McHugh", "The Nature Conservency- Tristen McHugh" = "The Nature Conservancy: Tristin McHugh", "TNC" = "The Nature Conservancy", "TNC - Tristin McHugh" = "The Nature Conservancy: Tristin McHugh", "TNC Australia" = "The Nature Conservancy", "TNC California" = "The Nature Conservancy", "TNC New Zealand" = "The Nature Conservancy", "Tom Dempsey, TNC" = "The Nature Conservancy: Tom Dempsey", "Tristan McHugh, The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh - The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh (The Nature Conservancy)" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh with TNC" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh- TNC" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, Kelp Forest Alliance" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, TNC" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, TNC CA Oceans Program / Reef Check" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh - Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh (Nature Conservancy)" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh Nature Conservnacy" = "The Nature Conservancy: Tristin McHugh", "Tristin McHugh, The Nature Conservancy" = "The Nature Conservancy: Tristin McHugh", "The Nature Conservancy; Tristin McHugh" = "The Nature Conservancy: Tristin McHugh"))

# University of California San Diego
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Birch Aquarium" = "University of California San Diego - Scripps Institution of Oceanography - Birch Aquarium", "Ed Parnell, Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography: Ed Parnell", "Ed Parnell, UCSD" = "University of California San Diego - Scripps Institution of Oceanography: Ed Parnell", "Jen Smith at SCRIPS" = "University of California San Diego - Scripps Institution of Oceanography: Jen Smith", "Kristin Riser, Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography: Kristin Riser", "Paul Dayton, Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography: Paul Daytona", "Scripps" = "University of California San Diego - Scripps Institution of Oceanography", "Scripps Instituion of Oceanography; Ed Parnell" = "University of California San Diego - Scripps Institution of Oceanography: Ed Parnell", "Scripps Institute of Oceanography: Masters of Advanced Studies in Marine Biodiversity & Conservation (MAS MBC)" = "University of California San Diego - Scripps Institution of Oceanography", "Scripps Institution of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography", "SIO" = "University of California San Diego - Scripps Institution of Oceanography", "UC San Diego" = "University of California San Diego", "UCSD Scripps Institute of Oceanography - Jim Leichter" = "University of California San Diego - Scripps Institution of Oceanography: Jim Leichter", "Scripps Inst. of Oceanography" = "University of California San Diego - Scripps Institution of Oceanography"))

# San Diego State University
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Dr Mathew Edwards" = "San Diego State University: Matthew Edwards", "Matt Edwards, San Diego State University" = "San Diego State University: Matthew Edwards", "Matthew Edwards - San Diego State University" = "San Diego State University: Matthew Edwards", "Matthew Edwards, San Diego State University" = "San Diego State University: Matthew Edwards", "Shelby Penn SDSU" = "San Diego State University: Shelby Penn", "San Diego state university" = "San Diego State University")) 

# Blue Harmony
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Blue Harmony Foundation" = "Blue Harmony"))

# University of California Davis
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "BML" = "University of California Davis - Bodega Marine Laboratory", "Bodega Marine Labs" = "University of California Davis - Bodega Marine Laboratory", "Bodega Bay Marine Labs" = "University of California Davis - Bodega Marine Laboratory", "DISES" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "DiSES ppl" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Gabby Yang" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Gabby Yang", "Kelp RISES personnel" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Kelp RISES project personnel, students at UCD, UCSC, Cal Poly Humboldt" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Kelp RISES Team, UC Davis" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "Marissa Baskett, UCD" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Marissa Baskett (UCD)" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Marissa Baskett - Kelp RISES" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Marissa Baskett, uc davis" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "Michael Springborn (UCD)" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Mike springborn" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Mike springborn, uc davis" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Mike Springborn, UCD" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborn", "Rachael Bay, UC Davis" = "University of California Davis: Rachael Bay", "The University of California, Davis" = "University of California Davis", "UC Davis" = "University of California Davis", "UC Davis (Baskett et al)" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett", "UC Davis Bodega Marine Lab - John Largier" = "University of California Davis - Bodega Marine Laboratory: John Largier", "UC Davis Bodega Marine Lab Boating and Diving Safety Office" = "University of California Davis - Bodega Marine Laboratory - Boating and Diving Safety Office", "UC Davis bodega marine labs" = "University of California Davis - Bodega Marine Laboratory", "UC Davis Department of Environmental Science and Policy" = "University of California Davis", "UC Davis- BML" = "University of California Davis - Bodega Marine Laboratory", "Uc davis" = "University of California Davis", "UC Davis, DiSES" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "UC Davis, Kelp RISES team" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "University of California Davis Kelp RISES Project" = "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System", "University of California, Davis" = "University of California Davis", "University of California, Davis Coastal & Marine Sciences Institute" = "University of California Davis"))

# Bureau of Ocean Energy Management
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "BOEM" = "Bureau of Ocean Energy Management"))

# Sonoma State University
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Rachael Karm- SSU" = "Sonoma State University: Rachael Karm", "Brent Hughes - Sonoma State Univ." = "Sonoma State University: Brent Hughes", "Brent Hughes (Sonoma State University)" = "Sonoma State University: Brent Hughes", "Sonoma State" = "Sonoma State University", "Hughes Lab, Sonoma State University" = "Sonoma State University - Hughes Lab", "Sonoma State University - Brent Hughes" = "Sonoma State University: Brent Hughes", "Brent Hughes, Sonoma State University" = "Sonoma State University: Brent Hughes", "SSU" = "Sonoma State University", "Brent Hughes (Sonoma State)" = "Sonoma State University: Brent Hughes", "Brent Hughes- SSU" = "Sonoma State University: Brent Hughes", "Brent Hughes, Sonoma State" = "Sonoma State University: Brent Hughes", "Rachel Karm SSU" = "Sonoma State University: Rachael Karm", "Somona State University (Hughes lab)" = "Sonoma State University - Hughes Lab", "Sonoma State University: Rachel Karm" = "Sonoma State University: Rachael Karm"))

# California Coastal Commission
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "CA Coastal Commission" = "California Coastal Commission", "Coastal Commission" = "California Coastal Commission"))

# California State Parks 
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "CA Dept. of Parks & Recreation - Santa Cruz" = "California State Parks", "CA DPR" = "California State Parks", "CA State Parks - Fort Ross" = "California State Park - Fort Ross Conservancy", "California State Parks Interpretation & Education Division" = "California State Parks - Interpretation and Education Division", "California State Parks Natural Resources Division" = "California State Parks - Natural Resources Division", "California State Parks - Interpretation & Education Division" = "California State Parks - Interpretation and Education Division", "State Parks" = "California State Parks", "The Fort Ross Conservancy" = "California State Parks - Fort Ross Conservancy"))

# California Diving News
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "CA DIving News" = "California Diving News"))

# California Academy of Sciences
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Steinhart Aquarium (California Academy of Sciences)" = "California Academy of Sciences"))

# California Fish and Game Commission
dat_survey <-  dat_survey %>%
  mutate_all(~ recode(., "CA Fish and Game Commission" = "California Fish and Game Commission", "CA Fish and Game Commission - Susan Ashcraft" = "California Fish and Game Commission: Susan Ashcraft", "CFGC" = "California Fish and Game Commission", "Fish & Game" = "California Fish and Game Commission"))

# California Ocean Science Trust
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Ocean Science Trust" = "California Ocean Science Trust", "CA Ocean Science Trust" = "California Ocean Science Trust", "Ocean Science Trust staff" = "California Ocean Science Trust"))

# California Ocean Protection Council
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "CA OPC" = "California Ocean Protection Council", "Calif Ocean Protection Council" = "California Ocean Protection Council", "Mike Esgro, Ocean Protection Council" = "California Ocean Protection Council: Mike Esgro", "ocean protection council" = "California Ocean Protection Council", "Ocean Protection Council" = "California Ocean Protection Council", "Ocean Protection Council- Michael Esgro" = "California Ocean Protection Council: Mike Esgro", "Ocean Protection Council; Pike Spector" = "California Ocean Protection Council: Pike Spector", "OPC" = "California Ocean Protection Council", "Pike Spector, OPC" = "California Ocean Protection Council: Pike Spector"))

# California Sea Grant
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "CA Sea Grant" = "California Sea Grant", "CA SeaGrant" = "California Sea Grant", "CA Seagrant (Oh)" = "California Sea Grant: Shauna Oh", "Cal seagrant" = "California Sea Grant", "california sea grant" = "California Sea Grant", "Jami Miller, Sea Grant / City of Fort Bragg" = "California Sea Grant: Jami Miller", "Sea grant" = "California Sea Grant", "Sea Grant" = "California Sea Grant", "Shauna Oh, California SeaGrant" = "California Sea Grant: Shauna Oh"))

# California Sea Urchin Commission
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "California Sea Urchin Commission (CSUC)" = "California Sea Urchin Commission", "California sea urchin commission" = "California Sea Urchin Commission", "CA Sea Urchin Commission" = "California Sea Urchin Commission", "Sea urchin commission" = "California Sea Urchin Commission", "California Sea Urchin Commission, David Goldenberg" = "California Sea Urchin Commission: David Goldenberg", "David Goldenburg" = "California Sea Urchin Commission: David Goldenberg", "David Goldenburg  California sea urchin commission" = "California Sea Urchin Commission: David Goldenberg", "The Urchin Commission" = "California Sea Urchin Commission", "Grant Downey Californina sea urchin commission" = "California Sea Urchin Commission: Grant Downie", "Cal Urchin Commission" = "California Sea Urchin Commission", "California Sea Urchin Commision- David Goldenberg" = "California Sea Urchin Commission: David Goldenberg", "California Sea Urchin Commission - David Goldenberg" = "California Sea Urchin Commission: David Goldenberg", "Grant Downie, The California Sea Urchin Commission" = "California Sea Urchin Commission: Grant Downie", "California Sea Urchin Comission" = "California Sea Urchin Commission", "California Sea Urchin Commission: Grant Downey" = "California Sea Urchin Commission: Grant Downie", "URCHIN COMMISSION" = "California Sea Urchin Commission"))

# California State Lands Commission
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "CA State Lands Commission" = "California State Lands Commission"))

# California State Polytechnic University Humboldt
dat_survey <- dat_survey %>%
  mutate_all(~recode(., "Cal Poly Humboldt - Biology department (Sean Craig)" = "California State Polytechnic University Humboldt: Sean Craig", "Cal Poly Humboldt - Fisheries department/Aquaculture" = "California State Polytechnic University Humboldt", "Cal Poly Humboldt - Oceanography department (Christine Cass)" = "California State Polytechnic University Humboldt: Christine Cass", "Cal Poly Humboldt, Food Sovereignty Lab, Dr. Cutcha Risling Baldy" = "California State Polytechnic University Humboldt - Food Sovereignty Lab: Cutucha Risling Baldy", "Cal Poly Humboldt, Jody Martinez" = "California State Polytechnic University Humboldt: Jody Martinez", "CalPoly Humboldt" = "California State Polytechnic University Humboldt", "Duncan Jackson" = "California State Polytechnic University Humboldt: Duncan Jackson", "Frankie Moitoza (Cal Poly Humboldt)" = "California State Polytechnic University Humboldt: Frankie Moitoza", "HSU" = "California State Polytechnic University Humboldt", "HSU (Craig)" = "California State Polytechnic University Humboldt: Sean Craig", "HSU Rick Alvarez" = "California State Polytechnic University Humboldt: Rick Alvarez", "Humbolt state university" = "California State Polytechnic University Humboldt", "NEREO, Cal Poly Humboldt" = "California State Polytechnic University Humboldt - Northcoast Evaluation of Reef Ecosystems Organization", "Paul Bourdeau" = "California State Polytechnic University Humboldt: Paul Bourdeau", "Rafael Cuevas Uribe" = "California State Polytechnic University Humboldt: Rafael Cuevas Uribe", "Rafael Cuveus-Uribe, Cal Poly Humboldt" = "California State Polytechnic University Humboldt: Rafael Cuevas Uribe", "Sean Craig" = "California State Polytechnic University Humboldt: Sean Craig", "Sean Craig (Cal Poly Humboldt)" = "California State Polytechnic University Humboldt: Sean Craig", "Telonicher Marine Lab" = "California State Polytechnic University Humboldt - Telonicher Marine Lab", "Cal Poly Humboldt" = "California State Polytechnic University Humboldt", "Cal Poly Humboldt-NEREO" = "California State Polytechnic University Humboldt - Northcoast Evaluation of Reef Ecosystems Organization", "NEREO" = "California State Polytechnic University Humboldt - Northcoast Evaluation of Reef Ecosystems Organization"))

# California State Polytechnic University Pomona
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Cal Poly Pomona" = "California State Polytechnic University Pomona"))

# California State University Northridge
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Cal State Northridge" = "California State University Northridge", "Janet Kubler scientist CSUN" = "California State University Northridge: Janet Kubler"))

# Marine Protected Area Collaborative Network
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Calla Allison and the MPA Collaborative Network" = "Marine Protected Area Collaborative Network: Calla Allison", "Monterey MPACN" = "Marine Protected Area Collaborative Network", "MPA CN" = "Marine Protected Area Collaborative Network", "MPA Collaborative Network" = "Marine Protected Area Collaborative Network", "MPA Collaborative Network, Humboldt Co." = "Marine Protected Area Collaborative Network", "MPA Collaboratives" = "Marine Protected Area Collaborative Network", "MPA Long Term Monitoring Project" = "Marine Protected Area Collaborative Network"))

# Giant Giant Kelp Restoration Project
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Caspar Cove kelp restoration project Jon Holcomb" = "Giant Giant Kelp Restoration Project - Caspar Cove Project: Jon Holcomb", "G 2 Kelp Restoration Keith Rootseart" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "G2KR" = "Giant Giant Kelp Restoration Project", "g2kr.com" = "Giant Giant Kelp Restoration Project", "Giant Giant Kelp - Keith Rootsaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Giant giant kelp restoration" = "Giant Giant Kelp Restoration Project", "Giant Giant Kelp Restoration" = "Giant Giant Kelp Restoration Project", "Giant Giant Kelp Restoration - Keith Rootsaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Giant Giant Kelp Restoration Project - Caspar Cove" = "Giant Giant Kelp Restoration Project - Caspar Cove Project", "Giant Giant Kelp Restoration Project (G2KR)" = "Giant Giant Kelp Restoration Project", "giant giant kep restoration" = "Giant Giant Kelp Restoration Project", "Giant Kelp Restoration Project" = "Giant Giant Kelp Restoration Project", "Great Great Kelp Restoration Project" = "Giant Giant Kelp Restoration Project", "Great great kelp restoration project (Monterey)" = "Giant Giant Kelp Restoration Project", "Keith  Rootsart" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rooseart and the Giant Giant Kelp Restoration Project" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rooseart" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rooseart - G2KR" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rootseart, Giant giant kelp restoration project" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rootsaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "Keith Rootsaert - G2KR" = "Giant Giant Kelp Restoration Project: Keith Rootsaert", "The Giant Giant Kelp restoration project" = "Giant Giant Kelp Restoration Project", "Great Great Kelp Restoration" = "Giant Giant Kelp Restoration Project", "https://www.facebook.com/UrchinsKelpOtters" = "Giant Giant Kelp Restoration Project", "Keith Roostaert" = "Giant Giant Kelp Restoration Project: Keith Rootsaert"))

# Watermen's Alliance
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Caspar Cove Project, Waterman's Alliance" = "Watermen's Alliance - Caspar Cove Project", "Josh Russo Waterman's alliance" = "Watermen's Alliance: Josh Russo", "The Waterman's Alliance, Josh Russo" = "Watermen's Alliance: Josh Russo", "watermans alliance" = "Watermen's Alliance", "Watermans Alliance" = "Watermen's Alliance", "Watermans Alliance Joshua Russo" = "Watermen's Alliance: Josh Russo", "Watermen's Alliance" = "Watermen's Alliance", "Watermen's Alliance Urchin Culling events Caspar Cove as urchin culler" = "Watermen's Alliance - Caspar Cove Project", "Watermen's Alliance Urchin Removal - Josh Russo" = "Watermen's Alliance: Josh Russo", "The Watermen's Alliance" = "Watermen's Alliance", "Waterman's Alliance" = "Watermen's Alliance"))

# Catalina Island Marine Institute
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Catalina island marine institute" = "Catalina Island Marine Institute", "Deidre Sullivan  - Catalina Island Marine Institute" = "Catalina Island Marine Institute: Deidre Sullivan"))

# Cordell Bank National Marine Sanctuary
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "CBNMS Advisory Council" = "Cordell Bank National Marine Sanctuary - Cordell Bank National Marine Sanctuary Advisory Council"))

# Channel Islands National Marine Sanctuary
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Channel Islands National Marine Sanctuary - Ryan Freedman" = "Channel Islands National Marine Sanctuary: Ryan Freedman", "Channel Islands National Marine Sanctuary Advisory Council" = "Channel Islands National Marine Sanctuary - Channel Islands National Marine Sanctuary Advisory Council", "Channel Islands National Park" = "Channel Islands National Marine Sanctuary", "CINMS (Freedman)" = "Channel Islands National Marine Sanctuary: Ryan Freedman", "NOAA CINMS" = "Channel Islands National Marine Sanctuary", "Scott Gabara Channel Islands National Park" = "Channel Islands National Marine Sanctuary: Scott Gabara", "Scott Gabara, Channel Islands National Parks" = "Channel Islands National Marine Sanctuary: Scott Gabara", "NOAA Channel Islands National Marine Sanctuary" = "Channel Islands National Marine Sanctuary"))

# Fish Reef Project 
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Chris Goldblatt of Fish Reef Project (fishreef.org)" = "Fish Reef Project: Chris Goldblatt"))

# City of Fort Bragg
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "City of fort bragg" = "City of Fort Bragg", "City of Ft Bragg" = "City of Fort Bragg", "Sarah McCormick, City of Fort Bragg" = "City of Fort Bragg: Sarah McCormick"))

# Redwood Elementary
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Redwood Elementary School, Fort Bragg" = "Redwood Elementary", "Redwood Elementary, Fort Bragg Unified School District" = "Redwood Elementary"))

# Coastal Conservation Association California
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Coastal Conservation Association - California Chapter" = "Coastal Conservation Association California", "Hank Goebel" = "Coastal Conservation Association California: Hank Goebel"))

# The Sea Ranch Association 
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Coastal Stewardship Task Force, The Sea Ranch Association" = "The Sea Ranch Association: Coastal Stewardship Task Force"))

# United States Air Force
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Col. David Lopez, USAF (ret.)" = "United States Air Force: David Lopez"))

# California State University Agricultural Research Institute
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "CSU Agricultural Research Institute" = "California State University Agricultural Research Institute"))

# California State University Monterey Bay
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "CSU Monterey Bay" = "California State University Monterey Bay", "CSUMB" = "California State University Monterey Bay"))

# California State University Chico
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "CSUC" = "California State University Chico"))

# University of California Berkeley
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Dan Okamoto (UCB)" = "University of California Berkeley: Dan Okamoto", "Dan Okamoto- UC Berkeley" = "University of California Berkeley: Dan Okamoto", "Dr. Dan Okamoto" = "University of California Berkeley: Dan Okamoto", "Maya Munstermann, expert scientist" = "University of California Berkeley: Maya Munstermann", "UC Berkeley" = "University of California Berkeley", "UC Berkeley Dan Okamoto" = "University of California Berkeley: Dan Okamoto", "University of Californa" = "University of California Berkeley", "University of California" = "University of California Berkeley"))

# Occidental College
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Dan Pondella (Occidental College)" = "Occidental College - Vantuna Research Group: Dan Pondella", "Occidental VRG (Pondella)" = "Occidental College - Vantuna Research Group: Dan Pondella", "Occidental College: Dan Pondella" = "Occidental College - Vantuna Research Group: Dan Pondella", "Vantuna Research Group, Occidental College" = "Occidental College - Vantuna Research Group"))

# Kashia Band of Pomo Indians
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Dan sweezy" = "Kashia Band of Pomo Indians: Dan Swezey", "Dr. Dan Sweazy" = "Kashia Band of Pomo Indians: Dan Swezey", "KASHIA" = "Kashia Band of Pomo Indians", "Kashia Band of Pomo Indians (Dan Sweezy)" = "Kashia Band of Pomo Indians: Dan Swezey", "Kashia Band of Pomo Indians, Dan Sweezy" = "Kashia Band of Pomo Indians: Dan Swezey"))

# Dandy Fish Company
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Dandy Fish Company (fish processor)" = "Dandy Fish Company"))

# DeeperBlue
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Deeperblue.com" = "DeeperBlue"))

# Ocean Rainforest
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Doug Bush, Ocean Rainforest" = "Ocean Rainforest: Doug Bush", "Ocean Rainforest, Inc." = "Ocean Rainforest"))

# The Cultured Abalone Farm
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Doug Bush, The Cultured Abalone" = "The Cultured Abalone Farm: Doug Bush", "The Cultured Abalone Farm Rick Gutierrez" = "The Cultured Abalone Farm: Rick Gutierrez", "The Cultured Abalone Farm LLC" = "The Cultured Abalone Farm"))

# Noyo Center for Marine Science
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Elizabeth Carpenter & NOYO Center - outreach webinar for Sea Otter Awareness Week 2023" = "Noyo Center for Marine Science: Elizabeth Carpenter", "Noyo Center" = "Noyo Center for Marine Science", "NOYO CENTER" = "Noyo Center for Marine Science", "Noyo Marine Science Center" = "Noyo Center For Marine Science", "Noyo Marine Science Center (worked with directly)" = "Noyo Center For Marine Science", "Noyo Ocean Center" = "Noyo Center For Marine Science", "Noyo Science Center" = "Noyo Center For Marine Science", "Sheila Semans, NOYO Center" = "Noyo Center For Marine Science: Sheila Semans", "Sheila Semans, Noyo Center" = "Noyo Center For Marine Science: Sheila Semans", "Sheila Semans" = "Noyo Center For Marine Science: Sheila Semans"))

# University of Washington
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Friday Harbor Lab" = "University of Washington - Friday Harbor Laboratories", "Friday Harbor Lab, University of Washington (Hodin lab)" = "University of Washington - Friday Harbor Laboratories - Hodin Lab", "University of Washington, Friday Harbor Labs" = "University of Washington - Friday Harbor Laboratories"))

# Washington State Department of Natural Resources
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "WA DNR; Helen Berry" = "Washington State Department of Natural Resources: Helen Berry"))

# Hog Island Oyster Company
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Gary Fleener, Hog Island Oyster Company" = "Hog Island Oyster Company: Gary Fleener"))

# Elkhorn Slough Ecological Reserve
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "ESNERR" = "Elkhorn Slough Ecological Reserve"))

# Get Inspired
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Get Inspired, Nancy Caruso" = "Get Inspired: Nancy Caruso", "My own kelp monitoring program (Get Inspired ), Nancy Caruso" = "Get Inspired"))

# Girl Scouts of America
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Girl Scouts of America - Cheryl Kingman" = "Girl Scouts of America: Cheryl Kingman"))

# Gwaii Haanas National Park Reserve
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "GWAII HAANAS" = "Gwaii Haanas National Park Reserve"))

# Council of the Haida Nation
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "HAIDA FISHERIES" = "Council of the Haida Nation -  Haida Fisheries Program"))

# California Marine Sanctuary Foundation
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Hallie Brown, CA Marine Sanctuary Foundation" = "California Marine Sanctuary Foundation: Hallie Brown"))

# The Bay Foundation
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Heather Burdick Bay Foundation" = "The Bay Foundation: Heather Burdick"))

# Surfrider Foundation
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Humboldt Surfrider" = "Surfrider Foundation - Surfrider Humboldt"))

# InterTribal Sinkyone Wilderness Council
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Inter Tribal Sinkyone Wilderness Council" = "InterTribal Sinkyone Wilderness Council"))

# University of Massachusetts Boston
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Jarrett Byrnes, University of Massachusetts Boston" = "University of Massachusetts Boston: Jarrett Byrnes"))

# University of Nevada Reno
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Jeremy McFarland, University of Nevada, Reno" = "University of Nevada Reno: Jeremy McFarland"))

# Trinidad Rancheria
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Jessica Gravelle, Trinidad Rancheria" = "Trinidad Rancheria: Jessica Gravelle", "The Cher-Ae Heights Indian Community of the Trinidad Rancheria" = "Trinidad Rancheria"))

# Marine Conservation Institute
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Jorge Arroyo-Esquival & Marine Conservation Institute - outreach webinar for Sea Otter Awareness Week 2023" = "Marine Conservation Institute: Jorge Arroyo Esquival"))

# Kelp Restoration and Management Plan Scientific Advisory Team
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Kelp restoration management plan - CA" = "Kelp Restoration and Management Plan Scientific Advisory Team", "Kelp Restoration Management Plan Scientific Advisory Team" = "Kelp Restoration and Management Plan Scientific Advisory Team"))

# Kelp Restoration and Management Plan Community Working Group
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "KRMP" = "Kelp Restoration and Management Plan Community Working Group", "KRMP (CDFW)" = "Kelp Restoration and Management Plan Community Working Group", "California Kelp Restoration and Management Community Group" = "Kelp Restoration and Management Plan Community Working Group", "Members of the KRMP stakeholder group" = "Kelp Restoration and Management Plan Community Working Group"))

# LAWaterkeeper
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "LA Waterkeeper - Michael Quill" = "LAWaterkeeper: Michael Quill"))

# University of California Merced
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Lauren Schiebelhut" = "University of California Merced: Lauren Schiebelhut", "Michael Dawson" = "University of California Merced: Michael Dawson", "Mike Dawson, UC Merced" = "University of California Merced: Michael Dawson", "UC Merced" = "University of California Merced", "UC Merced (Dawson lab)" = "University of California Merced - Dawson Lab"))

# Sepia Lux 
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Loren Crotty (private fundraising event)" = "Sepia Lux: Loren Crotty"))

# National Fish and Wildlife Foundation
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "National Fish and Wildlife Foundation - Multiple" = "National Fish and Wildlife Foundation"))

# National Center for Ecological Analysis and Synthesis
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "NCEAS kelp working group (lead is Eger)" = "National Center for Ecological Analysis and Synthesis"))

# National Marine Sanctuary Foundation
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "NMSF" = "National Marine Sanctuary Foundation", "NOAA NMSF" = "National Marine Sanctuary Foundation", "NOAA NMS" = "National Marine Sanctuary Foundation", "NOAA Sanctuaries" = "National Marine Sanctuary Foundation", "NOAA NMFS" = "National Marine Sanctuary Foundation"))

# Noozhawk
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "noozhawk.com (online news service)" = "Noozark"))

# Oregon Department of Fish and Wildlife
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "OR DFW; Scott Marion" = "Oregon Department of Fish and Wildlife: Scott Marion", "Oregon Department of Fisheries and Wildlife" = "Oregon Department of Fish and Wildlife"))

# Oregon Kelp Alliance
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Oregon kelp alliance" = "Oregon Kelp Alliance", "Oregon Kelp Alliance: Thomas Calvanes" = "Oregon Kelp Alliance: Tom Calvanese", "Oregon Kelp Forest Alliance" = "Oregon Kelp Alliance", "ORKA, Tom Calvanese" = "Oregon Kelp Alliance: Tom Calvanese", "Oregon Kelp Alliance; Thomas Calvanese" = "Oregon Kelp Alliance: Tom Calvanese"))

# San Jose State University
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "others at SJSU" = "San Jose State University"))

# Port of San Diego
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Paula Sylvia at the Port of San Diego" = "Port of San Diego: Paula Sylvia"))

# Port of Los Angeles
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Port of LA" = "Port of Los Angeles"))

# Puget Sound Restoration Fund
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Puget sound restoration fund" = "Puget Sound Restoration Fund", "Puget Sound Restoration Fund in seattle washington" = "Puget Sound Restoration Fund"))

# Partnership for Interdisciplinary Studies of Coastal Oceans
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "PISCO" = "Partnership for Interdisciplinary Studies of Coastal Oceans"))

# Pycnopodia Recovery Working Group
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Pycnopodia Restoration Group" = "Pycnopodia Recovery Working Group"))

# GreenWave
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Green Wave" = "GreenWave"))

# Reef Environmental Education Foundation
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "REEF,  Janna Nichols" = "Reef Environmental Education Foundation: Janna Nichols"))

# United States Congress
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Rep. Huffman (district 2)" = "United States Congress: Jared Huffman"))

# United States Congress
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Rep. Huffman (district 2)" = "United States Congress: Jared Huffman"))

# Universidad Autonoma de Baja California Mexico
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Rodrigo Beas-Lunas (ABC Ensenada)" = "Universidad Autonoma de Baja California Mexico: Rodrigo Beas-Lunas"))

# Tolowa Dee-ni' Nation
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Rosa Laucci, Tolowa Dee-ni' Nation" = "Tolowa Dee-ni' Nation: Rosa Laucci", "Tolowa Dee ni' Nation" = "Tolowa Dee-ni' Nation", "Tolowa Dee-Ni' Nation" = "Tolowa Dee-ni' Nation"))

# Salesforce
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "SalesForce" = "Salesforce"))

# San Diego Association of Governments
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "SANDAG" = "San Diego Association of Governments"))

# Save Our Shores
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Save our Shores" = "Save Our Shores"))

# Southern California Coastal Ocean Observing System
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "SCCOOS" = "Southern California Coastal Ocean Observing System"))

# Southern California Coastal Water Research Project
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "SCCWRP" = "Southern California Coastal Water Research Project"))

# Sherwood Valley Band of Pomo Indians
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Sherwood Valley Band of Pomo" = "Sherwood Valley Band of Pomo Indians"))

# Stanford University
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Stanford" = "Stanford University", "Stanford University - Steven Monismith" = "Stanford University: Steven Monismith"))

# California State Lands Commission
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "State Lands" = "California State Lands Commission"))

# Strategic Earth Consulting
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Strategic Earth" = "Strategic Earth Consulting", "Strategic Earth Inc." = "Strategic Earth Consulting"))

# Sunken Seaweed
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Sunken Seaweed LLC, Torre Polizzi" = "Sunken Seaweed: Torre Polizzi", "Torre Polizzi, Sunken Seaweed" = "Sunken Seaweed: Torre Polizzi", "Torre pollozi" = "Sunken Seaweed: Torre Polizzi"))

# The Jetlagged
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "The Jet Lagged (photographers)" = "The Jetlagged"))

# University of Southern California
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "U Southern California" = "University of Southern California"))

# United States Navy
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "U.S. Navy" = "United States Navy", "US Navy" = "United States Navy"))

# United States Geological Survey
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "USGS" = "United States Geological Survey", "U.S. Geological Survey" = "United States Geological Survey"))

# University of California Los Angeles
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "UCLA" = "University of California Los Angeles", "UCLA - Kyle Cavanaugh" = "University of California Los Angeles: Kyle Cavanaugh", "UCLA; Kyle Cavanuagh" = "University of California Los Angeles: Kyle Cavanaugh"))

# University of Miami 
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "University of Miami Rosenstiel School of Marine, Atmospheric, and Earth Science - Claire Paris" = "University of Miami: Claire Paris"))

# University of Oregon
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "UO OIMB Biology department - Aaron Galloway" = "University of Oregon: Aaron Galloway"))

# Pacific Urchin Harvesters Association
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "URCHIN HARVESTORS ASSOCIATION" = "Pacific Urchin Harvesters Association"))

# United States Fish and Wildlife Service
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "US Fish and Wildlife Service" = "United States Fish and Wildlife Service", "USFWS" = "United States Fish and Wildlife Service", "US Fish and Wildlife" = "United States Fish and Wildlife Service"))

# Woods Hole Oceanographic Institution
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "WHOI; Tom Bell" = "Woods Hole Oceanographic Institution: Tom Bell", "WHOI (Bell)" = "Woods Hole Oceanographic Institution: Tom Bell", "WHOI - Tom Bell" = "Woods Hole Oceanographic Institution: Tom Bell"))

# Earth Equity
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Earth equty" = "Earth Equity"))

# WSP
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "WSP (env consulting)" = "WSP"))

# West Coast Ocean Alliance
dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "West Coast Ocean alliance" = "West Coast Ocean Alliance"))

# individuals

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Byron Kohler (urchin diver" = "Urchin Diver 1", "Gary Trumper- urchin diver" = "Urchin Diver 2", "Mickey Kitahara- urchin diver" = "Urchin Diver 3", "Urchin removal diver" = "Urchin Diver 4"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Many Kelp Restoration Divers" = "Divers"))
# make this diver # 6 or whatever but make a footnote about it 

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "commercial fishermen in San Diego" = "Fishers")) 

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Urchin removal" = "Divers"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Dale Glanz" = "Individual 3"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Caspar Cove kelp restoration project Jon Holcomb" = "Fisher 1"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Erik Owen (commercial fisher)" = "Fisher 2"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Ernest (Tad) Thompson (commercial fisher)" = "Fisher 3"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Jeffrey Gritsch (commercial fisher)" = "Fisher 4"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Mark Gmeiner (commercial fisher)" = "Fisher 5", " Commerical Fisher: Mark Gmeiner" = "Fisher 5"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Grant Downey" = "Fisher 6", "Grant downie" = "Fisher 6", "Grant Downie (commercial fisher)" = "Fisher 6", "Grant Downie (Commercial urchin diver, mendocino)" = "Fisher 6", "Grant Downie- urchin diver" = "Fisher 6"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Pat Downie (commercial fisher)" = "Fisher 7"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Jeremy George Petty (commercial fisher)" = "Fisher 8"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Josie Iselin (kelp artist and educator)" = "Individual 4", "Josie Iselin, Author/Artist" = "Josie Iselin"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Kevin Quider (commercial fisher)" = "Fisher 9"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Patrick Webster, science communicator and underwater photographer" = "Photographer 1"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Roger Carlson, Roger Carlson Photography" = "Photographer 2"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Ron McPeak" = "Photographer 3"))

dat_survey <- dat_survey %>%
  mutate_all(~ recode(., "Shaun Wolfe, Shaun Wolfe Photography" = "Photographer 4"))

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

# Question 1 -----------

question_1 <- dat_survey %>% 
  select(Q1)

question_1 <- question_1 %>%
  filter(!is.na(Q1))

question_1 <- question_1[question_1$Q1 != "I have no involvement in kelp forest-related issues", ]


ggplot(data = question_1, aes(x = Q1, fill = Q1)) +
  geom_bar() +
  scale_x_discrete(labels = c('Occassional', 'Routine', 'Primary')) +
  paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby") +
  theme_minimal(base_size = 13) +
  #geom_text(stat = 'count', 
            #aes(label = ..count..),
            #position = position_dodge(width = 1),
            #size = 4,
            #vjust = -0.5) +
  ggtitle("Question 1: Level of involvement in kelp forest-related issues") +
  xlab("") +
  ylab("") +
  guides(fill=guide_legend(title=" "))


# Question 2 ------------

question_2 <- dat_survey %>% 
  select(Q2)

question_2 <- question_2 %>%
  filter(!is.na(Q2))

ggplot(data = question_2, aes(x = Q2, fill = Q2)) +
  geom_bar() +
  scale_x_discrete(labels = c("Own", "Single", "Multiple")) +
  paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby") +
  theme_minimal(base_size = 13) + 
  #geom_text(stat = 'count', 
            #aes(label = ..count..),
            #position = position_dodge(width = 1),
            #size = 4,
            #vjust = -0.5) +
  ggtitle("Question 2: Involvement to who?") +
  xlab("") +
  ylab("") +
  guides(fill=guide_legend(title=""))

# Question 3 (in progress) ----------
question_3 <- dat_survey %>% 
  select(org_name)

question_3 <- subset(question_3, question_3$org_name != "")



# Question 4 (pie chart?) --------
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

question_4 <- dat_survey %>%
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

question_4 %>%
  mutate(response = factor(choice, c('Research', 'Environmental management', 'Advocacy or outreach', 'Harvesting or fishing or growing', 'Recreation or tourism', 'Planning or permitting', "People management", "Business operations", "Student"))) %>%
  ggplot(., aes(x = fct_rev(response), fill = response)) +
  geom_bar() +
  theme_minimal(base_size = 13) + 
  #geom_text(stat = 'count', 
            #aes(label = ..count..),
            #position = position_dodge(width = 1),
            #size = 4.5,
            #hjust = 1.2) +
  ggtitle("Question 4: Type of involvement") +
  paletteer::scale_fill_paletteer_d("RColorBrewer::RdYlBu") +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(legend.position="none")

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
  ggplot(., aes(x = fct_rev(county), fill = region)) +
  geom_bar() +
  theme_minimal(base_size = 13) + 
  #geom_text(stat = 'count', 
            #aes(label = ..count..),
            #position = position_dodge(width = 1),
            #size = 4.5,
            #hjust = 1.2) +
  ggtitle("Question 5: County affiliation") +
  coord_flip() +
  paletteer::scale_fill_paletteer_d("ggthemes::excel_Median") +
  xlab("") +
  ylab("") +
  #theme(legend.position="none")
  guides(fill=guide_legend(title="Region"))

# binned by region
question_5 %>%
ggplot(data = ., aes(x = fct_rev(fct_infreq(region)), fill = region)) +
  geom_bar() +
  theme_minimal(base_size = 13) + 
  #geom_text(stat = 'count', 
            #aes(label = ..count..),
            #position = position_dodge(width = 1),
            #size = 4.5,
           # hjust = 1.2) +
  ggtitle("Question 5: County affiliation") +
  coord_flip() +
  paletteer::scale_fill_paletteer_d("ggthemes::excel_Median") +
  xlab("") +
  ylab("") +
  #theme(legend.position="none")
  guides(fill=guide_legend(title="Region"))

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
  paletteer::scale_fill_paletteer_d("waRhol::the_big_c_86") +
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
  paletteer::scale_fill_paletteer_d("waRhol::the_big_c_86") +
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

group.colors.9 <- c('I directly observe conditions' = "#A44122", 'I attend meetings or workshops' = "#DB6725", 'I talk to scientists' ="#F28A32", 'I read scientific publications' = "#F0AE76", 'I collect or analyze data' = "#D6D4C9", 'I follow news, social media, or online forums' = "#A5BDCF", 'I talk to managers or regulators' = "#72A7CF", 'I use websites and data dashboards' = '#5C8FBB', 'I talk to fishers, harvesters, or growers' = "#3F709E")

question_9 %>%
  #mutate(repsonse = factor(response, c("I directly observe conditions", "I attend meetings or workshops", "I talk to scientists", "I read scientific publications", "I collect or analyze data", "I follow news, social media, or online forums", "I talk to managers or regulators", "I use websites and data dashboards", "I talk to fishers, harvesters, or growers"))) %>%
  #mutate(repsonse = factor(question, c("Q9_6", "Q9_4", "Q9_1", "Q9_8", "Q9_7", "Q9_5", "Q9_9", "Q9_3", "Q9_11"))) %>%
  ggplot(., aes(x = fct_rev(fct_infreq(response)), fill = response)) +
  geom_bar() +
  theme_minimal(base_size = 13) + 
  #geom_text(stat = 'count', 
            #aes(label = ..count..),
            #position = position_dodge(width = 1),
            #size = 4.5,
            #hjust = 1.2) +
  ggtitle("Question 9: Main ways you learn about kelp forest-related issues") +
  coord_flip() +
  scale_fill_manual(values=group.colors) +
  xlab("") +
  ylab("") +
  theme(legend.position="none")

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

group.colors.10 <- c('Increasing direct relationships between scientists and non-scientists' = "#DB6725", 'Increasing the participation of scientists in policy efforts' ="#F28A32", 'Making research that is more management-relevant' = "#F0AE76", 'Communicating scientific results in simpler language' = "#D6D4C9", 'Promoting alternative forms of science (e.g. Indigenous knowledge)' = "#A5BDCF", 'Making it easier for the public to access scientific products' = "#72A7CF", 'Increasing public familiarity with scientific tools (e.g. electronic simulation maps)' = '#5C8FBB')

question_10 %>%
  ggplot(., aes(x = fct_rev(fct_infreq(response)), fill = response)) +
  geom_bar() +
  theme_minimal(base_size = 13) + 
  #geom_text(stat = 'count', 
            #aes(label = ..count..),
            #position = position_dodge(width = 1),
            #size = 4.5,
            #hjust = 1.2) +
  ggtitle("Question 10: Main ways to improve integration of science 
          and policy for managing kelp forests") +
  coord_flip() +
  scale_fill_manual(values=group.colors.10) +
  xlab("") +
  ylab("") +
  theme(legend.position="none")

# Question 14 -------
question_14 <- dat_survey %>% 
  select(starts_with("Q14"))

question_14[ , 'Q14_10'] = ""

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "Funding", paste(Q14_10, "Other: Lack of funding"), Q14_10))

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "Funding scarcity", paste(Q14_10, "Other: Lack of funding"), Q14_10))

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "Funding that covers multiple collaborations, lack of funding flexiblity, diving regulations", paste(Q14_10, "Other: Lack of funding"), Q14_10))

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "FUNDING!", paste(Q14_10, "Other: Lack of funding"), Q14_10)) 

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "lack of funding", paste(Q14_10, "Other: Lack of funding"), Q14_10)) 

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "Lack of funding", paste(Q14_10, "Other: Lack of funding"), Q14_10))

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "Lack of funding allocated to South Coast region", paste(Q14_10, "Other: Lack of funding"), Q14_10))

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "Lack of funding, competition for funding, ego", paste(Q14_10, "Other: Lack of funding"), Q14_10))

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "Non-profit focuse on diver only solution to urchin removal priority,area selection and salary pay", paste(Q14_10, "Other: Lack of funding"), Q14_10))

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "Ocean conditions and money", paste(Q14_10, "Other: Lack of funding"), Q14_10))

question_14 <- question_14 %>%
  mutate(Q14_10 = if_else(Q14_9_TEXT == "Permitting of kelp restoration/management projects is rediculously burdensome and expensive.", paste(Q14_10, "Other: Lack of funding"), Q14_10))

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
  pivot_longer(cols = -c(Q14_9, Q14_9_TEXT), names_to = "question", values_to = "response")

question_14 <- question_14[!(is.na(question_14$response) | question_14$response==""), ]

group.colors.14 <- c('Limited resources/staff time' = "#A44122", 'Regulatory obstacles' = "#DB6725", 'Lack of an overarching plan' ="#F28A32", 'Lack of adequate scientific information' = "#F0AE76", 'Distrust among potential partners' = "#D6D4C9", 'Lack of public support' = "#A5BDCF", 'Lack of experience' = "#72A7CF", ' Other: Lack of funding' = '#5C8FBB', 'Lack of suitable partners' = "#3F709E")

question_14 %>%
  ggplot(., aes(x = fct_rev(fct_infreq(response)), fill = response)) +
  geom_bar() +
  theme_minimal(base_size = 13) + 
  #geom_text(stat = 'count', 
            #aes(label = ..count..),
            #position = position_dodge(width = 1),
            #size = 4.5,
            #hjust = 1.2) +
  ggtitle("Question 14: Biggest barriers to partnering with others on kelp 
                     forest-related issues") +
  coord_flip() +
  scale_fill_manual(values=group.colors.14) +
  xlab("") +
  ylab("") +
  theme(legend.position="none")

# Question 15 --------
question_15 <- dat_survey %>% 
  select(starts_with("Q15"))

question_15 <- question_15 %>%
  pivot_longer(cols = -c(Q15_9, Q15_9_TEXT), names_to = "question", values_to = "response")

question_15 <- question_15 %>%
  filter(!is.na(response))

group.colors.15 <- c('Shared goals' = "#A44122", 'Reputation and trustworthiness' = "#DB6725", 'Prior experience' ="#F28A32", 'Funding opportunities' = "#F0AE76", 'Complementary activities' = "#A5BDCF", 'Authority to make decisions' = "#72A7CF", 'Ability to reach a broader network' = '#5C8FBB', 'Access to information' = "#3F709E")

question_15 %>%
  ggplot(., aes(fct_rev(fct_infreq(response)), fill = response)) +
  geom_bar() +
  theme_minimal(base_size = 13) + 
  #geom_text(stat = 'count', 
            #aes(label = ..count..),
            #position = position_dodge(width = 1),
            #size = 4.5,
            #hjust = 1.2) +
  ggtitle("Question 15: Most important factors when choosing partners") +
  coord_flip() +
  scale_fill_manual(values=group.colors.15) +
  xlab("") +
  ylab("") +
  theme(legend.position="none")


  
