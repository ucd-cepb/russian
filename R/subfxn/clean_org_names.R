#' Clean organization names
#'
#' The survey asked (1) if respondents were involved on behalf of an 
#' organization, and (2) who they collaborated with. This function
#' manually cleans up survey answers to these questions. General structure
#' of names is as follows:
#' {Primary Organization} - {Subgroup / Project}: {Individual}
#' 
#' Last updated: 7/3/2025
#'
#' @param data a vector of the names to clean
#' @param collab true / false. TRUE: organization names represent a collaboration / coordination / communication tie. **this is important because individual names may be over written if FALSE is incorrectly chosen.
#' @param return_original true / false. FALSE: return output as a 2-col data frame: response ID and org name. TRUE: return output as a 3-col data frame, with third column the original organization names
#' @return vector or list (if return_original)
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
clean_org_names <- function(x, collab = FALSE, return_original = FALSE){
  
  if(!collab){
    ## these are from people listing what orgs they work on behalf of
  out <- 
    case_when(
      grepl('NEREO',x) ~ "California State Polytechnic University Humboldt - North coast Evaluation of Reef Ecosystems Organization",
      grepl(paste(c('Cal Poly Humboldt','Student assistant with Sean Craig'),collapse="|"), x,ignore.case = TRUE) ~ "California State Polytechnic University Humboldt",
      grepl(paste(c('Reef check','reef check','reefcheck','Reef Check','ReefCheck',"Reefcheck"),collapse="|"), x) ~ "Reef Check",
      grepl(paste(c('moss landing','Moss Landing','MLML','Moss landing',"Moss Landing Marine Labs","Moss landing marine labs"),collapse="|"), x) ~ "Moss Landing Marine Laboratories",
      grepl(paste(c("Sunflower Sea Star Laboratory","Sunflower Star Lab","https://www.sunflowerstarlab.org/"),collapse="|"),x) ~ "Moss Landing Marine Laboratories - Sunflower Star Laboratory",
      x=="Association of Zoos and Aquariums SAFE Sunflower Sea Star" ~ "Association of Zoos and Aquariums - SAFE Sunflower Sea Star",
      x=="Moss Landing Marine Labs Aquaculture Center" ~ "Moss Landing Marine Laboratory - Aquaculture Center",
      grepl(paste(c('Giant Giant Kelp','g2kr','giant giant kelp','G2KR','Great Great Kelp',
                    'UrchinsKelpOtters',"Giant Kelp Project","Giant Giant Kelp Restoration Project",
                    "Giant Giant Kelp Restoration Project (G2KR)",
                    "Giant kelp restoration project - g2kr",
                    "Great Great Kelp Restoration"),collapse="|"), x) ~ "Giant Giant Kelp Restoration Project",
      grepl(paste(c('UCSC','UC Santa Cruz'),collapse="|"), x) ~ "University of California Santa Cruz",
      grepl(paste(c('CA Santa Barbara','California, Santa Barbara',"UC Santa Barbara"),collapse="|"),x) ~ "University of California Santa Barbara",
      grepl(paste(c("Bodega marine labs","Bodega Marine Laboratory"),collapse="|"), x) ~ "University of California Davis - Bodega Marine Laboratory",
      grepl(paste(c('UC Davis','Uc Davis','Uc davis','California Davis','California, Davis','uc davis'),collapse="|"), x) ~ "University of California Davis",
      grepl(paste(c('UC Berkeley','UCB','Uc berkeley'),collapse="|"), x) ~ "University of California Berkeley",
      grepl('U.S. Geological Survey', x) ~ "US Geological Survey",
      grepl(paste(c('CDFW','CALIFORNIA DEPARTMENT OF FISH AND WILDLIFE', 
                    'CA Dept of Fish and Wildlife',
                    'California Department of Fish and Wildlife (working with opc as our contractee)',
                    'Fish and Wildlife'),collapse="|"), x) ~ "California Department of Fish and Wildlife",
      grepl('Sea Urchin Commission', x, ignore.case=TRUE) ~ "California Sea Urchin Commission",
      grepl(paste(c('Earth equty', 'Earth equity'),collapse="|"), x) ~ "Earth Equity",
      grepl('Cal Poly Pomona', x) ~ "California State Polytechnic University Pomona",
      grepl('USFWS', x) ~ "US Fish and Wildlife Service",
      grepl(paste(c('CA Ocean Protection Council','California Ocean Protection Council','Calif Ocean Protection Council'),collapse='|'),x) ~ 'California Ocean Protection Council',
      x=='California Ocean Protection Council (contracted by them)' ~ 'California Ocean Protection Council',
      x=='UC San Diego' ~ "University of California San Diego",
      x=="San Diego state university" ~ "San Diego State University", 
      grepl(paste(c("Watermens Alliance","Watermans Alliance","Waterman's Alliance"),collapse="|"), x) ~ "Watermen's Alliance",
      grepl(paste(c("GFNMS Advisory Council","Greater Farralones"),collapse="|"), x) ~ 'Greater Farallones National Marine Sanctuary',
      x=="Ocean Rainforest Inc" ~ 'Ocean Rainforest',
      x=="Coastal Stewardship Task Force, The Sea Ranch Association" ~ "The Sea Ranch Association - Coastal Stewardship Task Force",
      grepl(paste(c("Nature Conservancy","TNC","The Nature Conservancy CA"),collapse='|'),x) ~ "The Nature Conservancy",
      x=="Vantuna Research Group at Occidental College" ~ "Occidental College - Vantuna Research Group",
      x=="Sherwood Valley Rancheria" ~ "Sherwood Valley Band of Pomo Indians",
      x=="Fish Reef Project (fishreef.org)"~"Fish Reef Project",
      x=="The Greater Farallones Association" ~ "Greater Farallones Association",
      grepl(paste(c("KRMP","KRMP community workgroup (CA DFW)"),collapse='|'),x) ~ "Kelp Restoration and Management Plan Community Working Group",
      x=="CDFW Red Abalone Recovery Working Group" ~ "Red Abalone Recovery Working Group",
      x=="Monterey Surfrider Chapter" ~ "Surfrider Foundation - Surfrider Monterey",
      x=="Humboldt Surfrider Foundation Humboldt Chapter" ~ "Surfrider Foundation - Surfrider Humboldt",
      x=="MPA Long Term Monitoring Program" ~ "California MPA Monitoring Program",
      x=="WaveWalker Charters" ~ "Wave Walker Charters",
      x=="West Coast Region, Office of National Marine Sanctuaries" ~ "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries - West Coast Region",
      grepl(paste(c("CA Sea Grant","CA Seagrant"),collapse='|'),x) ~ "California Sea Grant",
      grepl("PISCO",x) ~ "Partnership for Interdisciplinary Studies of Coastal Oceans",
      x=="The Kelp Forest Alliance" ~ "Kelp Forest Alliance",
      x=="SBC LTER" ~ "University of California Santa Barbara - SBC LTER",
      x=="MBNMS" ~ "Monterey Bay National Marine Sanctuary",
      grepl("Scripps",x) & !grepl("Birch",x) ~ "University of California San Diego - Scripps Institution of Oceanography",
      x=="Birch Aquarium" ~  "University of California San Diego - Scripps Institution of Oceanography - Birch Aquarium",
      x=="Monterey Abalone Co." ~ "Monterey Abalone Company",
      x=="Bamboo Reef Scuba Diving Centers" ~ "Bamboo Reef Scuba Diving Centers - Triton Spearfishing",
      x=='Urchin removal diver' ~ NA,
      x=="Stanford University (past)" ~ NA,
      x=="co-PI Santa Barbara Coastal LTER site" ~ "University of California Santa Barbara - SBC LTER",
      x=="PI - Seaweed CDR Project at UCSB" ~ "University of California Santa Barbara - Seaweed CDR Project",
      x=="The Cultured Abalone Farm LLC" ~ "The Cultured Abalone Farm",
      x=="WSP (env consulting)" ~ "WSP",
      x=="Greater Farallones and Cordell Bank National Marine Sanctuaries" ~ "Greater Farallones National Marine Sanctuary, Cordell Bank National Marine Sanctuary",
      x=="I am a harbor Commissioner" ~ "Noyo Harbor",
      x=="Stillwater Cove urchin divers" ~ "Stillwater Cove urchin divers",
      x %in% c("Triton Spearfishing",  # merged with _2
                    "I am a commercial Nearshore Rockfish Fisherman",
                    "I was an Abalone Diver", "Urchin removal diver"
               ) ~ "Individual: Commercial or Recreational Fishing", 
      x=="Independent filmmaking" ~ "Individual: Photographer",
      x=="Volunteer Diving groups" ~ "Individual: Diver",
      x=="North Coast KelpFest!" ~ "North Coast Kelp Fest",
      grepl(paste(c("x",
                    "ANCKR", # merged with _1
                    "Decline to state",
                    "CPC"    # name?
      ),collapse='|'),x) ~ NA, 
      TRUE ~ x
    )
  
  } else if(collab){
      ## these are from people listing collaborators
    #### part 1
    out <- case_when(
      x=="Sergey Nuzhdin  USC/ Alta Seads spore bank" ~ "AltaSeads Conservancy: Sergey Nuzhdin",
      x %in% c("The Kelp Forest Alliance (Eger)","Aaron eger and many many others","Kelp forest alliance") ~ "Kelp Forest Alliance: Aaron Eger",
      x %in% c("New Agency / Above Below","New Agency") ~ "Above/Below",
      x=="Alex Berger" ~ "Sea Forest: Alex Berger",
      
      x=="Ali Boutros (UCSC Coastal Science and Policy graduate program," ~ "University of California Santa Cruz: Ali Boutros", 
      x=="Andrea Paz Lacavex (UCSC)" ~ "University of California Santa Cruz: Andrea Paz Lacavex", 
      x=="Calvin Munson (UCSC)" ~ "University of California Santa Cruz: Calvin Munson", 
      x %in% c("Carr Lab UCSC","UC Santa Cruz (Carr et al)") ~ "University of California Santa Cruz - Raimondi-Carr Lab", 
      x=="Carrie Pomeroy (UCSC)" ~ "University of California Santa Cruz: Carrie Pomeroy", 
      x=="Dan Malone- UCSC" ~ "University of California Santa Cruz: Dan Malone", 
      x %in% c("Dr Pete raimondi","Pete Raimondi (UCSC)") ~ "University of California Santa Cruz: Peter Raimondi", 
      x=="Kristy Kroeker (UCSC)" ~ "University of California Santa Cruz: Kristy Kroeker", 
      x %in% c("Mark Carr- UCSC","UCSC (Carr)","Mark Carr, UCSC") ~ "University of California Santa Cruz: Mark Carr", 
      x %in% c("UC Santa Cruz","UCSC","University of California: Santa Cruz","US Santa Cruz","UC Santa Cruz") ~ "University of California Santa Cruz", 
      x=="UC Santa Cruz Long Marine Lab" ~ "University of California Santa Cruz - Joseph M. Long Marine Laboratory", 
      
      x=="Amah Mutsun" ~ "Amah Mutsun Land Trust",
      
      x %in% c("Andrew kim","Andrew Kim - MLML","Andrew Kim MLML","Andrew Kim- MLML","Andrew Kim, MLML") ~ "Moss Landing Marine Laboratories: Andrew Kim", 
      x=="Moss Landing Marine Labs Aquaculture Center" ~ "Moss Landing Marine Laboratories - Aquaculture Center", 
      x %in% c("Moss Landing marine labs","Moss landing marine labs",
                      "Moss Landing Marine Laboratory","Moss Landing Marine Lab","Moss landing",
                      "Moss Landing Marine Labs") ~ "Moss Landing Marine Laboratories",  
      grepl("Scott Hamilton", x) ~ "Moss Landing Marine Laboratories: Scott Hamilton",
      x=="Scott hamilton" ~ "Moss Landing Marine Laboratories: Scott Hamilton", 
      x %in% c("Bennett Bugbee - MLML","Bennet Bugbee- MLML","Bennett Bugbee, MLML") ~ "Moss Landing Marine Laboratories: Bennett Bugbee", 
      grepl("Mike Graham", x) ~ "Moss Landing Marine Laboratories: Mike Graham",   
      x=="Ashley Kidd" ~ "Moss Landing Marine Laboratories - Sunflower Star Laboratory: Ashley Kidd", 
      x %in% c("Sunflower Sea Star Laboratory","https://www.sunflowerstarlab.org/", "Sunflower Star Lab",
                      "Sunflower Star Lab, Moss Landing","Sunflower Star Laboratory") ~ "Moss Landing Marine Laboratories - Sunflower Star Laboratory", 
      x=="Vince Christian, Sunflower Star Laboratory, Moss Landing" ~ "Moss Landing Marine Laboratories - Sunflower Star Laboratory: Vince Christian",
      
      x=="Andrew Rassweiler, Florida State University" ~ "Florida State University: Andrew Rassweiler", 
      x=="FSU" ~ "Florida State University", 
      
      x %in% c("GFNMS",
                      "Greater farallones",
                      "Greater Farrallones",
                      "Greater Farralones",
                      "Gulf of the Farralons National Marine Sanctuary",
                      "NOAA Greater Farallones National Marine Sanctuary") ~ "Greater Farallones National Marine Sanctuary", 
      x %in% c("GFNMS Advisory Council",
                      "Greater Farallones Sanctuary Advisory Council") ~ "Greater Farallones National Marine Sanctuary - Greater Farallones National Marine Sanctuary Advisory Council", 
      
      grepl('Rietta Holman',x) ~ "Greater Farallones Association: Rietta Hohman", # Mary changed to GFA
      x=="GFNMS (Holman)" ~ "Greater Farallones Association: Rietta Hohman",      # Mary changed to GFA
      x=="Rietta Hoffman- GFA" ~ "Greater Farallones Association: Rietta Hohman", # Mary changed to GFA
      grepl("Angela Zepp",x) ~ "Greater Farallons Assocation: Angela Zepp", # Mary changed to GFA 
      
      x %in% c("The Greater Farrallones Association",
                      "The Greater Farallones Kelp Restoration Project (GFA)",
                      "Greater Farrallons association",
                      "Gulf of the Farallons Association",
                      "Kelp Restoration Team, Greater Farallones Association",
                      "Gulf of the Farallones Association") ~ "Greater Farallones Association", 
      grepl("Gina Contolini",x) ~ "Greater Farallones Association: Gina Contolini", 
      grepl("Julieta Gomez",x) ~ "Greater Farallones Association: Julieta Gomez", 
      grepl("Tyler Mears",x) ~ "Greater Farallones Association: Julieta Gomez", 
      
      grepl("Anita Giraldo", x) ~ "University of California Santa Barbara: Anita Giraldo Ospina", 
      x %in% c("Univ CA Santa Barbara",
                      "University of California, Santa Barbara",
                      "UCSB",
                      "Colleagues at UCSB and in New Zealand",
                      "UC Santa Barbara") ~ "University of California Santa Barbara", 
      x=="Capt. Merit McCrea" ~ "University of California Santa Barbara: Merit McCrea", 
      x=="Dan Reed (UCSB)" ~ "University of California Santa Barbara: Dan Reed", 
      x=="Dr. Steve Schroder" ~ "University of California Santa Barbara: Stephen Schroeter", 
      x=="Jennifer Caselle (UCSB)" ~ "University of California Santa Barbara: Jennifer Caselle", 
      x=="Steve Schroeter, UCSB" ~ "University of California Santa Barbara: Steve Schroeter", 
      
      grepl("Annie Bauer-Civiello", x) ~ "Reef Check: Annie Bauer-Civiello", 
      grepl("Ian Norton",x) ~ "Reef Check: Ian Norton", 
      grepl("Morgan Murphy",x) ~ "Reef Check: Morgan Murphy-Cannella", 
      grepl(paste(c("Dan Abbot","Dan Abbott"),collapse='|'),x) ~ "Reef Check: Dan Abbott", 
      grepl(paste(c("Jan freiwald","Jan Freiwald","Jan Friewald"),collapse='|'),x) ~ "Reef Check: Dan Abbott", 
      x %in% c("Reef","reef check","Reef check","Reef Check California",
                      "Reef Check Central California",
                      "Reef Check Foundation",
                      "Reef Check in Sonoma & Mendocino as volunteer",
                      "Reef Check Kelp Forest Program",
                      "Reef Check KFM",
                      "Reef Check of California", 
                      "Reef Check So Cal region",
                      "Reef Check Southern California",
                      "Reef check staff in Sonoma, Mendocino, and Monterey",
                      "Reefcheck" = "Reef Check", "ReefCheck", 
                      "Reefcheck CA", "ReefCheck CA", "reef check ca.", 
                      "Reef check so cal", "Reef Check CA") ~ "Reef Check",

      grepl("Anthony Shiao", x) ~ "California Department of Fish and Wildlife: Anthony Shiao",
      grepl("Laura Rogers-Bennett",x) ~ "California Department of Fish and Wildlife: Laura Rogers-Bennett",
      grepl("Ian Kelmartin",x) ~ "California Department of Fish and Wildlife: Ian Kelmartin", 
      grepl("Kirsten Ramey",x) ~ "California Department of Fish and Wildlife: Kirsten Ramey",
      grepl("Kristen Elsmore",x) ~ "California Department of Fish and Wildlife: Kristen Elsmore",
      x=="Mike Prall, CDFW" ~ "California Department of Fish and Wildlife: Mike Prall", 
      grepl(paste(c("CDFW","CA DFW",
                    "Calif. Dept. of Fish and Wildlife",
                    "California Fish and Wildlife"),collapse='|'), x) ~ "California Department of Fish and Wildlife",
      x=="California Department of Fish and Wildlife Marine Region" ~ "California Department of Fish and Wildlife", 
      x=="Fish and Wildlife" ~ "California Department of Fish and Wildlife", 
      x=="The California Department of Fish and Wildlife" ~ "California Department of Fish and Wildlife", 
      x=="CALIFORNIA DEPARTMENT OF FISH AND WILDLIFE" ~ "California Department of Fish and Wildlife",


      x=="NOAA Montrose Settlements Restoration Program" ~ "Montrose Settlements Restoration Program",
      x=="SBC LTER" ~ "University of California Santa Barbara - SBC LTER",
      x=="Anthropogene Institute" ~ "Anthropocene Institute",
      x=="April Ridlon (Monterey Bay Aquarium)" ~ "Monterey Bay Aquarium: April Ridlon",

      grepl(paste(c("Josh Smith","Joshua Smith"),collapse='|'), x) ~ "Monterey Bay Aquarium: Josh Smith",
      x %in% c("MB Aquarium","The Monterey Bay Aquarium") ~ "Monterey Bay Aquarium",

      grepl("Lonhart", x) ~ "Monterey Bay National Marine Sanctuary: Steve Lonhart",
     x %in% c("MBNMS","NOAA MBNMS") ~ "Monterey Bay National Marine Sanctuary",
     x=="NOAA Monterey Bay National Marine Sanctuary" ~ "Monterey Bay National Marine Sanctuary",

     x=="Monterey Bay Mermaid # Alison Smith" ~ "Monterey Bay Mermaid: Alison Smith",

     x=="Art Seavey, Monterey Abalone Company" ~ "Monterey Abalone Company: Art Seavey",
     x=="MC Weekly" ~ "Monterey County Weekly",

     x=="Bay Net" ~ "National Oceanic and Atmospheric Administration - Bay Net",
     x=="CA coastal national marine sanctuaries" ~ "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries - West Coast Region",
     x=="Cameron Spier, SWFSC" ~ "National Oceanic and Atmospheric Administration - Southwest Fisheries Science Center: Cameron Spier",

     x=="NOAA" ~ "National Oceanic and Atmospheric Administration",
     x=="NOAA NCCOS" ~ "National Oceanic and Atmospheric Administration - National Centers for Coastal Ocean Science",
     x=="Office of National Marine Sanctuaries - Multiple" ~ "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries",
     x=="Zach Gold, National Oceanic and Atmospheric Administration" ~ "National Oceanic and Atmospheric Administration: Zach Gold",
     x=="NOAA Office of National Marine Sanctuaries" ~ "National Oceanic and Atmospheric Administration - Office of National Marine Sanctuaries",

     x=="Bengt Allen, CSU Long Beach" ~ "California State University Long Beach: Bengt Allen",
     grepl(paste0(c("CSU Long Beach", "CSULB"),collapse="|"),x) ~  "California State University Long Beach",

     grepl(paste0(c("Tristin McHugh",
                    "Tristen McHugh",
                    "Tristan McHugh"),collapse="|"),x) ~ "The Nature Conservancy: Tristin McHugh",
     grepl("Norah Eddy",x) ~ "The Nature Conservancy: Norah Eddy",
     grepl(paste0(c("Benjamin Grime",
                    "Benjaman.grimes@tnc"),collapse="|"),x) ~ "The Nature Conservancy: Ben Grime",
     x=="everyone on the Eastern Pacific Kelp Restoration forum via The Nature Conservancy (BC to Baja)" ~ "The Nature Conservancy - Eastern Pacific Kelp Restoration Forum",
     x=="Jono Wilson, The Nature Conservancy" ~ "The Nature Conservancy: Jono Wilson",
     x=="Tom Dempsey, TNC" ~ "The Nature Conservancy: Tom Dempsey",
     x %in% c("Nature conservancy",
              "Nature Conservancy",
              "The Nature Conservancey CA",
              "The nature conservancy",
              "The Nature Conservancy Australia and Tas",
              "The Nature Conservancy of California") ~ "The Nature Conservancy",
     grepl("TNC",x) ~ "The Nature Conservancy",
     
     grepl("Ed Parnell",x) ~ "University of California San Diego - Scripps Institution of Oceanography: Ed Parnell",
     grepl("Jen Smith",x) ~ "University of California San Diego - Scripps Institution of Oceanography: Jen Smith",
     grepl("Kristin Riser",x) ~ "University of California San Diego - Scripps Institution of Oceanography: Kristin Riser", 
     grepl("Paul Dayton",x) ~ "University of California San Diego - Scripps Institution of Oceanography: Paul Daytona",
     grepl("Jim Leichter",x) ~ "University of California San Diego - Scripps Institution of Oceanography: Jim Leichter", 
     x=="Birch Aquarium" ~ "University of California San Diego - Scripps Institution of Oceanography - Birch Aquarium", 
     x %in% c("Scripps",
             "Scripps Institute of Oceanography: Masters of Advanced Studies in Marine Biodiversity & Conservation (MAS MBC)",
             "Scripps Institution of Oceanography",
             "SIO",
             "Scripps Inst. of Oceanography") ~ "University of California San Diego - Scripps Institution of Oceanography", 
     x=="UC San Diego" ~ "University of California San Diego", 

            # San Diego State University
     grepl(paste0(c("Mathew Edwards",
                    "Matt Edwards","Matthew Edwards"),collapse="|"),x) ~ "San Diego State University: Matthew Edwards",
     x=="Shelby Penn SDSU" ~ "San Diego State University: Shelby Penn",
     x=="San Diego state university" ~ "San Diego State University",
     x=="Blue Harmony Foundation" ~ "Blue Harmony",

     grepl(paste0(c("BML","Bodega Marine Labs","Bodega Bay Marine Labs","bodega marine labs"),collapse="|"),x) ~ "University of California Davis - Bodega Marine Laboratory",
     grepl("Gabby Yang",x) ~ "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Marissa Baskett",
     grepl("Marissa Baskett",x) ~ "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Gabby Yang",
     grepl("springborn",x, ignore.case=TRUE) ~ "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System: Michael Springborng",
     grepl(paste0(c("DISES","DiSES","Kelp RISES"),collapse="|"),x) ~ "University of California Davis - Kelp Restoration as an Integrated Socio-Ecological System",
     x=="Rachael Bay, UC Davis" ~ "University of California Davis: Rachael Bay",
     x=="The University of California, Davis" ~ "University of California Davis", 
     x=="UC Davis Bodega Marine Lab - John Largier" ~ "University of California Davis - Bodega Marine Laboratory: John Largier",
     x=="UC Davis Bodega Marine Lab Boating and Diving Safety Office" ~ "University of California Davis - Bodega Marine Laboratory - Boating and Diving Safety Office",
     x %in% c("UC Davis Department of Environmental Science and Policy",
              "Uc davis","UC Davis",
              "University of California, Davis",
              "University of California, Davis Coastal & Marine Sciences Institute",
              "uc davis") ~ "University of California Davis",

     x=="BOEM" ~ "Bureau of Ocean Energy Management",

            # Sonoma State University
     grepl(paste0(c("Brent Hughes","Hughes Lab"),collapse="|"),x) ~ "Sonoma State University: Brent Hughes",
     grepl("Rachael Karm",x) ~ "Sonoma State University: Rachael Karm",
     x %in% c("Sonoma State","SSU") ~ "Sonoma State University",

     x=="CA Coastal Commission" ~ "California Coastal Commission", x=="Coastal Commission" ~ "California Coastal Commission",

     x %in% c("CA Dept. of Parks & Recreation - Santa Cruz",
                     "CA DPR",
                     "State Parks")~ "California State Parks",
     x=="CA State Parks - Fort Ross" ~ "California State Park - Fort Ross Conservancy",
     x=="California State Parks Interpretation & Education Division" ~ "California State Parks - Interpretation and Education Division",
     x=="California State Parks Natural Resources Division" ~ "California State Parks - Natural Resources Division",
     x=="California State Parks - Interpretation & Education Division" ~ "California State Parks - Interpretation and Education Division",
     x=="The Fort Ross Conservancy" ~ "California State Parks - Fort Ross Conservancy",
     #
     x=="CA DIving News" ~ "California Diving News",
     x=="Steinhart Aquarium (California Academy of Sciences)" ~ "California Academy of Sciences",
     x %in% c("CA Fish and Game Commission","CFGC","Fish & Game") ~ "California Fish and Game Commission",
     x=="CA Fish and Game Commission - Susan Ashcraft" ~ "California Fish and Game Commission: Susan Ashcraft",

     # California Ocean Science Trust
     grepl("Ocean Science Trust",x) ~ "California Ocean Science Trust",

            # California Ocean Protection Council
     grepl("Pike Spector",x) ~ "California Ocean Protection Council: Pike Spector",
     grepl("Esgro",x) ~  "California Ocean Protection Council: Mike Esgro",
            x %in% c("CA OPC",
                            "Calif Ocean Protection Council",
                            "ocean protection council",
                            "Ocean Protection Council",
                            "OPC") ~ "California Ocean Protection Council",
     #
     grepl(paste0(c("(Oh)","Shauna Oh"),collapse="|"),x) ~ "California Sea Grant: Shauna Oh",
     x=="Jami Miller, Sea Grant / City of Fort Bragg" ~ "California Sea Grant: Jami Miller",
     x %in% c("CA Sea Grant",
              "CA SeaGrant",
              "Cal seagrant",
              "california sea grant",
              "Sea grant",
              "Sea Grant") ~ "California Sea Grant",

            # California Sea Urchin Commission
     grepl(paste0(c("David Goldenburg","David Goldenberg"),collapse="|"),x) ~ "California Sea Urchin Commission: David Goldenberg",
     grepl(paste0(c("Grant Downey","Grant Downie"),collapse="|"),x) ~ "California Sea Urchin Commission: Grant Downie",
     x %in% c("California Sea Urchin Commission (CSUC)",
              "California sea urchin commission",
              "CA Sea Urchin Commission",
              "Sea urchin commission",
              "The Urchin Commission",
              "Cal Urchin Commission",
              "California Sea Urchin Comission",
              "URCHIN COMMISSION") ~ "California Sea Urchin Commission",

            # California State Lands Commission
            x=="CA State Lands Commission" ~ "California State Lands Commission",

            # California State Polytechnic University Humboldt
     grepl("Sean Craig",x) ~ "California State Polytechnic University Humboldt: Sean Craig",
     #
     x=="Cal Poly Humboldt - Oceanography department (Christine Cass)" ~ "California State Polytechnic University Humboldt: Christine Cass",
     x=="Cal Poly Humboldt, Food Sovereignty Lab, Dr. Cutcha Risling Baldy" ~ "California State Polytechnic University Humboldt - Food Sovereignty Lab: Cutucha Risling Baldy",
     x=="Cal Poly Humboldt, Jody Martinez" ~ "California State Polytechnic University Humboldt: Jody Martinez",
     x=="Duncan Jackson" ~ "California State Polytechnic University Humboldt: Duncan Jackson",
     x=="Frankie Moitoza (Cal Poly Humboldt)" ~ "California State Polytechnic University Humboldt: Frankie Moitoza",
     #
     x=="HSU (Craig)" ~ "California State Polytechnic University Humboldt: Sean Craig",
     x=="HSU Rick Alvarez" ~ "California State Polytechnic University Humboldt: Rick Alvarez",
     #
     x=="Paul Bourdeau" ~ "California State Polytechnic University Humboldt: Paul Bourdeau",
     x=="Rafael Cuevas Uribe" ~ "California State Polytechnic University Humboldt: Rafael Cuevas Uribe",
     x=="Rafael Cuveus-Uribe, Cal Poly Humboldt" ~ "California State Polytechnic University Humboldt: Rafael Cuevas Uribe",
     x=="Telonicher Marine Lab" ~ "California State Polytechnic University Humboldt - Telonicher Marine Lab",

     grepl("NEREO",x) ~ "California State Polytechnic University Humboldt - North coast Evaluation of Reef Ecosystems Organization",
     grepl(paste0(c('Cal Poly Humboldt','CalPoly Humboldt','Humbolt state','HSU'),collapse="|"),x) ~ "California State Polytechnic University Humboldt",

     x=="Cal Poly Pomona" ~ "California State Polytechnic University Pomona",
     x=="Cal State Northridge" ~ "California State University Northridge",
     x=="Janet Kubler scientist CSUN" ~ "California State University Northridge: Janet Kubler",

     x=="Calla Allison and the MPA Collaborative Network" ~ "Marine Protected Area Collaborative Network: Calla Allison",
     x %in% c("Monterey MPACN",
              "MPA CN",
              "MPA Collaborative Network",
              "MPA Collaborative Network, Humboldt Co.",
              "MPA Collaboratives",
              "MPA Long Term Monitoring Project") ~ "Marine Protected Area Collaborative Network",

     grepl(paste0(c('Keith Rootsaert','Keith Rootseart',
                    'Keith Rooseart',
                    'Keith Roostaert','Keith  Rootsart'),collapse="|"),x) &
       grepl('Caspar Cove',x) ~ "Giant Giant Kelp Restoration Project - Caspar Cove Project: Keith Rootsaert",
     grepl(paste0(c('Keith Rootsaert','Keith Rootseart',
                    'Keith Rooseart',
                    'Keith Roostaert','Keith  Rootsart'),collapse="|"),x)  &
       !grepl('Caspar Cove',x) ~ "Giant Giant Kelp Restoration Project: Keith Rootsaert",

     x=="Caspar Cove kelp restoration project Jon Holcomb" ~ "Giant Giant Kelp Restoration Project - Caspar Cove Project: Jon Holcomb",
     x=="Giant Giant Kelp Restoration Project - Caspar Cove" ~ "Giant Giant Kelp Restoration Project - Caspar Cove Project",
     x %in% c("G2KR","g2kr.com",
              "Giant giant kelp restoration",
              "Giant Giant Kelp Restoration","Giant Giant Kelp Restoration Project (G2KR)",
              "giant giant kep restoration","Giant Kelp Restoration Project",
              "Great Great Kelp Restoration Project","Great great kelp restoration project (Monterey)",
              "Great Great Kelp Restoration", "https://www.facebook.com/UrchinsKelpOtters","g2kr") ~ "Giant Giant Kelp Restoration Project",

     grepl("alliance",x,ignore.case=TRUE) & grepl("Josh",x) ~ "Watermen's Alliance: Josh Russo",
     grepl("alliance",x,ignore.case=TRUE) & grepl("waterm",x,ignore.case=TRUE) &
       grepl("Caspar",x) & !grepl("Josh",x) ~ "Waterman's Alliance - Caspar Cove Project",
     grepl("alliance",x,ignore.case=TRUE) & grepl("waterm",x,ignore.case=TRUE) &
       !grepl("Caspar",x) & !grepl("Josh",x)  ~ "Waterman's Alliance",

     x=="Catalina island marine institute" ~ "Catalina Island Marine Institute",
     x=="Deidre Sullivan  - Catalina Island Marine Institute" ~ "Catalina Island Marine Institute: Deidre Sullivan",
     x=="CBNMS Advisory Council" ~ "Cordell Bank National Marine Sanctuary - Cordell Bank National Marine Sanctuary Advisory Council",
     x=="Channel Islands National Marine Sanctuary Advisory Council" ~ "Channel Islands National Marine Sanctuary - Channel Islands National Marine Sanctuary Advisory Council",
     grepl("Scott Gabara",x) ~ "Channel Islands National Marine Sanctuary: Scott Gabara",
     grepl("Freedman",x) ~ "Channel Islands National Marine Sanctuary: Ryan Freedman",
     x %in% c("NOAA Channel Islands National Marine Sanctuary",
              "Channel Islands National Park",
              "NOAA CINMS" ) ~ "Channel Islands National Marine Sanctuary",

     x=="Chris Goldblatt of Fish Reef Project (fishreef.org)" ~ "Fish Reef Project: Chris Goldblatt",
     x %in% c("City of fort bragg","City of Ft Bragg") ~ "City of Fort Bragg",
     x=="Sarah McCormick, City of Fort Bragg" ~ "City of Fort Bragg: Sarah McCormick",
     grepl("Redwood Elementary", x) ~ "Redwood Elementary",
     x=="Coastal Conservation Association - California Chapter" ~ "Coastal Conservation Association California",
     x=="Hank Goebel" ~ "Coastal Conservation Association California: Hank Goebel",
     x=="Coastal Stewardship Task Force, The Sea Ranch Association" ~ "The Sea Ranch Association - Coastal Stewardship Task Force",
     x=="Col. David Lopez, USAF (ret.)" ~ "David Lopez",
     x=="CSU Agricultural Research Institute" ~ "California State University Agricultural Research Institute",
     x %in% c("CSU Monterey Bay","CSUMB") ~ "California State University Monterey Bay",
     x=="CSUC" ~ "California State University Chico",
     grepl('Dan Okamoto',x) ~ "University of California Berkeley: Dan Okamoto",
     x=="Maya Munstermann, expert scientist" ~ "University of California Berkeley: Maya Munstermann",
     x %in% c("UC Berkeley")~ "University of California Berkeley",
     grepl("Pondella",x) ~ "Occidental College - Vantuna Research Group: Dan Pondella",
     grepl("Vantuna Research Group",x) & !grepl("Pondella",x) ~ "Occidental College - Vantuna Research Group",
     grepl(paste0(c("Dan sweezy","Dan Sweazy","Dan Swezey"),collapse="|"),x, ignore.case=TRUE) ~ "Kashia Band of Pomo Indians: Dan Swezey",
     x=="KASHIA" ~ "Kashia Band of Pomo Indians",
     
      x=="Deeperblue.com" ~ "DeeperBlue",
     x=="Doug Bush, Ocean Rainforest" ~ "Ocean Rainforest: Doug Bush",
     x=="Ocean Rainforest, Inc." ~ "Ocean Rainforest",
     x=="Doug Bush, The Cultured Abalone" ~ "The Cultured Abalone Farm: Doug Bush",
     x=="The Cultured Abalone Farm Rick Gutierrez" ~ "The Cultured Abalone Farm: Rick Gutierrez",
     x=="The Cultured Abalone Farm LLC" ~ "The Cultured Abalone Farm",
     x=="Elizabeth Carpenter & NOYO Center - outreach webinar for Sea Otter Awareness Week 2023" ~ "Noyo Center for Marine Science: Elizabeth Carpenter",
     grepl("Sheila Semans",x) ~ "Noyo Center For Marine Science: Sheila Semans",
     grepl("Noyo",x,ignore.case=TRUE) & grepl("center",x,ignore.case=TRUE) & !grepl("Sheila",x) ~ "Noyo Center For Marine Science",
     x %in% c("Friday Harbor Lab",
              "University of Washington, Friday Harbor Labs") ~ "University of Washington - Friday Harbor Laboratories",
     x=="Friday Harbor Lab, University of Washington (Hodin lab)" ~ "University of Washington - Friday Harbor Laboratories - Hodin Lab",
     x=="WA DNR; Helen Berry" ~ "Washington State Department of Natural Resources: Helen Berry",
     x=="Gary Fleener, Hog Island Oyster Company" ~ "Hog Island Oyster Company: Gary Fleener",
     x=="ESNERR" ~ "Elkhorn Slough Ecological Reserve",
     grepl("Nancy Caruso",x) ~ "Get Inspired: Nancy Caruso",
     x=="Girl Scouts of America - Cheryl Kingman" ~ "Girl Scouts of America: Cheryl Kingman",
     x=="GWAII HAANAS" ~ "Gwaii Haanas National Park Reserve",
     x=="HAIDA FISHERIES" ~ "Council of the Haida Nation -  Haida Fisheries Program",
     x=="Hallie Brown, CA Marine Sanctuary Foundation" ~ "California Marine Sanctuary Foundation: Hallie Brown",
     x=="Heather Burdick Bay Foundation" ~ "The Bay Foundation: Heather Burdick",
     x=="Humboldt Surfrider" ~ "Surfrider Foundation - Surfrider Humboldt",
     x=="Inter Tribal Sinkyone Wilderness Council" ~ "InterTribal Sinkyone Wilderness Council",
     x=="Jarrett Byrnes, University of Massachusetts Boston" ~ "University of Massachusetts Boston: Jarrett Byrnes",
     x=="Jeremy McFarland, University of Nevada, Reno" ~ "University of Nevada Reno: Jeremy McFarland",
     x=="Jessica Gravelle, Trinidad Rancheria" ~ "Trinidad Rancheria: Jessica Gravelle",
     x=="The Cher-Ae Heights Indian Community of the Trinidad Rancheria" ~ "Trinidad Rancheria",
     x=="Jorge Arroyo-Esquival & Marine Conservation Institute - outreach webinar for Sea Otter Awareness Week 2023" ~ "Marine Conservation Institute: Jorge Arroyo Esquival",

     x %in% c("Kelp restoration management plan - CA",
              "Kelp Restoration Management Plan Scientific Advisory Team") ~ "Kelp Restoration and Management Plan Scientific Advisory Team",
     x %in% c("KRMP", "KRMP (CDFW)", "California Kelp Restoration and Management Community Group",
              "Members of the KRMP stakeholder group") ~ "Kelp Restoration and Management Plan Community Working Group",

     x=="LA Waterkeeper - Michael Quill" ~ "LAWaterkeeper: Michael Quill",
     x=="Lauren Schiebelhut" ~ "University of California Merced: Lauren Schiebelhut",
     grepl("Dawson",x) ~ "University of California Merced: Michael Dawson",
     x=="UC Merced" ~ "University of California Merced",
     x=="Loren Crotty (private fundraising event)" ~ "Sepia Lux: Loren Crotty",
     x=="National Fish and Wildlife Foundation - Multiple" ~ "National Fish and Wildlife Foundation",
     x=="NCEAS kelp working group (lead is Eger)" ~ "National Center for Ecological Analysis and Synthesis",
     x %in% c("NMSF",
              "NOAA NMSF",
              "NOAA NMS",
              "NOAA Sanctuaries") ~ "National Marine Sanctuary Foundation",
     x=="NOAA NMFS" ~ "National Marine Fisheries Service",  ## edited from sanctuary foundation
     #
     x=="noozhawk.com (online news service)" ~ "Noozark",
     x=="OR DFW; Scott Marion" ~ "Oregon Department of Fish and Wildlife: Scott Marion",
     x=="Oregon Department of Fisheries and Wildlife" ~ "Oregon Department of Fish and Wildlife",
     grepl("Calvanes",x) ~ "Oregon Kelp Alliance: Tom Calvanes",
     x %in% c("Oregon Kelp Forest Alliance","Oregon kelp alliance") ~ "Oregon Kelp Alliance",
     x=="others at SJSU" ~ "San Jose State University",
     x=="Paula Sylvia at the Port of San Diego" ~ "Port of San Diego: Paula Sylvia",
     x=="Port of LA" ~ "Port of Los Angeles",
     x=="Puget sound restoration fund" ~ "Puget Sound Restoration Fund",
     x=="Puget Sound Restoration Fund in seattle washington" ~ "Puget Sound Restoration Fund",

     x=="PISCO" ~ "Partnership for Interdisciplinary Studies of Coastal Oceans",

     x=="Pycnopodia Restoration Group" ~ "Pycnopodia Recovery Working Group",
     x=="Green Wave" ~ "GreenWave",
     x=="REEF,  Janna Nichols" ~ "Reef Environmental Education Foundation: Janna Nichols",
     x=="Rep. Huffman (district 2)" ~ "United States Congress: Jared Huffman",
     x=="Rodrigo Beas-Lunas (ABC Ensenada)" ~ "Universidad Autónoma de Baja California: Rodrigo Beas-Lunas",
     x=="Rosa Laucci, Tolowa Dee-ni' Nation" ~ "Tolowa Dee-ni' Nation: Rosa Laucci",
     x %in% c("Tolowa Dee ni' Nation", "Tolowa Dee-Ni' Nation") ~ "Tolowa Dee-ni' Nation",
     x=="SalesForce" ~ "Salesforce",
     x=="SANDAG" ~ "San Diego Association of Governments",
     x=="Save our Shores" ~ "Save Our Shores",
     x=="SCCOOS" ~ "Southern California Coastal Ocean Observing System",
     x=="SCCWRP" ~ "Southern California Coastal Water Research Project",
     x=="Sherwood Valley Band of Pomo" ~ "Sherwood Valley Band of Pomo Indians",
     
     x=="Stanford" ~ "Stanford University",
     x=="Stanford University - Steven Monismith" ~ "Stanford University: Steven Monismith",
     
     x=="State Lands" ~ "California State Lands Commission",
     grepl("Strategic Earth",x) ~ "Strategic Earth Consulting",
     grepl(paste0(c("Torre Polizzi","Torre pollozi"),collapse="|"),x) ~ "Sunken Seaweed: Torre Polizzi",
     x=="The Jet Lagged (photographers)" ~ "The Jetlagged",
     x=="U Southern California" ~ "University of Southern California",
     x %in% c("U.S. Navy","US Navy") ~ "United States Navy",
     x=="USGS" ~ "United States Geological Survey",
     x=="U.S. Geological Survey" ~ "United States Geological Survey",
     x=="UCLA" ~ "University of California Los Angeles",
     grepl("Kyle Cavanaugh",x) ~ "University of California Los Angeles: Kyle Cavanaugh",
     x=="University of Miami Rosenstiel School of Marine, Atmospheric, and Earth Science - Claire Paris" ~ "University of Miami: Claire Paris",

     x=="UO OIMB Biology department - Aaron Galloway" ~ "University of Oregon: Aaron Galloway",
     x=="URCHIN HARVESTORS ASSOCIATION" ~ "Pacific Urchin Harvesters Association",

     # United States Fish and Wildlife Service
     x %in% c("US Fish and Wildlife Service",
              "United States Fish and Wildlife Service",
              "US Fish and Wildlife") ~ "United States Fish and Wildlife Service",

     grepl("Tom Bell",x) ~ "Woods Hole Oceanographic Institution: Tom Bell",
     x=="WHOI (Bell)" ~ "Woods Hole Oceanographic Institution: Tom Bell",
     x=="Earth equty" ~ "Earth Equity",
     x=="WSP (env consulting)" ~ "WSP",
     x=="West Coast Ocean alliance" ~ "West Coast Ocean Alliance",


     # Individuals
     ## --note-Mary standardized "commercial fisher" "diver" etc.
     x=="Byron Kohler (urchin diver" ~ "Commercial Urchin Diver: Byron Kohler",
     x=="Gary Trumper- urchin diver" ~ "Commercial Urchin Diver: Gary Trumper",
     x=="Mickey Kitahara- urchin diver" ~ "Commercial Urchin Diver: Mickey Kitahara",
     x=="Urchin removal diver" ~ "Urchin Divers",
     x=="Many Kelp Restoration Divers" ~ "Urchin Divers",
     x=="commercial fishermen in San Diego" ~ "Commercial Urchin Diver",
     x=="Urchin removal" ~ "Urchin Divers",
     x=="Dale Glanz" ~ "Dale Glanz",
     x=="Caspar Cove kelp restoration project Jon Holcomb" ~ "Commercial Urchin Diver: Jon Holcomb",
     x=="Erik Owen (commercial fisher)" ~ "Commercial Urchin Diver: Erik Owen",
     x=="Ernest (Tad) Thompson (commercial fisher)" ~ "Commercial Urchin Diver: Ernest Thompson",
     x=="Jeffrey Gritsch (commercial fisher)" ~ "Commercial Urchin Diver: Jeffrey Gritsch",
     grepl("Mark Gmeiner",x) ~ "Commercial Urchin Diver: Mark Gmeiner",
     # Grant Downie is on the urchin commission, urchin diver, kelp restoration specialist, etc
     grepl(paste0(c("Grant Downey","Grant Downie"),collapse="|"),x,ignore.case=TRUE) ~ "Commercial Urchin Diver: Grant Downie",
     x=="Pat Downie (commercial fisher)" ~ "Commercial Urchin Diver: Pat Downie",
     x=="Jeremy George Petty (commercial fisher)" ~ "Commercial Urchin Diver: Jeremy George Petty",
     grepl("Josie Iselin",x) ~ "Artist: Josie Iselin",
     x=="Kevin Quider (commercial fisher)" ~ "Commercial Urchin Diver: Kevin Quider",
     x=="Patrick Webster, science communicator and underwater photographer" ~ "Photographer: Pat Webster",
     x=="Roger Carlson, Roger Carlson Photography" ~ "Photographer: Roger Carlson",
     x=="Ron McPeak" ~ "Photographer: Ron McPeak",
     x=="Shaun Wolfe, Shaun Wolfe Photography" ~ "Photographer: Shaun Wolfe",
     TRUE ~ x
    )
  }  # end if(collab)
  
  if(!return_original){ return(out) }
  if(return_original){ return( list(orig=data, new=out) )  }

  
}
