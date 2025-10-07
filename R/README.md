## russian >> R

R and Rmd scripts.

Mary's workflow for producing social network: 

**data_processing**

0. process_q5_counties, process_q4_activity, process_q9_source_of_info. *Who works in California? Who provided location information that we need for the social-ecological network?* *identify 'systematic direct observers' from involvement types, information sources for the different levels of the sn in the social-ecological network.*

01-04. process_orgs_[...]. *process ego and alter data for clearer naming, duplicates, cit-sci egos listed as alters, and funders / collaborators listed as egos.* **04_process_orgs_4.R** contains some names of survey respondents, so it's not pushed to the public repository but should be run before script 05.**

05. sen_02_assign_respondent_categories_REF_ONLY. *identify 'systematic direct observers' for the different levels of the sn in the social-ecological network. This is actually done in the repository `california-kelp-SEN` so there can be cross-talk between this script and the script assigning actors to ecological nodes, but is placed here for reference.*

06. social_network_for_sen. *generate edge lists and matrices for the sn in the social-ecological network. the version 'collabs only' does NOT add social ties between organizations that share employees / volunteers.*

07. 07_refine_l1_social_network_for_sen. *filter the edge lists / matrices from script 06 to include only egos and alters that are considered in/on the water.*

**visualization**

social_network_for_sen_visuals. *various visualizations of the social network produced for the sen, including for presentations.*