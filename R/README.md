## russian >> R

R and Rmd scripts.

Mary's workflow for producing social network: 

**data_processing**

1. process_orgs_1, process_orgs_2, process_orgs_3. *clean up responses on what organizations people work on behalf of, and who people collaborate with.*

2. process_q5_counties. *Who works in California? Who provided location information that we need for the social-ecological network?* 

3. process_q4_activity, process_q9_source_of_info. *identify 'systematic direct observers' for the different levels of the sn in the social-ecological network.*

4. sen_02_assign_respondent_categories_REF_ONLY. *identify 'systematic direct observers' for the different levels of the sn in the social-ecological network. This is actually done in the repository california-kelp-SEN so there can be cross-talk between this script and the script assigning actors to ecological nodes, but is placed here for reference.*

5. social_network_for_sen. *some additional data cleaning of organization names. generate two edge lists for the sn in the social-ecological network. the version 'collabs only' does NOT add social ties between organizations that share employees / volunteers.*

**visualization**

1. social_network_for_sen_visuals. *various visualizations of the social network produced for the sen, including for presentations.*