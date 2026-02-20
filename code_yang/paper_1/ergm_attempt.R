library(readr)
library(dplyr)

edge <- read_csv("Desktop/R_stuff/kelp_restoration/russian/data_yang/network_specific copy/edgelists.csv")
node <- read_csv("Desktop/R_stuff/kelp_restoration/russian/data_yang/network_specific copy/nodelist.csv")

edge <- edge %>% relocate(type, .after = last_col())
edge1 <- edge %>%
   rowwise() %>%
   mutate(
      node1 = min(from_2, to_1),
      node2 = max(from_2, to_1)
   ) %>%
   ungroup() %>%
   count(node1, node2, name = "frequency")

edge1 <- edge1 %>%
   filter(node1 != node2)

node$organization_type <- NULL
node$id <- NULL

library(statnet)
library(ergm)

net1 <- network(
   edge1,
   vertices = node,
   directed = TRUE
)

model1 <- ergm(
      net1 ~ edges +
      gwidegree(0.25, fixed = TRUE) +
      gwesp(0.25, fixed = TRUE, type = "OTP") +
      nodeifactor("organization_label")
   ) # did not run

edge2 <- subset(edge, select = c(from_2, to_1))

edge2 <- edge2 %>%
   dplyr::filter(from_2 != to_1)

net2 <- network(
   edge2,
   vertices = node,
   multiple = TRUE,
   directed = TRUE
)

model2 <- ergm(
   net2 ~ edges +
      gwidegree(0.25, fixed = TRUE) +
      gwesp(0.25, fixed = TRUE, type = "OTP") +
      nodeifactor("organization_label"),
) # did not run

# changed to adjaceny matrix
edge3 <- as.edgelist(net2)

edge3 <- unique(edge3)

net3 <- network(edge3, 
                       directed = TRUE, 
                       vertices = network.size(net2))
                          
net3 %v% "organization_label" <- net2 %v% "organization_label"

summary(net3)

model3 <- ergm(
  net3 ~ edges +
    gwidegree(0.5, fixed=TRUE) +
    gwesp(0.5, fixed=TRUE, type="OTP"),
  control = control.ergm(MCMLE.maxit=60)
) # did not run

gtrans(net3)      

summary(net3 ~ gwesp(0.5, fixed=TRUE, type="OTP"))  # outgoing two-path
summary(net3 ~ gwesp(0.5, fixed=TRUE, type="ITP"))  # incoming two-path  
summary(net3 ~ gwesp(0.5, fixed=TRUE, type="OSP"))  # outgoing shared partner
summary(net3 ~ gwesp(0.5, fixed=TRUE, type="ISP"))  # incoming shared partner
# GWESP OTP has good value but still could not get model to run

model3 <- ergm(
  net3 ~ edges +
    gwidegree(0.5, fixed=TRUE) +
    mutual,
  control = control.ergm(
    MCMLE.maxit = 60,
    MCMC.interval = 20000,  # increase from default
    MCMC.samplesize = 5000  # larger sample
  )
) # this one ran, but needed addition of increased interval and sample for better numbers 

mcmc.diagnostics(model3)










