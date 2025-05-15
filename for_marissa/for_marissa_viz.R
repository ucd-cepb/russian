# loaded packages
library(readr)
library(tidyverse)
library(igraph)
library(network)
library(sna)
library(dplyr)

all.edge <- read_csv("~/Desktop/R_stuff/kelp_restoration/data_files/for_marissa - edgelist2.csv")
all.node <- read_csv("~/Desktop/R_stuff/kelp_restoration/data_files/for_marissa - nodelist.csv")

# organization affiliation to organizations mentioned, no loops
edgelist1 <- all.edge %>% 
  select(c(from_2, to_1))

edgelist1 <- edgelist1[edgelist1$from_2 != edgelist1$to_1,]

net1 <- graph_from_data_frame(d = edgelist1, directed = TRUE)

plot(net1, layout=layout_with_kk,
     vertex.label.cex = .8,
     vertex.label.family = "Verdana",
     vertex.size = 2,
     edge.arrow.size = .7,
     edge.arrow.width = .5,
)

l <- layout_on_sphere(net1)
plot(net1, layout = l,
     vertex.label.cex = .8,
     vertex.label.family = "Verdana",
     vertex.size = 1,
     edge.arrow.size = .7,
     edge.arrow.width = .5,
     edge.width = .9,
     )

# added weight
edgelist2 <- edgelist1 %>%
  group_by(from_2, to_1) %>%
  summarise(weight = n(), .groups = 'drop') %>%
  arrange(desc(weight))

net2 <- graph_from_data_frame(edgelist2, directed = FALSE)

plot(net2, edge.width = E(net2)$weight,
     vertex.label.cex = .8,
     vertex.size = 2,
     edge.arrow.size = .7,
     edge.arrow.width = .5
     )

# only show orgs with weight > 2
heavy.edge <- E(net2)[weight > 2]
nodes_to_label <- unique(c(ends(net2, heavy.edge)))

V(net2)$name <- ifelse(V(net2)$name %in% nodes_to_label, V(net2)$name, "")

l2 <- layout_on_sphere(net2)
plot(net2, layout = l2,
     edge.width = E(net2)$weight, 
     vertex.label.cex = .8,
     vertex.size = 2,
     edge.arrow.size = .7,
     edge.arrow.width = .5
     )

# did frequency for this one
edgelist5 <- edgelist1 %>% 
  group_by(to_1) %>%
  add_count(name = "frequency")

net5 <- graph_from_data_frame(edgelist5, directed = FALSE)

big.edge <- E(net5)[E(net5)$frequency > 9]
nodes_to_label2 <- unique(c(ends(net5, big.edge)))

V(net5)$name <- ifelse(V(net5)$name %in% nodes_to_label2, V(net5)$name, "")

l5 <- layout_on_sphere(net5)
plot(net5, layout = l5,
     #edge.width = E(net2)$weight, 
     vertex.label.font = 2,
     vertex.label.family = "Verdana",
     vertex.label.cex = .7,
     vertex.label.dist = 1.5,
     vertex.size = 2,
     edge.arrow.size = .7,
     edge.arrow.width = .5
)


# organization affiliation type to organizations mentioned type, no loops
edgelist3 <- all.edge %>% 
  select(c(from_3, to_2))

edgelist3 <- edgelist3[edgelist3$from_3 != edgelist3$to_2,]

net3 <- graph_from_data_frame(d = edgelist3, directed = FALSE)

plot(net3, layout=layout_with_kk,
     vertex.label.cex = 1,
     vertex.size = 2,
     edge.arrow.size = .5,
     edge.arrow.width = 1,
     )

# added weight
edgelist4 <- edgelist3 %>%
  group_by(from_3, to_2) %>%
  summarise(weight = n(), .groups = 'drop') %>%
  arrange(desc(weight))

net4 <- graph_from_data_frame(edgelist4, directed = FALSE)

plot(net4, edge.width = E(net4)$weight,
     vertex.size = 4,
     vertex.label.cex = 1,
     edge.arrow.size = .5,
)
