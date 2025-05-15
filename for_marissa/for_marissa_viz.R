
library(tidyverse)
library(statnet)
library(ggnetwork)
all.edge <- read_csv("for_marissa/for_marissa - edgelist2.csv")
all.node <- read_csv("for_marissa/for_marissa - nodelist.csv")

#### rname these so they are clear about what is what
all.edge <- all.edge |> rename(from_id=from_1,from_org = from_2,from_label = from_3,to_org = to_1,to_label = to_2)
### defaults are false, but we have each of these so want to be clear since we might try them
### for simplicity we'll collapse to orgs here
net <- network.initialize(length(unique(all.node$organization)),loops = F,multiple = T,hyper = F)
network.vertex.names(net) <- sort(unique(all.node$organization))
set.vertex.attribute(net,'org_label',all.node$organization_label[match(network.vertex.names(net),all.node$organization)])

# group by org-org-type
grouped_edges <- all.edge |> group_by(from_org,to_org,type) |> filter(from_org != to_org) |> summarise(tie_count = n())

edge_mat_index = cbind(from = match(grouped_edges$from_org,net %v% 'vertex.names'),
                 to = match(grouped_edges$to_org,net %v% 'vertex.names'),
                 type = grouped_edges$type,
                 count = grouped_edges$tie_count)

# add edges by matching vertices
### collab edges
add.edges(net,
         tail = edge_mat_index[,'from'],
         head = edge_mat_index[,'to'],
         names.eval = rep(list(list('type', 'count')), nrow(edge_mat_index)),
         vals.eval = lapply(1:nrow(edge_mat_index), function(r) { as.list(edge_mat_index[r, c('type', 'count')]) })
)

library(ggthemes)
layout_coords = ggnetwork(net)
top_between <- {net %v% 'vertex.names'}[rank(-betweenness(net))<=10]

brokrs <- brokerage(net,net %v% 'org_label')
brokrs_rank <- rank(-rowSums(brokrs$z.nli[,c('b_O')]))
top_brokrs <- names(sort(brokrs_rank)[1:10])

base_plot <- ggplot(mapping = aes(x = x,y = y,yend = yend, xend = xend)) + 
  theme_void() + 
  scale_color_tableau()

gg_communication = base_plot + geom_nodes(data = layout_coords,size = 0.25) +
  geom_edges(data = layout_coords[layout_coords$type=='communication',],alpha = 0.5) + 
  geom_nodelabel_repel(box.padding = 0.1,label.padding = 0.1,
          data = layout_coords[layout_coords$vertex.names %in% top_brokrs,],
          aes(label = vertex.names),
          size = 1.5,max.overlaps = 10) +
  scale_y_continuous(limits = c(NA,0.8))+
  scale_x_continuous(limits = c(NA,0.8))+
  ggtitle('Communication ties and top 10 information brokers') +
  labs(caption = 'brokerage = A-B-C')
  
gg_collaboration = base_plot + geom_nodes(data = layout_coords,size = 0.25) +
  geom_edges(data = layout_coords[layout_coords$type=='collaboration',],alpha = 0.5) + 
  geom_nodelabel_repel(box.padding = 0.1,label.padding = 0.1,
                       data = layout_coords[layout_coords$vertex.names %in% top_between,],
                       aes(label = vertex.names),
                       size = 1.5,max.overlaps = 10) +
  scale_y_continuous(limits = c(NA,0.8))+
  scale_x_continuous(limits = c(NA,0.8))+
  ggtitle('Collaboration ties and most central collaborators') 

gg_central <- base_plot + geom_nodes(data = layout_coords,size = 0.25) +
  geom_edges(data = layout_coords,aes(color = type),alpha = 0.5,curvature = 0.05) + 
  #geom_nodelabel_repel(box.padding = 0.1,label.padding = 0.1,
   #                    data = layout_coords,
  #                     aes(label = vertex.names),
  #                     size = 1.5,max.overlaps = 10) +
  geom_nodes(data = layout_coords[layout_coords$vertex.names %in% top_between,],
                       size = 2) + 
  geom_nodelabel_repel(box.padding = 0.1,label.padding = 0.1,col = 'black',
                       data = layout_coords[layout_coords$vertex.names %in% top_between,],
                       aes(label = vertex.names),
                       size = 2,max.overlaps = 10) +
  scale_y_continuous(limits = c(NA,0.8))+
  scale_x_continuous(limits = c(NA,0.8))+
  theme(legend.position = 'bottom',legend.position.inside = c(0.8,0.2))+
  ggtitle('Reported ties and most central actors') 


ggsave(plot = gg_central,filename= 'for_marissa/centrality_plot.png',dpi = 600,width = 7,height = 7,units = 'in')

base_plot + geom_nodes(data = layout_coords,size = 0.25) +
  geom_edges(data = layout_coords,aes(color = type),alpha = 0.5,curvature = 0.05) + 
  #geom_nodelabel_repel(box.padding = 0.1,label.padding = 0.1,
  #                    data = layout_coords,
  #                     aes(label = vertex.names),
  #                     size = 1.5,max.overlaps = 10) +
  geom_nodelabel_repel(box.padding = 0.1,label.padding = 0.1,col = 'black',
                       data = layout_coords[layout_coords$vertex.names %in% top_brokrs,],
                       aes(label = vertex.names),
                       size = 2,max.overlaps = 10) +
  scale_y_continuous(limits = c(NA,0.8))+
  scale_x_continuous(limits = c(NA,0.8))+
  theme(legend.position = 'bottom',legend.position.inside = c(0.8,0.2))+
  ggtitle('Reported ties and liaison brokers') + 
  labs(caption = 'liaison brokers bridge org. types, e.g., A-B-C')




setdiff(top_between,top_brokrs)

geom_nodelabel(box.padding = 0.1,data = layout_coords[layout_coords$vertex.names %in% brokrs,],col = 'red',max.overlaps = Inf)
  
geom_edges(data = layout_coords[layout_coords$x != layout_coords$xend,],aes(color = type),alpha = 0.5) +
  
  

theme(legend.position = 'inside',legend.position.inside = c(0.75,0.25))

base_plot + geom_nodes(data = layout_coords[layout_coords$x != layout_coords$xend,])

match(grouped_edges$from_org[grouped_edges$type == 'collaboration'],net %v% 'vertex.names')
match(grouped_edges$to_org[grouped_edges$type == 'collaboration'],net %v% 'vertex.names')

match(grouped_edges$from_org[grouped_edges$type == 'collaboration'],net %v% 'vertex.names')


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


