library(dplyr)
library(reshape2)
data <- read.csv("Data/life_polinizadores.csv")

#check structure of the data
#str(data)

#create separate datasets for the two different locations
canta <- data %>% filter(site_id=="cantavieja")
ejea <- data %>% filter(site_id=="ejea caballeros")

#select for now plant and pollinator species to create plant-pollinator networks from transects
canta_species <- canta %>% select(c("plants", "pollinators"))
ejea_species <- ejea %>% select(c("plants", "pollinators"))

#Now aggregate pollinator visits per plant species
canta_aggreagted <- canta_species %>% count(plants, pollinators, sort = TRUE)
ejea_aggreagted <-  ejea_species %>% count(plants, pollinators, sort = TRUE)

#prepare the data for visNetwork
#cantavieja
links <- canta_aggreagted[,c("plants", "pollinators", "n")]
colnames(links)[3] <- "weight"
node1 <-  data.frame(node = unique(canta_aggreagted[,c("plants")]),
                     attribute = NA,
                     type = "plants")
node2 <-  data.frame(node = unique(canta_aggreagted[,c("pollinators")]),
                     attribute = NA,
                     type = "pollinators")
nodes <- rbind(node1, node2)
library(igraph)
net <- graph_from_data_frame(d=links,
                             vertices=nodes, directed=F)
# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")

library('visNetwork')
colnames(nodes)[1] <- "id"
nodes$shape <- "dot"
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$attribute <- as.character(nodes$attribute)
nodes$title <- nodes$id # Text on click
nodes$label <- as.character(nodes$type) # Node label
nodes$size <- deg*5 # Node size
nodes$borderWidth <- 2 # Node border width
nodes$color.background <- ifelse(nodes$label == "plants", "darkgreen", "darkorange")
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"
links$width <- links$weight*5 # line width
links$color <- "gray"    # line color
#links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
links$smooth <- TRUE    # should the edges be curved?
links$shadow <- FALSE    # edge shadow
colnames(links)[1:2] <- c("from", "to")
visNetwork(nodes, links)


#Ejea
links <- ejea_aggreagted[,c("plants", "pollinators", "n")]
colnames(links)[3] <- "weight"
node1 <-  data.frame(node = unique(ejea_aggreagted[,c("plants")]),
                     attribute = NA,
                     type = "plants")
node2 <-  data.frame(node = unique(ejea_aggreagted[,c("pollinators")]),
                     attribute = NA,
                     type = "pollinators")
nodes <- rbind(node1, node2)
library(igraph)
net <- graph_from_data_frame(d=links,
                             vertices=nodes, directed=F)
# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")

library('visNetwork')
colnames(nodes)[1] <- "id"
nodes$shape <- "dot"
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$attribute <- as.character(nodes$attribute)
nodes$title <- nodes$id # Text on click
nodes$label <- as.character(nodes$type) # Node label
nodes$size <- deg*5 # Node size
nodes$borderWidth <- 2 # Node border width
nodes$color.background <- ifelse(nodes$label == "plants", "darkgreen", "darkorange")
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"
links$width <- links$weight*5 # line width
links$color <- "gray"    # line color
#links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
links$smooth <- TRUE    # should the edges be curved?
links$shadow <- FALSE    # edge shadow
colnames(links)[1:2] <- c("from", "to")
visNetwork(nodes, links)

