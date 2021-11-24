data <- read.csv("Data/life_polinizadores.csv")

#check structure of the data
#str(data)

#create separate datasets for the two different locations
canta <- data %>% filter(site_id=="cantavieja")
ejea <- data %>% filter(site_id=="ejea caballeros")


#prepare the data for igraph
links <- canta[,c("plants", "pollinators")]
node1 <-  data.frame(node = unique(canta[,c("plants")]),
                     attribute = NA,
                     type = "plants")
node2 <-  data.frame(node = unique(canta[,c("pollinators")]),
                     attribute = NA,
                     type = "pollinators")
nodes <- rbind(node1, node2)

library('visNetwork')
colnames(nodes)[1] <- "id"
nodes$shape <- "dot"
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$attribute <- as.character(nodes$attribute)
#nodes$attribute[10:23] <-  as.character(nodes$id[10:23])
nodes$title <- nodes$id # Text on click
nodes$label <- nodes$type # Node label
nodes$size <- 40 # Node size
nodes$borderWidth <- 2 # Node border width
nodes$color.background <- ifelse(nodes$label == "plants", "red", "blue")
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"
links$width <- links$weight # line width
links$color <- "gray"    # line color
#links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
links$smooth <- TRUE    # should the edges be curved?
links$shadow <- FALSE    # edge shadow
colnames(links)[1:2] <- c("from", "to")
visNetwork(nodes, links)

