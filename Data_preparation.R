
#In this script the data is processed and prepared for analysis and visualization
#First,load libraries
library(dplyr)
library(reshape2)

#Read data
data <- read.csv("life_polinizadores.csv")

#check structure of the data
str(data)

#create separate datasets for the two different locations
ejea <- data %>% filter(site_id=="ejea caballeros")
canta <- data %>% filter(site_id=="cantavieja")

#select for now plant and pollinator species to create plant-pollinator networks from transects
ejea_species <- ejea %>% select(c("plants", "pollinators"))
canta_species <- canta %>% select(c("plants", "pollinators"))

#Now aggregate pollinator visits per plant species
ejea_aggreagted <-  ejea_species %>% count(plants, pollinators, sort = TRUE)
canta_aggreagted <- canta_species %>% count(plants, pollinators, sort = TRUE)

#Now convert to a matrix with the help of acast function from reshape library
ejea_matrix <- acast(ejea_aggreagted, plants~pollinators, value.var="n")
canta_matrix <- acast(canta_aggreagted, plants~pollinators, value.var="n")

#Convert NA's to zeros
ejea_matrix[is.na(ejea_matrix)] <- 0
canta_matrix[is.na(canta_matrix)] <- 0


#Visualize the networks now

library(bipartite)
plotweb(sortweb(ejea_matrix, sort.order="dec"), method="normal", text.rot=90, col.low = "darkolivegreen1", col.high = "darkorange",
        col.interaction="gray75",bor.col.interaction ="NA", labsize =.55)


(obs <- networklevel(web = canta_matrix, index = "weighted NODF"))
nm <- nullmodel(web = canta_matrix, N=1000, method="r2d")
null <- unlist(sapply(nm, networklevel, index="weighted NODF"))
plot(density(null), xlim=c(min(obs, min(null)), max(obs, max(null))),
     main="comparison of observed with null model Patefield")
abline(v=obs, col="red", lwd=2)

praw <- sum(null>obs) / length(null)

res <- computeModules(canta_matrix)
plotModuleWeb(res, displayAlabels = T)

modules.nulls <- sapply(nm, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
praw <- sum(like.nulls > res@likelihood) / length(like.nulls)
