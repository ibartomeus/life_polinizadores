
#Analysis

############################################################
#Summary of this script

#1) Calculate nestedness and modularity

#Resources used to conduct this analysis:
#https://ibartomeus.github.io/hab-sp_ntw/demo.html
############################################################

#Load libraries
library(bipartite)

#Load networks
canta_matrix <- readRDS("Data/canta_matrix.rds")
ejea_matrix <- readRDS("Data/ejea_matrix.rds")

###########
#LOCATION 1
###########

#####################
#Calculate nestedness
#####################

(obs_canta <- networklevel(web = canta_matrix, index = "weighted NODF"))

#To know the meaning of our nestedness value we have to compare it with a randomized version
#of our plant-pollinator network
#Create 1000 random versions of our network
nm_canta <- nullmodel(web = canta_matrix, N=1000, method="r2d") #method 2 for quantitative networks

#Prepare data for plotting
null_canta <- unlist(sapply(nm_canta, networklevel, index="weighted NODF"))

#Plot the distribution of nestedness of the 1000 random networks
plot(density(null_canta), xlim=c(min(obs_canta, min(null_canta)), max(obs_canta, max(null_canta))),
     main="Comparison of observed network with null model")

#Add our value of nedtedness to the plot
abline(v=obs_canta, col="red", lwd=2)

#Calculate p-value
praw_canta <- sum(null_canta>obs_canta) / length(null_canta)

#####################
#Calculate modularity
#####################

res_canta <- computeModules(canta_matrix)

#Plot modules
plotModuleWeb(res_canta, displayAlabels = T)

#Calculate modules for each random network
modules.nulls_canta <- sapply(nm_canta, computeModules)
#Calculate p-value
like.nulls_canta <- sapply(modules.nulls_canta, function(x) x@likelihood)
praw_canta <- sum(like.nulls_canta > res_canta@likelihood) / length(like.nulls_canta)

#Save modularity variables because it takes quite a bit to run
saveRDS(res_canta, "Data/res_canta.rds")
saveRDS(modules.nulls_canta, "Data/modules.nulls_canta.rds")
saveRDS(like.nulls_canta, "Data/like.nulls_canta.rds")
saveRDS(praw_canta, "Data/praw_canta.rds")


############
#LOCATION 2
############

#####################
#Calculate nestedness
#####################

(obs_ejea <- networklevel(web = ejea_matrix, index = "weighted NODF"))

#To know the meaning of our nestedness value we have to compare it with a randomized version
#of our plant-pollinator network
#Create 1000 random versions of our network
nm_ejea <- nullmodel(web = ejea_matrix, N=1000, method="r2d") #method 2 for quantitative networks

#Prepare data for plotting
null_ejea <- unlist(sapply(nm_ejea, networklevel, index="weighted NODF"))

#Plot the distribution of nestedness of the 1000 random networks
plot(density(null_ejea), xlim=c(min(obs_ejea, min(null_ejea)), max(obs_ejea, max(null_ejea))),
     main="Comparison of observed network with null model")

#Add our value of nedtedness to the plot
abline(v=obs_ejea, col="red", lwd=2)

#Calculate p-value
praw_ejea <- sum(null_ejea>obs_ejea) / length(null_ejea)

#####################
#Calculate modularity
#####################
res_ejea <- computeModules(ejea_matrix)

#Plot modules
plotModuleWeb(res_ejea, displayAlabels = T)

#Calculate modules for each random network
modules.nulls_ejea <- sapply(nm_ejea, computeModules)
#Calculate p-value
like.nulls_ejea <- sapply(modules.nulls_ejea, function(x) x@likelihood)
praw_ejea <- sum(like.nulls_ejea > res_ejea@likelihood) / length(like.nulls_ejea)

#Save modularity variables because it takes quite a bit to run
saveRDS(res_ejea, "Data/res_ejea")
saveRDS(modules.nulls_ejea, "Data/modules.nulls_ejea.rds")
saveRDS(like.nulls_ejea, "Data/like.nulls_ejea.rds")
saveRDS(praw_ejea, "Data/praw_ejea.rds")

