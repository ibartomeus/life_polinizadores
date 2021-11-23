
#In this script the data is processed and prepared for analysis and visualization
#First,load libraries
library(dplyr)
library(reshape2)

#Read data
data <- read.csv("life_polinizadores.csv")

#check structure of the data
str(data)

#create separate datasets for the two different locations
canta <- data %>% filter(site_id=="cantavieja")
ejea <- data %>% filter(site_id=="ejea caballeros")

#select for now plant and pollinator species to create plant-pollinator networks from transects
canta_species <- canta %>% select(c("plants", "pollinators"))
ejea_species <- ejea %>% select(c("plants", "pollinators"))

#Now aggregate pollinator visits per plant species
canta_aggreagted <- canta_species %>% count(plants, pollinators, sort = TRUE)
ejea_aggreagted <-  ejea_species %>% count(plants, pollinators, sort = TRUE)

#Now convert to a matrix with the help of acast function from reshape library
canta_matrix <- acast(canta_aggreagted, plants~pollinators, value.var="n")
ejea_matrix <- acast(ejea_aggreagted, plants~pollinators, value.var="n")

#Convert NA's to zeros
canta_matrix[is.na(canta_matrix)] <- 0
ejea_matrix[is.na(ejea_matrix)] <- 0

#Save networks
saveRDS(canta_matrix, "Data/canta_matrix.rds")
saveRDS(ejea_matrix, "Data/ejea_matrix.rds")
