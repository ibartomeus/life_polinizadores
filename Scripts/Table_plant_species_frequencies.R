
#Prepare table for appendices
library(dplyr)
library(reshape2)
library(kableExtra)

#Read data
data <- read.csv("Data/life_polinizadores.csv")

#check structure of the data
str(data)

#create separate datasets for the two different locations
canta <- data %>% filter(site_id=="cantavieja")
ejea <- data %>% filter(site_id=="ejea caballeros")

#select for now plant and pollinator species to create plant-pollinator networks from transects
canta_species <- canta %>% select(c("plants", "pollinators"))
ejea_species <- ejea %>% select(c("plants", "pollinators"))


#Aggreagte the number of plant occurrences
canta_table_plants <- canta_species %>%
  group_by(plants) %>%
  summarise(no_rows = length(plants)) %>% arrange(desc(no_rows))

ejea_table_plants <- ejea_species %>%
  group_by(plants) %>%
  summarise(no_rows = length(plants)) %>% arrange(desc(no_rows))

#Set new colnames
colnames(canta_table_plants) <- c("Plants species", "Frequency")
colnames(ejea_table_plants) <- c("Plants species", "Frequency")

################
#Now pollinators
################

#Aggreagte the number of pollinator occurrences
canta_table_poll <- canta_species %>%
  group_by(pollinators) %>%
  summarise(no_rows = length(pollinators)) %>% arrange(desc(no_rows))

ejea_table_poll <- ejea_species %>%
  group_by(pollinators) %>%
  summarise(no_rows = length(pollinators)) %>% arrange(desc(no_rows))

colnames(canta_table_poll) <- c("Pollinator species", "Frequency")
colnames(ejea_table_poll) <- c("Pollinator species", "Frequency")

#bind plantas and pollinators per location
canta_table_all <- qpcR:::cbind.na(canta_table_poll, canta_table_plants)
canta_table_all <- sapply(canta_table_all, as.character)
canta_table_all[is.na(canta_table_all)] <- " "

ejea_table_all <- qpcR:::cbind.na(ejea_table_poll, ejea_table_plants)
ejea_table_all <- sapply(ejea_table_all, as.character)
ejea_table_all[is.na(ejea_table_all)] <- " "


#Write csv to export to word
write.csv(canta_table_all, "Data/canta_table_all.csv")
write.csv(ejea_tale_all, "Data/ejea_tale_all.csv")

