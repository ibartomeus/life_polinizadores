
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


ejea_tale_all <- qpcR:::cbind.na(canta_table_poll, canta_table_plants)
ejea_tale_all <- sapply(ejea_tale_all, as.character)
ejea_tale_all[is.na(ejea_tale_all)] <- " "

ejea_tale_all %>%
  kbl(col.names = c("\\normalfont{Pollinator species}", "Frequency","\\normalfont{Plants species}", "Frequency")) %>%
  kable_styling(bootstrap_options = C("striped","repeat_header"), full_width = F, position = "left") %>%
  column_spec(1, italic = T)
