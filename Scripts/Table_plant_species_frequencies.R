
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
canta_table <- canta_species %>%
  group_by(plants) %>%
  summarise(no_rows = length(plants)) %>% arrange(desc(no_rows))

ejea_table <- ejea_species %>%
  group_by(plants) %>%
  summarise(no_rows = length(plants)) %>% arrange(desc(no_rows))

#Set new colnames
colnames(canta_table) <- c("Plants species", "Frequency")
colnames(ejea_table) <- c("Plants species", "Frequency")


#Plot table
canta_table %>%
  kbl("latex",col.names = c("\\normalfont{Plants species}", "Frequency")) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left") %>%
  column_spec(1, italic = T)

ejea_table %>%
  kbl("latex",col.names = c("\\normalfont{Plants species}", "Frequency")) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left") %>%
  column_spec(1, italic = T)

