#Read data from 2021 and 2024
#First,load libraries
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(tibble)
#Read data
data_2021 = read.csv("Data/life_polinizadores_2021.csv")
data_2024 = read.csv("Data/life_polinizadores_2024.csv")
#Combine data
all_data = bind_rows(data_2021, data_2024)
#Rename ejea caballeros to only ejea
all_data = all_data %>%
mutate(site_id = if_else(site_id == "ejea caballeros",
                "ejea", site_id))

#Delete leading and trailing spaces in spp names
all_data = all_data %>%
mutate(pollinators = trimws(pollinators)) %>%
mutate(plants = trimws(plants))

main_cols = all_data %>%
mutate(site_id = paste0(site_id, "_" ,year)) %>%
mutate(site_id = str_replace_all(site_id, " ", "_")) %>%
select(site_id, pollinators, plants)

#Split the data frame based on site_id
long_format_list = split(main_cols, main_cols$site_id)

#Function to convert to matrix and sum interactions of same plants and polls
sum_interactions = function(data) {
  data %>%
  group_by(plants, pollinators) %>%
  summarise(Interactions = n()) %>%
  #Comment next line if you want quantitative matrices
  #mutate(Interactions = if_else(Interactions > 0, 1, 0)) %>%
  pivot_wider(names_from = pollinators,
                      values_from = Interactions,
                      values_fill = 0) %>%
  column_to_rownames("plants") %>%
  as.matrix()
}

#Apply function to each network within the list
matrices_list = map(long_format_list, sum_interactions)

saveRDS(matrices_list, "Data/matrices_list.rds")

