data_2021 = read.csv("Data/life_polinizadores_2021.csv")
#Create table with spp
pollinators_2021 = data_2021 %>%
group_by(site_id, year) %>%
summarise(Pollinators = n_distinct(pollinators))

plants_2021 = data_2021 %>%
group_by(site_id, year) %>%
summarise(Plants = n_distinct(plants))

spp_2021 = left_join(pollinators_2021, plants_2021)

data_2024 = read.csv("Data/life_polinizadores_2024.csv")
#Create table with spp
pollinators_2024 = data_2024 %>%
group_by(site_id, year) %>%
summarise(Pollinators = n_distinct(pollinators))

plants_2024 = data_2024 %>%
group_by(site_id, year) %>%
summarise(Plants = n_distinct(plants))

spp_2024 = left_join(pollinators_2024, plants_2024)

#Now bind both years
spp_year = bind_rows(spp_2021,spp_2024)

table_1 = spp_year %>%
  rename(Location = site_id) %>%
  rename(Year = year) %>%
  arrange(Location) %>%
  mutate(Location = recode_factor(Location, "cantavieja" = "Cantavieja")) %>%
  mutate(Location = recode_factor(Location, "ejea caballeros" = "Ejea"))

#Add metrics
matrices_list = readRDS("Data/matrices_list.rds")

#Calculate network descriptors
library(bipartite)
library(purrr)
library(tidyr)
#Compute specialization
H2_results = map(matrices_list, H2fun, H2_integer=TRUE)
network_h2 = bind_rows(H2_results, .id = "Site_id")
network_h2 = network_h2 %>%
select(Site_id, H2)
#Compute connectance
connectance_results = map(matrices_list, networklevel, index="connectance")
network_connectance = bind_rows(connectance_results, .id = "Site_id")
#Bind metric data
metrics = left_join(network_h2,network_connectance)
#Separate Site_id again for plotting
connectance_nestedness = metrics %>%
separate(Site_id, c("site", "Year"), "_") %>%
mutate(site = str_to_title(site)) %>%
rename(Connectance = connectance) %>%
rename(Location =site) %>%
mutate(Year =as.integer(Year))

table_1 = left_join(table_1, connectance_nestedness)

#Prepare robustness data to load on markdown
long_plants = readRDS("Data/robustness_plants.rds")
long_pollinators = readRDS("Data/robustness_pollinators.rds")
long_network = readRDS("Data/robustness_network.rds")

table_1$`Robustness plant loss` = long_plants$Robustness
table_1$`Robustness poll loss` = long_pollinators$Robustness
table_1$`Network robustness` = long_network$Robustness

#Prepare nestedness to load data on markdown
nestedness_tibble = readRDS("Data/nestedness.rds")

table_1$Nestedness = nestedness_tibble$temperature

