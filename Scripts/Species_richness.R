#Read data from 2021 and 2024
#First,load libraries
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
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

#Create a site_id label with year
#and select only cols of interest
main_cols = all_data %>%
mutate(locality = site_id) %>%
mutate(date = paste(day,month,year,transect, sep = "_")) %>%
mutate(site_id = paste0(site_id, "_" ,year)) %>%
mutate(site_id = str_replace_all(site_id, " ", "_")) %>%
select(locality, site_id, date, transect,  plants, pollinators)

#Get number of species of plants and pollinators
#per location and year (site_id has already year on it)
n_spp = main_cols %>%
group_by(site_id) %>%
summarise(n_pollinators = n_distinct(pollinators),
          n_plants = n_distinct(plants))

n_spp$site = c("cantavieja", "cantavieja","ejea", "ejea")
n_spp$Year = c("2021", "2024","2021", "2024")

p1 = n_spp %>%
ggplot(aes(site, n_pollinators, fill = Year), group = Year) +
geom_col(position = "dodge") +
scale_fill_manual(values = c("tan2", "azure3")) +
theme_bw() +
coord_cartesian(expand = FALSE) +
xlab(NULL) +
ylab("Species") +
ggtitle("Pollinators")


p2 = n_spp %>%
ggplot(aes(site, n_plants, fill = Year), group = Year) +
geom_col(position = "dodge") +
scale_fill_manual(values = c("tan2", "azure3")) +
theme_bw() +
coord_cartesian(expand = FALSE) +
xlab(NULL) +
ylab("Species") +
ggtitle("Plants")

library(cowplot)
plot_grid(p1, p2)


