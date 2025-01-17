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
select(locality, site_id, year, transect,  plants, pollinators) %>%
filter(!str_detect(pollinators, " sp")) %>%
filter(!str_detect(plants, " sp")) %>%
mutate(locality = if_else(locality== "cantavieja", "Cantavieja", "Ejea"))

#Calculate degree distributions for pollinators-----
degree_dist = main_cols %>%
group_by(site_id, locality, year, pollinators) %>%
summarise(degree = n_distinct(plants))


#Compare statistical differences of the distribution
d_cantavieja = degree_dist %>%
filter(locality == "Cantavieja")
d_ejea = degree_dist %>%
filter(locality == "Ejea")

kruskal.test(degree ~ year, data = d_cantavieja)
kruskal.test(degree ~ year, data = d_ejea)


#Plot degree distributions
degree_dist %>%
ggplot(aes(degree), group = year, fill = year) +
geom_histogram(aes(fill=year),bins=50, binwidth = 1.2)  +
facet_wrap(~locality)

p1 = degree_dist %>%
  ggplot(aes(degree, fill = factor(year),colour = factor(year), group = year, alpha= factor(year))) +
  geom_histogram(bins = 50, binwidth = 0.8, position = "identity") +
  geom_density(aes(y = after_stat(count) * 0.6,colour = factor(year), group = year), alpha = .3) +
  facet_wrap(~locality) +
  theme_minimal() +
  labs(title = "Pollinators degree distribution",
       x = "Degree",
       y = "Frequency") +
  scale_fill_manual(name = "Year",values = c("2021" = "black", "2024" = "tomato4")) +
  scale_color_manual(name = "Year",values = c("2021" = "black", "2024" = "tomato4")) +
  scale_alpha_manual(name = "Year",values = c("2021" = 0.8, "2024" = 0.2))  # Custom alphas for 'year'

#Calculate degree distributions for plants-----
degree_dist = main_cols %>%
group_by(site_id, locality, year, plants) %>%
summarise(degree = n_distinct(pollinators))

#Compare statistical differences of the distribution
d_cantavieja = degree_dist %>%
filter(locality == "Cantavieja")
d_ejea = degree_dist %>%
filter(locality == "Ejea")

kruskal.test(degree ~ year, data = d_cantavieja)
kruskal.test(degree ~ year, data = d_ejea)


#Plot degree distributions
degree_dist %>%
ggplot(aes(degree), group = year, fill = year) +
geom_histogram(aes(fill=year),bins=50, binwidth = 1.2)  +
facet_wrap(~locality)

p2 = degree_dist %>%
  ggplot(aes(degree, fill = factor(year),colour = factor(year), group = year, alpha= factor(year))) +
  geom_histogram(bins = 50, binwidth = 0.8, position = "identity") +
  geom_density(aes(y = after_stat(count) * 1.5,colour = factor(year), group = year), alpha = .3) +
  facet_wrap(~locality) +
  theme_minimal() +
  labs(title = "Plant degree distribution",
       x = "Degree",
       y = "Frequency") +
  scale_fill_manual(name = "Year",values = c("2021" = "black", "2024" = "tomato4")) +
  scale_color_manual(name = "Year",values = c("2021" = "black", "2024" = "tomato4")) +
  scale_alpha_manual(name = "Year",values = c("2021" = 0.8, "2024" = 0.2))  # Custom alphas for 'year'
p2


library(patchwork)

p1/p2




