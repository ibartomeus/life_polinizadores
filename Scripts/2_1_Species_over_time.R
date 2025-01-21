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

#Create a plot of pollinators and plants per date
main_cols = all_data %>%
select(c(site_id,
         day,
         month,
         year,
         plants,
         pollinators)) %>%
mutate(date = as.Date(paste(year, month, day, sep = "-"),
  format = "%Y-%m-%d"))

#Count plant and pollinator species by date and site
spp_date = main_cols %>%
group_by(date, site_id, year) %>%
summarise(plants = n_distinct(plants),
          pollinators = n_distinct(pollinators))

#Subset data by location
library(tidyr)
spp_date_ejea_2021 = spp_date %>%
filter(site_id == "ejea" & year == "2021") %>%
  pivot_longer(cols = c(pollinators, plants),
               names_to = "Species",
               values_to = "count")

spp_date_ejea_2024 = spp_date %>%
filter(site_id == "ejea" & year == "2024") %>%
  pivot_longer(cols = c(pollinators, plants),
               names_to = "Species",
               values_to = "count")

#Plot data by location
library(scales)
p1 = spp_date_ejea_2021 %>%
ggplot(aes(date, count, fill = Species)) +
geom_col(position = "dodge",width=3) +
scale_fill_manual(values = c("darkgreen", "orange3")) +
coord_cartesian(expand = FALSE) +
ylab("Species") +
ylim(0,55) +
theme_bw() +
xlab(NULL) +
ggtitle("b) Ejea de los caballeros") +
theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
scale_x_date(
    limits = as.Date(c("2021-03-15", "2021-07-25")),
    breaks = as.Date(c("2021-03-20","2021-04-20", "2021-05-20",
                       "2021-06-20", "2021-07-20")),
    labels = format(as.Date(c("2021-03-20","2021-04-20", "2021-05-20",
                       "2021-06-20", "2021-07-20")),  "%d %b %Y"))

#Plot data by location
p2 = spp_date_ejea_2024 %>%
ggplot(aes(date, count, fill = Species)) +
geom_col(position = "dodge",width=3) +
scale_fill_manual(values = c("darkgreen", "orange3")) +
coord_cartesian(expand = FALSE) +
ylab("Species") +
ylim(0,55) +
xlab("Date") +
theme_bw()+
scale_x_date(
    limits = as.Date(c("2024-03-15", "2024-07-25")),
    breaks = as.Date(c("2024-03-20","2024-04-20", "2024-05-20",
                       "2024-06-20", "2024-07-20")),
    labels = format(as.Date(c("2024-03-20","2024-04-20", "2024-05-20",
                       "2024-06-20", "2024-07-20")),  "%d %b %Y"))

#Repeat process for cantavieja
spp_date_canta_2021 = spp_date %>%
filter(site_id == "cantavieja" & year == "2021") %>%
  pivot_longer(cols = c(pollinators, plants),
               names_to = "Species",
               values_to = "count")

spp_date_canta_2024 = spp_date %>%
filter(site_id == "cantavieja" & year == "2024") %>%
  pivot_longer(cols = c(pollinators, plants),
               names_to = "Species",
               values_to = "count")

#Plot data by location
library(scales)

p3 = spp_date_canta_2021 %>%
ggplot(aes(date, count, fill = Species)) +
geom_col(position = "dodge",width=3) +
scale_fill_manual(values = c("darkgreen", "orange3")) +
coord_cartesian(expand = FALSE) +
ylab("Species") +
ylim(0,55) +
theme_bw() +
xlab(NULL) +
ggtitle("a) Cantavieja") +
theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
scale_x_date(
    limits = as.Date(c("2021-05-15", "2021-08-25")),
    breaks = as.Date(c("2021-05-20",
                       "2021-06-20", "2021-07-20", "2021-08-20")),
    labels = format(as.Date(c("2021-05-20",
                       "2021-06-20", "2021-07-20",
                       "2021-08-20")),  "%d %b %Y"))
#Plot data by location
p4 = spp_date_canta_2024 %>%
ggplot(aes(date, count, fill = Species)) +
geom_col(position = "dodge",width=3) +
scale_fill_manual(values = c("darkgreen", "orange3")) +
coord_cartesian(expand = FALSE) +
ylab("Species") +
ylim(0,55) +
xlab("Date") +
theme_bw()+
scale_x_date(
    limits = as.Date(c("2024-05-15", "2024-08-25")),
    breaks = as.Date(c("2024-05-20",
                       "2024-06-20", "2024-07-20", "2024-08-20")),
    labels = format(as.Date(c("2024-05-20",
                       "2024-06-20", "2024-07-20",
                       "2024-08-20")),  "%d %b %Y"))

((p3 / p4) | (p1/p2)) + plot_layout(guides = "collect") &
theme(legend.position = "bottom")
