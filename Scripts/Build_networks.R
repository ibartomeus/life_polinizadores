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

library(bipartite)
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
for_plotting = metrics %>%
separate(Site_id, c("site", "Year"), "_") %>%
mutate(site = str_to_title(site)) %>%
rename(Connectance = connectance)
#Plot data
p1 = for_plotting %>%
ggplot(aes(site, H2, fill = Year), group = Year) +
geom_col(position = "dodge", alpha=0.9) +
scale_fill_manual(values = c("black", "tomato4")) +
theme_bw() +
coord_cartesian(expand = FALSE) +
xlab(NULL) +
ylab("Species") +
ggtitle("Specialization")+ theme(plot.title = element_text(size=18, face= "bold"), panel.border = element_rect(size=1.3),
                axis.title = element_text(size=14),
                axis.text = element_text(size=12))


p2 = for_plotting %>%
ggplot(aes(site, Connectance, fill = Year), group = Year) +
geom_col(position = "dodge", alpha=0.9) +
scale_fill_manual(values = c("black", "tomato4")) +
theme_bw() +
coord_cartesian(expand = FALSE) +
xlab(NULL) +
ylab("Species") +
ggtitle("Connectance")+ theme(plot.title = element_text(size=18, face= "bold"), panel.border = element_rect(size=1.3),
                axis.title = element_text(size=14),
                axis.text = element_text(size=12))


#Try to find a way to visualize networks
canta_matrix_2021 = matrices_list[[1]]

plotweb(sortweb(canta_matrix_2021, sort.order="dec"), method="normal", text.rot=90,
        col.low = "darkolivegreen1", col.high = "darkorange",
        col.interaction="gray75",bor.col.interaction ="NA", labsize =.55,
        #y.lim = c(-1, 1), high.lablength=0,low.lablength=0
        )

plotweb(sortweb(canta_matrix_2021, sort.order="dec"))

plotweb(canta_matrix_2021,
        method = 'normal',
        col.high = "darkorange",
        col.low = 'darkolivegreen1',
        col.interaction="gray75",
        high.lablength = NULL,
        low.lablength = NULL,
        text.rot = 90,
        text.high.col = 'black',
        text.low.col='black',
        low.lab.dis=0 )


#Prepare data to load markdown
can_2021 = for_plotting %>%
filter(site == "Cantavieja" & Year == "2021")
can_2024 = for_plotting %>%
filter(site == "Cantavieja" & Year == "2024")
eje_2021 = for_plotting %>%
filter(site == "Ejea" & Year == "2021")
eje_2024 = for_plotting %>%
filter(site == "Ejea" & Year == "2024")

