#Compute robustness

#Load libraries
library(dplyr)
library(maxnodf) #for nestedness
library(purrr)
library(bipartite)
library(tidyr)
library(ggplot2)
matrices_list = readRDS("Data/matrices_list.rds")


#Compute robustness to plant loss
robust = function(interaction_matrix){
#The participant is the group being exterminated
ex = second.extinct(interaction_matrix, participant="lower", method="random", nrep=1000,
	details=FALSE)
robustness(ex)
}

robustness_plants = map(matrices_list, robust)

robustness_plants1 = bind_rows(robustness_plants, .id = "id")

long_plants = robustness_plants1 %>%
  pivot_longer(
    cols = everything(),
    names_to = c("site", "Year"),
    values_to = "Robustness",
    names_sep = "_")


long_plants %>%
ggplot(aes(site, Robustness, fill = Year), group = Year) +
geom_col(position = "dodge", alpha=0.9) +
scale_fill_manual(values = c("black", "tomato4")) +
theme_bw() +
coord_cartesian(expand = FALSE) +
xlab(NULL) +
ylab("Robustness") +
ggtitle("d) Robustness to plant loss")+ theme(plot.title = element_text(size=18, face= "bold"), panel.border = element_rect(size=1.3),
                axis.title = element_text(size=14),
                axis.text = element_text(size=12))



#Compute robustness to pollinator loss
robust = function(interaction_matrix){
#The participant is the group being exterminated
ex = second.extinct(interaction_matrix, participant="higher", method="random", nrep=1000,
	details=FALSE)
robustness(ex)
}

robustness_pollinators = map(matrices_list, robust)

robustness_pollinators1 = bind_rows(robustness_pollinators, .id = "id")

long_pollinators = robustness_pollinators1 %>%
  pivot_longer(
    cols = everything(),
    names_to = c("site", "Year"),
    values_to = "Robustness",
    names_sep = "_")


long_pollinators %>%
ggplot(aes(site, Robustness, fill = Year), group = Year) +
geom_col(position = "dodge", alpha=0.9) +
scale_fill_manual(values = c("black", "tomato4")) +
theme_bw() +
coord_cartesian(expand = FALSE) +
xlab(NULL) +
ylab("Robustness") +
ggtitle("d) Robustness to pollinator loss")+ theme(plot.title = element_text(size=18, face= "bold"), panel.border = element_rect(size=1.3),
                axis.title = element_text(size=14),
                axis.text = element_text(size=12))


#Compute robustness at network level
robust = function(interaction_matrix){
#The participant is the group being exterminated
ex = second.extinct(interaction_matrix, participant="both", method="random", nrep=1000,
	details=FALSE)
robustness(ex)
}

robustness_network = map(matrices_list, robust)

robustness_network1 = bind_rows(robustness_network, .id = "id")

long_network = robustness_network1 %>%
  pivot_longer(
    cols = everything(),
    names_to = c("site", "Year"),
    values_to = "Robustness",
    names_sep = "_")


long_network %>%
ggplot(aes(site, Robustness, fill = Year), group = Year) +
geom_col(position = "dodge", alpha=0.9) +
scale_fill_manual(values = c("black", "tomato4")) +
theme_bw() +
coord_cartesian(expand = FALSE) +
xlab(NULL) +
ylab("Robustness") +
ggtitle("d) Robustness network-level")+ theme(plot.title = element_text(size=18, face= "bold"), panel.border = element_rect(size=1.3),
                axis.title = element_text(size=14),
                axis.text = element_text(size=12))


#Save data to avoid running it on report
saveRDS(long_plants, "Data/robustness_plants.rds")
saveRDS(long_pollinators, "Data/robustness_pollinators.rds")
saveRDS(long_network, "Data/robustness_network.rds")
