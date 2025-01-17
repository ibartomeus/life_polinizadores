#Compute robustness

#Load libraries
library(dplyr)
library(maxnodf) #for nestedness
matrices_list = readRDS("Data/matrices_list.rds")

net1 = matrices_list[[1]]


robust = function(interaction_matrix){
#The participant is the group being exterminated
ex = second.extinct(interaction_matrix, participant="lower", method="random", nrep=1000,
	details=FALSE)
robustness(ex)
}

robustness_result = map(matrices_list, robust)

robustness_tibble1 = bind_rows(robustness_result, .id = "id")

long_data = robustness_tibble1 %>%
  pivot_longer(
    cols = everything(),
    names_to = c("site", "Year"),
    values_to = "Robustness",
    names_sep = "_")


long_data %>%
ggplot(aes(site, Robustness, fill = Year), group = Year) +
geom_col(position = "dodge", alpha=0.9) +
scale_fill_manual(values = c("black", "tomato4")) +
theme_bw() +
coord_cartesian(expand = FALSE) +
xlab(NULL) +
ylab("Species") +
ggtitle("d) Robustness")+ theme(plot.title = element_text(size=18, face= "bold"), panel.border = element_rect(size=1.3),
                axis.title = element_text(size=14),
                axis.text = element_text(size=12))

