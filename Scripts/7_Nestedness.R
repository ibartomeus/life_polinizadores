#Compute nestedness

#Load libraries
library(dplyr)
library(maxnodf) #for nestedness
matrices_list = readRDS("Data/matrices_list.rds")


nestedness_temp = function(interaction_matrix) {
# Calculate nestedness using the nestedtemp function
s = nestedtemp(interaction_matrix)
# Return the statistic
return(s$statistic/100)

}

nestedness_networks = map(matrices_list, nestedness_temp)

nestedness_tibble = bind_rows(nestedness_networks, .id = "id")

nestedness_tibble = nestedness_tibble %>%
separate(id, into = c("site", "Year"), sep = "_")

#Save data to avoid running it on report
saveRDS(nestedness_tibble, "Data/nestedness.rds")

nestedness_tibble %>%
ggplot(aes(site, temperature, fill = Year), group = Year) +
geom_col(position = "dodge", alpha=0.9) +
scale_fill_manual(values = c("black", "tomato4")) +
theme_bw() +
coord_cartesian(expand = FALSE) +
xlab(NULL) +
ylab("Species") +
ggtitle("c) Nestedness")+ theme(plot.title = element_text(size=18, face= "bold"), panel.border = element_rect(size=1.3),
                axis.title = element_text(size=14),
                axis.text = element_text(size=12))




can_2021_nest = nestedness_tibble %>%
filter(site == "cantavieja" & Year == "2021")
can_2024_nest = nestedness_tibble %>%
filter(site == "cantavieja" & Year == "2024")
eje_2021_nest = nestedness_tibble %>%
filter(site == "ejea" & Year == "2021")
eje_2024_nest = nestedness_tibble %>%
filter(site == "ejea" & Year == "2024")
