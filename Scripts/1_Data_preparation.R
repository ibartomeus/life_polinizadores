#Read data from 2021 and 2024
#First,load libraries
library(dplyr)
library(reshape2)
library(stringr)
library(iNEXT)
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

#Cantavieja data ----
#Catavieja 2021
main_cols_cantavieja_2021 = main_cols %>%
filter(site_id == "cantavieja_2021")
#Count occurrences of each species in each study
poll_result_cantavieja_2021 = main_cols_cantavieja_2021 %>%
count(date, pollinators) %>%
mutate(n = if_else(n>0,1,0)) %>%
group_by(pollinators) %>%
summarise(Incidence = sum(n))
#Generate incidence matrix with number of sampling units at the begining
poll_cantavieja_2021 = matrix(c(length(unique(main_cols_cantavieja_2021$date)) ,
          poll_result_cantavieja_2021 %>% pull(Incidence)),
          ncol = 1)
row.names(poll_cantavieja_2021) = c("Plot", poll_result_cantavieja_2021 %>% pull(pollinators))
poll_cantavieja_2021 = data.frame(poll_cantavieja_2021)
colnames(poll_cantavieja_2021) = "Species"

#Calculate sampling coverage
poll_output_cantavieja_2021 = iNEXT(poll_cantavieja_2021, datatype = 'incidence_freq')
poll_output_cantavieja_2021$iNextEst$size_based
#Plot
ggiNEXT(poll_output_cantavieja_2021, type=1)
plotting_cantavieja_2021 = poll_output_cantavieja_2021$iNextEst$size_based
plotting_cantavieja_2021
#Plot
ggiNEXT(poll_output_cantavieja_2021, type=1)

#Catavieja 2024
main_cols_cantavieja_2024 = main_cols %>%
filter(site_id == "cantavieja_2024")
#Count occurrences of each species in each study
poll_result_cantavieja_2024 = main_cols_cantavieja_2024 %>%
count(date, pollinators) %>%
mutate(n = if_else(n>0,1,0)) %>%
group_by(pollinators) %>%
summarise(Incidence = sum(n))
#Generate incidence matrix with number of sampling units at the begining
poll_cantavieja_2024 = matrix(c(length(unique(main_cols_cantavieja_2024$date)) ,
          poll_result_cantavieja_2024 %>% pull(Incidence)),
          ncol = 1)
row.names(poll_cantavieja_2024) = c("Plot", poll_result_cantavieja_2024 %>% pull(pollinators))
poll_cantavieja_2024 = data.frame(poll_cantavieja_2024)
colnames(poll_cantavieja_2024) = "Species"
#Calculate sampling coverage
poll_output_cantavieja_2024 = iNEXT(poll_cantavieja_2024, datatype = 'incidence_freq')
plotting_cantavieja_2024 = poll_output_cantavieja_2024$iNextEst$size_based
plotting_cantavieja_2024
#Plot
ggiNEXT(poll_output_cantavieja_2024, type=1)

#Plot all
plotting_cantavieja_2021$Year = "2021"
plotting_cantavieja_2024$Year = "2024"

cantavieja_combined = bind_rows(plotting_cantavieja_2021,
                                plotting_cantavieja_2024)

#Create a unique tibble one for each method
cantavieja_rarefaction = cantavieja_combined %>%
filter(Method == "Rarefaction")
cantavieja_extrapolation = cantavieja_combined %>%
filter(Method == "Extrapolation")
cantavieja_observed = cantavieja_combined %>%
filter(Method == "Observed")
#Small hack to visualize better the graph
cantavieja_observed_fix_rarefaction = cantavieja_observed %>%
mutate(Method = "Rarefaction")
cantavieja_observed_fix_extrapolation = cantavieja_observed %>%
mutate(Method = "Extrapolation")
#Bind rows
cantavieja_rarefaction = bind_rows(cantavieja_rarefaction,
                                cantavieja_observed_fix_rarefaction)
cantavieja_extrapolation = bind_rows(cantavieja_extrapolation,
                                cantavieja_observed_fix_extrapolation)

#Plot
p1 = ggplot(cantavieja_rarefaction, aes(x = t, y = qD, group = Year)) +
 geom_line(aes(colour = Year), size= 1.5) +
 geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = Year), alpha = 0.2) +
 scale_colour_manual(name = "Year",
      values = c("2021" = "black", "2024" = "tomato4")) +
 scale_fill_manual(name = "Year",
      values = c("2021" = "black", "2024" = "tomato4")) + # Added scale for fill
 geom_line(data = cantavieja_extrapolation,
      aes(x = t, y = qD, colour = Year, group = Year),
      linetype = "dashed", size= 1.5) +
 geom_ribbon(data =cantavieja_extrapolation, aes(ymin = qD.LCL, ymax = qD.UCL, fill = Year), alpha = 0.2) +
 geom_point(data = cantavieja_observed,
            aes(x = t, y = qD, colour = Year),
            inherit.aes = FALSE, size= 3.5) +
 xlab("Sampling rounds") +
 ylab("Pollinator species") +
 ggtitle("Cantavieja") +
 theme_bw() +
 theme(panel.grid.minor = element_blank()) +
  coord_cartesian(expand = FALSE) +
 scale_y_continuous(limits = c(10, 150),
      breaks = c(50, cantavieja_observed$qD,
      cantavieja_observed$qD, 100))

#Ejea data ----
main_cols_ejea_2021 = main_cols %>%
filter(site_id == "ejea_2021")
#Count occurrences of each species in each study
poll_result_ejea_2021 = main_cols_ejea_2021 %>%
count(date, pollinators) %>%
mutate(n = if_else(n>0,1,0)) %>%
group_by(pollinators) %>%
summarise(Incidence = sum(n))
#Generate incidence matrix with number of sampling units at the begining
poll_ejea_2021 = matrix(c(length(unique(main_cols_ejea_2021$date)) ,
          poll_result_ejea_2021 %>% pull(Incidence)),
          ncol = 1)
row.names(poll_ejea_2021) = c("Plot", poll_result_ejea_2021 %>% pull(pollinators))
poll_ejea_2021 = data.frame(poll_ejea_2021)
colnames(poll_ejea_2021) = "Species"

#Calculate sampling coverage
poll_output_ejea_2021 = iNEXT(poll_ejea_2021, datatype = 'incidence_freq')
poll_output_ejea_2021$iNextEst$size_based
#Plot
ggiNEXT(poll_output_ejea_2021, type=1)
plotting_ejea_2021 = poll_output_ejea_2021$iNextEst$size_based
plotting_ejea_2021
#Plot
ggiNEXT(poll_output_ejea_2021, type=1)

#Catavieja 2024
main_cols_ejea_2024 = main_cols %>%
filter(site_id == "ejea_2024")
#Count occurrences of each species in each study
poll_result_ejea_2024 = main_cols_ejea_2024 %>%
count(date, pollinators) %>%
mutate(n = if_else(n>0,1,0)) %>%
group_by(pollinators) %>%
summarise(Incidence = sum(n))
#Generate incidence matrix with number of sampling units at the begining
poll_ejea_2024 = matrix(c(length(unique(main_cols_ejea_2024$date)) ,
          poll_result_ejea_2024 %>% pull(Incidence)),
          ncol = 1)
row.names(poll_ejea_2024) = c("Plot", poll_result_ejea_2024 %>% pull(pollinators))
poll_ejea_2024 = data.frame(poll_ejea_2024)
colnames(poll_ejea_2024) = "Species"
#Calculate sampling coverage
poll_output_ejea_2024 = iNEXT(poll_ejea_2024, datatype = 'incidence_freq')
plotting_ejea_2024 = poll_output_ejea_2024$iNextEst$size_based
plotting_ejea_2024
#Plot
ggiNEXT(poll_output_ejea_2024, type=1)

#Plot all
plotting_ejea_2021$Year = "2021"
plotting_ejea_2024$Year = "2024"

ejea_combined = bind_rows(plotting_ejea_2021,
                                plotting_ejea_2024)

#Create a unique tibble one for each method
ejea_rarefaction = ejea_combined %>%
filter(Method == "Rarefaction")
ejea_extrapolation = ejea_combined %>%
filter(Method == "Extrapolation")
ejea_observed = ejea_combined %>%
filter(Method == "Observed")
#Small hack to visualize better the graph
ejea_observed_fix_rarefaction = ejea_observed %>%
mutate(Method = "Rarefaction")
ejea_observed_fix_extrapolation = ejea_observed %>%
mutate(Method = "Extrapolation")
#Bind rows
ejea_rarefaction = bind_rows(ejea_rarefaction,
                                ejea_observed_fix_rarefaction)
ejea_extrapolation = bind_rows(ejea_extrapolation,
                                ejea_observed_fix_extrapolation)

#Plot
p2 = ggplot(ejea_rarefaction, aes(x = t, y = qD, group = Year)) +
 geom_line(aes(colour = Year), size= 1.5) +
 geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = Year), alpha = 0.2) +
 scale_colour_manual(name = "Year",
      values = c("2021" = "black", "2024" = "tomato4")) +
 scale_fill_manual(name = "Year",
      values = c("2021" = "black", "2024" = "tomato4")) + # Added scale for fill
 geom_line(data = ejea_extrapolation,
      aes(x = t, y = qD, colour = Year, group = Year),
      linetype = "dashed",  size= 1.5) +
 geom_ribbon(data =ejea_extrapolation, aes(ymin = qD.LCL, ymax = qD.UCL, fill = Year), alpha = 0.2) +
 geom_point(data = ejea_observed,
            aes(x = t, y = qD, colour = Year),
            inherit.aes = FALSE,  size= 3.5) +
 xlab("Sampling rounds") +
 ylab("Pollinator species") +
 ggtitle("Ejea") +
 theme_bw() +
 theme(panel.grid.minor = element_blank()) +
  coord_cartesian(expand = FALSE) +
 scale_y_continuous(limits = c(0, 180),
      breaks = c(40, ejea_observed$qD,
      ejea_observed$qD, 160))


library(patchwork)
library(cowplot)

#save plots
saveRDS(p1, "Data/cantavieja_sampling_curve.rds")
saveRDS(p2, "Data/ejea_sampling_curve.rds")

plot_grid(p1, p2)

