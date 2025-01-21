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
dplyr::select(locality, site_id, date,
       transect, year, plants, pollinators, temperature)

ejea_interactions_2021=main_cols %>%
filter(locality=="ejea") %>%
#filter(pollinators=="Apis mellifera") %>%
filter(year=="2021") %>%
group_by(date, temperature, year) %>%
summarise(interactions = n())


library(MASS)

model_ejea_2021 = glm.nb(interactions ~ temperature ,
                  data=ejea_interactions_2021,
                  control = glm.control(maxit=1000))
summary(model_ejea_2021)

# Create a data frame with the original data
ejea_interactions_2021$predicted = predict(model_ejea_2021, type = "response", interval = "confidence")
p1 = ggplot(ejea_interactions_2021, aes(x = temperature, y = interactions)) +
  geom_point(alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted), color = "black", size = 1) +  # Fitted line
  labs(title = "Ejea 2021",
       x = "Temperature",
       y = "Interactions") +
  theme_minimal()

predict(model_ejea_2021, newdata = ejea_interactions_2021, interval = "prediction")


ejea_interactions_2024=main_cols %>%
filter(locality=="ejea") %>%
#filter(pollinators=="Apis mellifera") %>%
filter(year=="2024") %>%
group_by(date, temperature, year) %>%
summarise(interactions = n())

model_ejea_2024 = glm.nb(interactions ~ temperature ,
                  data=ejea_interactions_2024,
                  control = glm.control(maxit=1000))
summary(model_ejea_2024)

# Create a data frame with the original data
ejea_interactions_2024$predicted = predict(model_ejea_2024, type = "response")
p2 = ggplot(ejea_interactions_2024, aes(x = temperature, y = interactions)) +
  geom_point(alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted), color = "black", size = 1) +  # Fitted line
  labs(title = "Ejea 2024",
       x = "Temperature",
       y = "Interactions") +
  theme_minimal()



#CANTAVIEJA

canta_interactions_2021=main_cols %>%
filter(locality=="cantavieja") %>%
#filter(pollinators=="Apis mellifera") %>%
filter(year=="2021") %>%
group_by(date, temperature, year) %>%
summarise(interactions = n())


library(MASS)

model_canta_2021 = glm.nb(interactions ~ temperature ,
                  data=canta_interactions_2021,
                  control = glm.control(maxit=1000))
summary(model_canta_2021)

# Create a data frame with the original data
canta_interactions_2021$predicted = predict(model_canta_2021, type = "response")
p3 = ggplot(canta_interactions_2021, aes(x = temperature, y = interactions)) +
  geom_point(alpha = 0.5, color = "tomato3") +  # Actual data points
  geom_line(aes(y = predicted), color = "black", size = 1) +  # Fitted line
  labs(title = "cantavieja 2021",
       x = "Temperature",
       y = "Interactions") +
  theme_minimal()


canta_interactions_2024=main_cols %>%
filter(locality=="cantavieja") %>%
#filter(pollinators=="Apis mellifera") %>%
filter(year=="2024") %>%
group_by(date, temperature, year) %>%
summarise(interactions = n())

model_canta_2024 = glm.nb(interactions ~ temperature ,
                  data=canta_interactions_2024,
                  control = glm.control(maxit=1000))
summary(model_canta_2024)

# Create a data frame with the original data
canta_interactions_2024$predicted = predict(model_canta_2024, type = "response")
p4 = ggplot(canta_interactions_2024, aes(x = temperature, y = interactions)) +
  geom_point(alpha = 0.5, color="tomato3") +  # Actual data points
  geom_line(aes(y = predicted), color = "black", size = 1) +  # Fitted line
  labs(title = "Cantavieja 2024",
       x = "Temperature",
       y = "Interactions") +
  theme_minimal()


library(patchwork)

p1/p2|p3/p4




#Quick checks of the models
library(DHARMa)
testDispersion(model_ejea_2021)
simulationOutput <- simulateResiduals(fittedModel = model_ejea_2021, plot = F)
plot(simulationOutput)

testDispersion(model_ejea_2024)
simulationOutput <- simulateResiduals(fittedModel = model_ejea_2024, plot = F)
plot(simulationOutput)

testDispersion(model_canta_2021)
simulationOutput <- simulateResiduals(fittedModel = model_canta_2021, plot = F)
plot(simulationOutput)

testDispersion(model_canta_2021)
simulationOutput <- simulateResiduals(fittedModel = model_canta_2021, plot = F)
plot(simulationOutput)
