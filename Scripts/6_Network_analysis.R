
library(bipartite)
library(dplyr)
library(purrr)

matrices_list = readRDS("Data/matrices_list.rds")

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




