
library(stringr)
library(bipartite)
library(patchwork)
#Load data
matrices_list = readRDS("Data/matrices_list.rds")

canta_matrix_2021 = matrices_list[[1]]
canta_matrix_2024 = matrices_list[[2]]

#Make the matrix binary to see if it affects the graph
canta_matrix_2021 = ifelse(canta_matrix_2021>0, 1,0)


#Set species names in italic
par(font = 3)
#select just two words to avoid axes cut off
colnames(canta_matrix_2021) <- word(colnames(canta_matrix_2021),1,2, sep=" ")
rownames(canta_matrix_2021) <- word(rownames(canta_matrix_2021),1,2, sep=" ")

#Plot network canta
p1 = plotweb(sortweb(canta_matrix_2021, sort.order="dec"), method="normal", text.rot=90,
        col.low = "darkolivegreen1", col.high = "darkorange",
        col.interaction="gray75",bor.col.interaction ="NA", labsize =.6,
        y.lim = c(-2.5, 4))

#select just two words to avoid axes cut off
colnames(canta_matrix_2024) <- word(colnames(canta_matrix_2024),1,2, sep=" ")
rownames(canta_matrix_2024) <- word(rownames(canta_matrix_2024),1,2, sep=" ")

#Plot network canta
p2 = plotweb(sortweb(canta_matrix_2024, sort.order="dec"), method="normal", text.rot=90,
        col.low = "darkolivegreen1", col.high = "darkorange",
        col.interaction="gray75",bor.col.interaction ="NA", labsize =.6,
        y.lim = c(-2.5, 4), main= "a")


p1 + p2
