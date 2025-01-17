
#Data visualization

#Load libraries
library(bipartite)

#Load networks
canta_matrix <- readRDS("Data/canta_matrix.rds")
ejea_matrix <- readRDS("Data/ejea_matrix.rds")

#Plot network canta

plotweb(sortweb(canta_matrix, sort.order="dec"), method="normal", text.rot=90,
        col.low = "darkolivegreen1", col.high = "darkorange",
        col.interaction="gray75",bor.col.interaction ="NA", labsize =.55,
        #y.lim = c(-1, 1), high.lablength=0,low.lablength=0
        )


#Plot network ejea
plotweb(sortweb(ejea_matrix, sort.order="dec"), method="normal", text.rot=90,
        col.low = "darkolivegreen1", col.high = "darkorange",
        col.interaction="gray75",bor.col.interaction ="NA", labsize =.55)


