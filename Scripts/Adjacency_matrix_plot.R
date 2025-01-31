library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)  # For pivot_longer
library(tidyverse)
#Load data
matrices_list = readRDS("Data/matrices_list.rds")

create_interaction_plot <- function(matrix_data) {
  # Convert the matrix to long format
  net_long <- melt(matrix_data, varnames = c("from", "to"), value.name = "weight")

  # Create the edgelist by filtering for positive weights
  edgelist <- net_long %>%
    filter(weight > 0)  # Keep only positive interactions

  # Create node list using unique node names from edgelist
  nodelist <- data.frame(name = unique(c(edgelist$from, edgelist$to)), stringsAsFactors = FALSE)

  # Create the adjacency matrix plot data from the edgelist
  plot_data <- edgelist %>%
    filter(weight > 0) %>%
    mutate(from =as.character(from)) %>%
    mutate(to =as.character(to))

  # Create the adjacency matrix plot with a color gradient
  ggplot(plot_data, aes(x = from, y = to)) +
    geom_tile(aes(fill = weight), color = "black", height = 1, width = 1) +
    theme_bw() +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Number of Interactions") +
    theme(
      axis.text.x = element_text(angle = 270, hjust = 0),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    coord_fixed(ratio = 1)  # Maintain square shape for the tiles
}

# Example Usage:
 matrices_list = readRDS("Data/matrices_list.rds")
 create_interaction_plot(matrices_list[[1]])
