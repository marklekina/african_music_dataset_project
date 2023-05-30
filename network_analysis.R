# libraries
library(igraph)
library(GGally)

# load network data
path_to_output <- "data/output/"
artist_african_adj_matrix <- as.matrix(read.csv(paste0(path_to_output, "african_artist_adj_matrix.csv"), header = TRUE, row.names = 1, check.names = FALSE))

# create an igraph object from the adjacency matrix
artist_african_graph <- simplify(graph_from_adjacency_matrix(artist_african_adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE))

# plot the network
plot(artist_african_graph, vertex.size = 3, vertex.label = NA, edge.arrow.size = 0.1, edge.curved = FALSE, edge.width = 1)
ggnet(artist_african_graph, mode = "fruchtermanreingold")
