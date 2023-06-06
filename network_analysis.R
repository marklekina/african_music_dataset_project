# libraries
library(igraph)
library(nnet)
library(lmtest)
library(sandwich)
library(broom)
library(knitr)
library(kableExtra)

# load custom functions
source("preprocessing_analysis.R")

# load network data
path_to_output <- "data/output/"
artist_african_adj_matrix <- as.matrix(read.csv(paste0(path_to_output, "african_artist_adj_matrix.csv"), header = TRUE, row.names = 1, check.names = FALSE))

# specify figures directory
path_to_figures <- "data/figures/"

# create an igraph object from the adjacency matrix
artist_african_graph <- simplify(graph_from_adjacency_matrix(artist_african_adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE))

# get the largest connected component
artist_african_components <- components(artist_african_graph)
largest_component_idx <- which.max(artist_african_components$csize)
artists_graph <- induced_subgraph(artist_african_graph, artist_african_components$membership == largest_component_idx)

# load artist nationality data
path_to_artists <- "data/artists/"
artist_data <- read.csv(paste0(path_to_artists, "spotify_data.csv"))

# compute artist attributes from nationality data
artist_ids <- V(artists_graph)$name
artist_countries <- map(artist_ids, ~ get_country_name(.x, artist_data)) %>%
    unlist() %>%
    gsub("Democratic Republic of the Congo", "DRC", .) %>%
    gsub("Republic of the Congo", "Congo", .)
artist_names <- map(artist_ids, ~ get_artist_name(.x, artist_data)) %>% unlist()


# update the attributes of the graph
V(artists_graph)$name <- artist_names
V(artists_graph)$id <- artist_ids
V(artists_graph)$country <- artist_countries

# add country colors attribute
countries_unique <- unique(V(artists_graph)$country)
country_colors <- rainbow(length(countries_unique))
node_colors <- country_colors[match(V(artists_graph)$country, countries_unique)]

# specify layout
layout <- layout_with_fr(artists_graph)

# [FIGURE] : African Artist Network by Country
# plot the network with country colors and specified layout and save the plot
# png(paste0(path_to_figures, "african_artist_network_countries.png"), width = 10, height = 7, units = "in", res = 300)
plot(artists_graph, layout = layout, vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.1, edge.curved = FALSE, edge.width = 1, vertex.color = node_colors)
legend("bottomright",
    legend = countries_unique, col = country_colors, pch = 16, title = "Nationalities",
    bg = "white", border = "black", cex = 0.8, pt.cex = 1.0, title.font = 2, title.col = "black"
)
# dev.off()

# 1. Centrality Measures

# compute centrality measures
centrality_measures <- data.frame(
    degree = degree(artists_graph),
    closeness = closeness(artists_graph),
    betweenness = betweenness(artists_graph),
    eigenvector = eigen_centrality(artists_graph)$vector,
    page_rank = page_rank(artists_graph)$vector
) %>% mutate(popularity = unlist(map(V(artists_graph)$id, ~ get_artist_popularity(.x, artist_data))))

# perform linear regression to determine whether (jointly) centrality measures are predictors of artist popularity
centrality_jointly <- centrality_measures %>%
    mutate(
        degree = scale(degree),
        closeness = scale(closeness),
        betweenness = scale(betweenness),
        eigenvector = scale(eigenvector),
        page_rank = scale(page_rank)
    ) %>%
    lm(popularity ~ ., data = .) %>%
    summary() %>%
    tidy()

# [TABLE] : Linear Regression of Artist Popularity on Joint Centrality Measures
centrality_jointly_latex <- centrality_jointly %>%
    kable(format = "latex", booktabs = TRUE, caption = "Linear Regression of Artist Popularity on Joint Centrality Measures")

# perform linear regression to determine whether (individually) centrality measures are predictors of artist popularity
centrality_individual <- centrality_measures %>%
    pivot_longer(-popularity, names_to = "centrality_measure", values_to = "value") %>%
    group_by(centrality_measure) %>%
    do(model_summary = tidy(lm(popularity ~ value, data = .))) %>%
    ungroup() %>%
    unnest(model_summary)

# [TABLE] : Linear Regression of Artist Popularity on Individual Centrality Measures
centrality_individual_latex <- centrality_individual %>%
    kable(format = "latex", booktabs = TRUE, caption = "Linear Regression of Artist Popularity on Individual Centrality Measures") # nolint



# 2. Community Structure

# compute community structure using the Girvan-Newman algorithm
communities <- cluster_edge_betweenness(artists_graph)

# get colors for each community
unique_communities <- unique(membership(communities))
community_colors <- rainbow(length(unique_communities))

# specify layout
layout <- layout_with_fr(artists_graph)

# [FIGURE] : Artist Network with Communities
# png(paste0(path_to_figures, "african_artist_network_communities.png"), width = 10, height = 7, units = "in", res = 300)
plot(communities, artists_graph, layout = layout, vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.1, edge.curved = FALSE, edge.width = 1, vertex.color = community_colors)
# dev.off()

# perform logistic regression with one-hot encoding to determine whether community membership is a predictor of artist popularity
membership_data <- data.frame(
    id = V(artists_graph)$id, community = as.numeric(membership(communities))
) %>%
    mutate(popularity = unlist(map(id, ~ get_artist_popularity(.x, artist_data))))

# create a one-hot encoding of the community membership
membership_one_hot <- model.matrix(~ community - 1, membership_data)

# bind the one-hot encoding to the data frame
membership_data <- cbind(membership_one_hot, membership_data)

# create a logistic regression model
membership_model <- glm(popularity ~ community, data = membership_data)

# use robust standard errors to determine whether the community membership is a predictor of artist popularity
membership_popularity_df <- coeftest(membership_model, vcov = vcovHC(membership_model, type = "HC1")) %>%
    tidy()

# [TABLE] : Logistic Regression of Artist Popularity on Community Membership
membership_popularity_latex <- membership_popularity_df  %>%
    kable(format = "latex", booktabs = TRUE, caption = "Logistic Regression of Artist Popularity on Community Membership")