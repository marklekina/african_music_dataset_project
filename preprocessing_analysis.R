# Libraries
library(tidyverse)
library(stringr)
library(ggplot2)

# path(s) to input data
path_to_artists <- "data/artists/"
path_to_tracks <- "data/tracks/"

# path to output data
path_to_output <- "data/output/"

artist_spotify_data <- read_csv(paste0(path_to_artists, "spotify_data.csv"))
track_spotify_data <- read_csv(paste0(path_to_tracks, "spotify_data.csv"))
nationality_data <- read_csv(paste0(path_to_artists, "nationalities.csv"))

# explore the data
# View(artist_spotify_data)
# View(track_spotify_data)
# View(nationality_data)

# glimpse(artist_spotify_data)
# glimpse(track_spotify_data)
# glimpse(nationality_data)

## Preprocessing

# function to split `|`-separated strings into vectors
split_string_lists <- function(string) {
    split_vector <- strsplit(string, "\\|")[[1]]
    return(split_vector)
}

# apply function to artist data
artist_data <- artist_spotify_data %>%
    mutate(genres_vector = lapply(genres, split_string_lists))

# apply function to track data
track_data <- track_spotify_data %>%
    mutate(
        artist_ids_vector = lapply(artist_ids, split_string_lists),
        artist_names_vector = lapply(artist_names, split_string_lists)
    )

# note that some tracks have the same IDs but are essentially different releases of the same song, usually in different compilations.
# at this stage, we do not expect duplicate IDs, since we already filtered them out in the data collection stage.

# function to remove whitespace and convert track name to lowercase
remove_whitespace_lowecase <- function(track_name) {
    track_name <- track_name %>%
        str_remove_all("\\s+") %>%
        str_to_lower()

    return(track_name)
}

# filter out these duplicate tracks
track_data_dedup <- track_data %>%
    arrange(-popularity) %>%
    mutate(
        track_name_processed = remove_whitespace_lowecase(track_name),
        is_duplicate = duplicated(track_name_processed, artist_ids_vector)
    ) %>%
    filter(!is_duplicate) %>%
    select(-track_name_processed, -is_duplicate)

## Preliminary analysis

# 1. Which country's artists have the highest proportion of collaborations?

# mark collaborations and single-artist tracks
track_data_clean <- track_data_dedup %>%
    mutate(num_artists = sapply(artist_ids_vector, length))

# function to return the country names of an artist with the given ID
get_country_name <- function(artist_id, artist_nationality_data) {
    # get country name from artist ID
    match <- artist_nationality_data %>%
        filter(id %in% artist_id)

    if (nrow(match) > 0) {
        country_name <- pull(match, country)
        return(country_name[[1]])
    }
    return(NA)
}

# function to get artist popularity from ID
get_artist_popularity <- function(artist_id, artist_data) {
    # get country name from artist ID
    match <- artist_data %>%
        filter(id %in% artist_id)

    if (nrow(match) > 0) {
        artist_popularity <- pull(match, popularity)
        return(artist_popularity[[1]])
    }
    return(NA)
}

# function to get artist name from ID
get_artist_name <- function(artist_id, artist_data) {
    # get country name from artist ID
    match <- artist_data %>%
        filter(id %in% artist_id)

    if (nrow(match) > 0) {
        artist_name <- pull(match, name)
        return(artist_name[[1]])
    }
    return(NA)
}

# subset to tracks with at least one African artist
track_data_clean <- track_data_clean %>%
    mutate(artist_countries = map(artist_ids_vector, ~ get_country_name(.x, artist_data))) %>%
    filter(!is.na(artist_countries))

# compute country appearance frequencies for collaborative tracks
collaborative_country_counts <- track_data_clean %>%
    filter(num_artists > 1) %>%
    pull(artist_countries) %>%
    unlist() %>%
    table() %>%
    as.data.frame()
colnames(collaborative_country_counts) <- c("country", "collaboration_count")

# compute country appearance frequencies for single-artist tracks
single_artist_country_counts <- track_data_clean %>%
    filter(num_artists == 1) %>%
    pull(artist_countries) %>%
    unlist() %>%
    table() %>%
    as.data.frame()
colnames(single_artist_country_counts) <- c("country", "single_artist_count")

# join the two dataframes to compute the proportion of collaborative tracks for each country
country_proportions_df <- collaborative_country_counts %>%
    full_join(single_artist_country_counts, by = "country") %>%
    mutate(
        collaboration_count = replace(collaboration_count, is.na(collaboration_count), 0),
        collaboration_proportion = collaboration_count / (collaboration_count + single_artist_count),
        single_artist_proportion = single_artist_count / (collaboration_count + single_artist_count)
    ) %>%
    arrange(-collaboration_proportion, -single_artist_proportion)

# [TABLE] Top 10 countries with the highest proportion of collaborative tracks
top_10_countries_latex <- country_proportions_df %>%
    arrange(-collaboration_proportion, ) %>%
    select(country, collaboration_proportion, collaboration_count) %>%
    head(10) %>%
    kable(format = "latex", booktabs = TRUE, caption = "Top 10 countries with the highest proportion of collaborative tracks")

# [TABLE] Top 10 countries with the highest proportion of single-artist tracks
bottom_10_countries_latex <- country_proportions_df %>%
    arrange(-single_artist_proportion) %>%
    select(country, single_artist_proportion, single_artist_count) %>%
    head(10) %>%
    kable(format = "latex", booktabs = TRUE, caption = "Top 10 countries with the highest proportion of single-artist tracks")

# View(country_proportions_df)

# write to CSV
write_csv(country_proportions_df, paste0(path_to_output, "collaboration_proportions_by_country.csv"))


# 2. Which genres tend to have a higher frequency of collaborations?

# function to return the genres of an artist with the given ID
get_artist_genres <- function(artist_id, artist_data) {
    # get genres from artist ID
    match <- artist_data %>%
        filter(id %in% artist_id)

    if (nrow(match) > 0) {
        genres <- pull(match, genres_vector)
        return(genres)
    }
    return(NA)
}

# subset to tracks whose artists have at least one associated genre
track_data_clean <- track_data_clean %>%
    mutate(artist_genres = map(artist_ids_vector, ~ get_artist_genres(.x, artist_data))) %>%
    filter(!is.na(artist_genres))

# View(track_data_clean)

# compute genre appearance frequencies for collaborative tracks
collaborative_genre_counts <- track_data_clean %>%
    filter(num_artists > 1) %>%
    pull(artist_genres) %>%
    unlist() %>%
    table() %>%
    as.data.frame()

colnames(collaborative_genre_counts) <- c("genre", "collaboration_count")

# compute genre appearance frequencies for single-artist tracks
single_artist_genre_counts <- track_data_clean %>%
    filter(num_artists == 1) %>%
    pull(artist_genres) %>%
    unlist() %>%
    table() %>%
    as.data.frame()

colnames(single_artist_genre_counts) <- c("genre", "single_artist_count")

# join the two dataframes to compute the proportion of collaborative tracks for each genre
genre_proportions_df <- collaborative_genre_counts %>%
    full_join(single_artist_genre_counts, by = "genre") %>%
    mutate(
        collaboration_count = replace(collaboration_count, is.na(collaboration_count), 0),
        single_artist_count = replace(single_artist_count, is.na(single_artist_count), 0),
        collaboration_proportion = collaboration_count / (collaboration_count + single_artist_count),
        single_artist_proportion = single_artist_count / (collaboration_count + single_artist_count)
    ) %>%
    arrange(-collaboration_proportion, -single_artist_proportion)

# View(genre_proportions_df)

# [TABLE]: Top 10 genres by collaboration proportion
top_10_genres_latex <- genre_proportions_df %>%
    arrange(-collaboration_proportion) %>%
    select(genre, collaboration_proportion, collaboration_count) %>%
    head(10) %>%
    kable(format = "latex", booktabs = TRUE, caption = "Top 10 genres by collaboration proportion")

# [TABLE]: Top 10 genres by single-artist proportion
bottom_10_genres_latex <- genre_proportions_df %>%
    arrange(-single_artist_proportion) %>%
    select(genre, single_artist_proportion, single_artist_count) %>%
    head(10) %>%
    kable(format = "latex", booktabs = TRUE, caption = "Top 10 genres by single-artist proportion")


# write to CSV
write_csv(genre_proportions_df, paste0(path_to_output, "collaboration_proportions_by_genre.csv"))

# 3. How has collaboration between African artists evolved over time?
time_proportions_df <- track_data_clean %>%
    mutate(
        release_date_dbl = case_when(
            str_length(release_date) == 4 ~ as.Date(paste0(release_date, "-01-01")),
            str_length(release_date) > 4 ~ as.Date(release_date),
            TRUE ~ NA
        ),
        release_year = year(release_date_dbl)
    ) %>%
    group_by(release_year) %>%
    summarise(
        collaboration_count = sum(num_artists > 1),
        single_artist_count = sum(num_artists == 1),
    ) %>%
    mutate(
        collaboration_proportion = collaboration_count / (collaboration_count + single_artist_count),
        single_artist_proportion = single_artist_count / (collaboration_count + single_artist_count)
    )
# View(time_proportions_df)

# write to CSV
write_csv(time_proportions_df, paste0(path_to_output, "collaboration_proportions_by_time.csv"))

# [FIGURE] : Proportion of collaborative tracks over time
# plot the proportion of collaborative tracks over time and save to file
path_to_figures <- "data/figures/"
png(filename = paste0(path_to_figures, "collaboration_proportions_by_time.png"), width = 800, height = 600, units = "px")
time_proportions_df %>%
    ggplot(aes(x = release_year, y = collaboration_proportion)) +
    geom_line() +
    geom_point() +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme_minimal() +
    theme(
        legend.position = c(0.2, 0.8)
    )
dev.off()

## Network analysis

# function to compute an adjacency matrix from a dataframe of collaborations
compute_adjacency_matrix <- function(collaborations_data) {
    # get all unique artist IDs
    artist_ids <- collaborations_data %>%
        pull(artist_ids_vector) %>%
        unlist() %>%
        unique()

    # create an empty adjacency matrix
    adjacency_matrix <- matrix(0, nrow = length(artist_ids), ncol = length(artist_ids))
    colnames(adjacency_matrix) <- artist_ids
    rownames(adjacency_matrix) <- artist_ids

    # fill in the adjacency matrix
    for (i in 1:nrow(collaborations_data)) {
        artist_ids <- collaborations_data$artist_ids_vector[[i]]
        for (j in 1:length(artist_ids)) {
            for (k in 1:length(artist_ids)) {
                if (j != k) {
                    adjacency_matrix[artist_ids[j], artist_ids[k]] <- adjacency_matrix[artist_ids[j], artist_ids[k]] + 1
                }
            }
        }
    }
    return(adjacency_matrix)
}

# create an adjacency matrix for the collaboration network
artist_adj_matrix <- track_data_clean %>%
    filter(num_artists > 1) %>%
    compute_adjacency_matrix()

# write the adjacency matrix to a CSV file
write.csv(artist_adj_matrix, paste0(path_to_output, "artist_adj_matrix.csv"))

# function to get the name or country of artists with the given ID vector
get_artist_attributes <- function(artist_ids, artist_data, attribute) {
    artist_attributes <- map_chr(artist_ids, ~ {
        artist_id <- .x
        match <- artist_data %>%
            filter(id %in% artist_id)

        if (nrow(match) > 0) {
            if (attribute == "name") {
                return(pull(match, name)[1])
            } else if (attribute == "country") {
                return(pull(match, country)[1])
            }
        }
        return(NA)
    })
    return(artist_attributes)
}

# add country attribute to the adjacency matrix
attr(colnames(artist_adj_matrix), "country") <- get_artist_attributes(colnames(artist_adj_matrix), artist_data, "country")

# subset to collaborations between known African artists
valid_ids <- !is.na(attr(colnames(artist_adj_matrix), "country"))
artist_african_adj_matrix <- artist_adj_matrix[valid_ids, valid_ids]

# write the adjacency matrix to a CSV file
write.csv(artist_african_adj_matrix, paste0(path_to_output, "african_artist_adj_matrix.csv"))
