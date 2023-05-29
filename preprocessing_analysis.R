# Libraries
library(tidyverse)
library(stringr)

# read in data
path_to_artists <- "data/artists/"
path_to_tracks <- "data/tracks/"

artist_spotify_data <- read_csv(paste0(path_to_artists, "spotify_data.csv"))
track_spotify_data <- read_csv(paste0(path_to_tracks, "spotify_data.csv"))
nationality_data <- read_csv(paste0(path_to_artists, "nationalities.csv"))

# explore the data
View(artist_spotify_data)
View(track_spotify_data)
View(nationality_data)

glimpse(artist_spotify_data)
glimpse(track_spotify_data)
glimpse(nationality_data)

## preprocessing

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

## Analysis

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
        return(country_name)
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
    as.data.frame() %>%
    arrange(-Freq)
colnames(collaborative_country_counts) <- c("country", "collaboration_count")

# compute country appearance frequencies for single-artist tracks
single_artist_country_counts <- track_data_clean %>%
    filter(num_artists == 1) %>%
    pull(artist_countries) %>%
    unlist() %>%
    table() %>%
    as.data.frame() %>%
    arrange(-Freq)
colnames(single_artist_country_counts) <- c("country", "single_artist_count")

# join the two dataframes to compute the proportion of collaborative tracks for each country
country_counts_df <- collaborative_country_counts %>%
    full_join(single_artist_country_counts, by = "country") %>%
    mutate(
        collaboration_count = replace(collaboration_count, is.na(collaboration_count), 0),
        collaboration_proportion = collaboration_count / (collaboration_count + single_artist_count)
    ) %>%
    arrange(-collaboration_proportion)

View(country_counts_df)

# 2. Which genres tend to have a higher frequency of collaborations?
# 3. Is there a correlation between collaboration and artistic success?
# 4. How does collaboration impact overall artist success?
# 5. How has collaboration between African artists evolved over time?
