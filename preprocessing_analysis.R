# Libraries
library(tidyverse)
library(stringr)

# read in data
path_to_artists <- "data/artists/"
path_to_tracks <- "data/tracks/"

artist_spotify_data <- read_csv(paste0(path_to_artists, "artist_spotify_data.csv"))
track_spotify_data <- read_csv(paste0(path_to_tracks, "track_spotify_data.csv"))
nationality_data <- read_csv(paste0(path_to_artists, "nationalities.csv"))

# explore the data
View(artist_spotify_data)
View(track_spotify_data)

colnames(artist_spotify_data)
colnames(track_spotify_data)

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

# filter out these duplicate tracks
track_data_dedup <- track_data %>%
    arrange(-popularity) %>%
    mutate(
        track_name_processed = process_names(track_name),
        is_duplicate = duplicated(track_name_processed, artist_ids_vector)
    ) %>%
    filter(!is_duplicate) %>%
    select(-track_name_processed, -is_duplicate)

# subset to tracks with at least two artist IDs, i.e. collaborations
collaborative_track_data <- track_data_dedup %>%
    mutate(num_artists = lapply(artist_ids_vector, length)) %>%
    filter(num_artists > 1)


# function to return a vector of country names from a vector of artist IDs
get_country_name <- function(artist_id, artist_nationality_data) {
    # get country name from artist ID
    match <- artist_nationality_data %>%
        filter(id %in% artist_id)

    if (nrow(match) > 0) {
        return(pull(match, country))
    }
    return("")
}

# add a column of artist country names to the collaborative track data
track_data_with_countries <- collaborative_track_data %>%
    mutate(artist_countries = sapply(artist_ids_vector, function(vector) {
        map_chr(vector, ~ get_country_name(.x, artist_data)) %>%
            .[nzchar(.)] %>%
            na.omit()
    }))
