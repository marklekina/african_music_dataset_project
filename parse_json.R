# libraries
library(tidyverse)
library(jsonlite)
library(listviewer)

# function to parse artist data into a dataframe
parse_artist_data <- function(artist_list) {
    artist_df <- artist_list %>%
        tibble() %>%
        select(followers, genres, id, name, popularity) %>%
        unnest_wider(followers) %>%
        mutate(
            followers = total,
            genres = sapply(genres, function(char_vector) {
                paste0(char_vector, collapse = "|")
            })
        ) %>%
        select(name, id, everything(), -href, -total)

    return(artist_df)
}

# function to parse track data into a dataframe
parse_track_data <- function(track_list) {
    track_df <- track_list %>%
        select(track_name = name, track_id = id, popularity, artists, album) %>%
        unnest_wider(artists) %>%
        select(track_name, track_id, popularity, artist_names = name, artist_ids = id, album) %>%
        unnest_wider(album) %>%
        select(track_name, track_id, artist_names, artist_ids, release_date, album_name = name, album_id = id, album_total_tracks = total_tracks, album_type, popularity) %>%
        mutate(
            artist_names = sapply(artist_names, function(char_vector) {
                paste0(char_vector, collapse = "|")
            }),
            artist_ids = sapply(artist_ids, function(char_vector) {
                paste0(char_vector, collapse = "|")
            })
        )

    return(track_df)
}

# test artist parser function
artist_df <- fromJSON("data/artists/afrobeat_0.json") %>%
    pluck("artists", "items") %>%
    parse_artist_data()
View(artist_df)

artist_df <- fromJSON("data/misc/artists_sauti_sol_related.json") %>%
    pluck() %>%
    parse_artist_data()
View(artist_df)

# test artist parser function
track_df <- fromJSON("data/misc/tracks_afrobeats.json") %>%
    pluck("tracks", "items") %>%
    parse_track_data()
View(track_df)

track_df <- fromJSON("data/tracks/burna_top_tracks.json") %>%
    pluck("tracks") %>%
    parse_track_data()
View(track_df)
