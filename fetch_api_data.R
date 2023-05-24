## Libraries
library(tidyverse)
library(httr)
library(spotifyr)
library(jsonlite)

## Authentication

# function to read client_id and client_secret from file
read_credentials <- function(credentials_file) {
    credentials <- readLines(credentials_file) %>%
        strsplit("=") %>%
        unlist()

    cred_names <- credentials[c(TRUE, FALSE)]
    credentials <- credentials[c(FALSE, TRUE)]
    names(credentials) <- cred_names

    return(credentials)
}

# read credentials from file
credentials <- read_credentials("spotify_credentials.txt")

# set credential environment variables
Sys.setenv(SPOTIFY_CLIENT_ID = credentials["client_id"])
Sys.setenv(SPOTIFY_CLIENT_SECRET = credentials["client_secret"])

# fetch access token and set environment variable
access_token <- get_spotify_access_token()
Sys.setenv(SPOTIFY_ACCESS_TOKEN = access_token)


## Format queries

# global variables
base_url <- "https://api.spotify.com/v1/"
max_limit <- 50

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

# function to search artists by name
get_artist_by_name <- function(artist_name, max_results) {
    # metadata
    type <- "&type=artist"
    limit <- paste0("&limit=", max_results)

    # format query
    query <- URLencode(paste0("artist:", artist_name))

    # put URL together
    url <- paste0(base_url, "search?q=", query, type, limit)
    print(paste0("URL: ", url))

    # fetch data
    artist_data <- GET(url, add_headers(Authorization = paste0("Bearer ", access_token)))

    # convert to JSON
    artist_json <- content(artist_data, as = "text") %>%
        fromJSON()

    # return JSON object
    return(artist_json)
}

# function to filter out non-exact artist name matches
filter_stray_artist_entries <- function(names_df) {
    names_df <- names_df %>%
        filter(str_to_lower(str_remove_all(name, "\\s")) == str_to_lower(str_remove_all(wikipedia_name, "\\s")))
}

# function to fetch Spotify data for all artists
get_artist_data <- function(nationality_data, exact_match = TRUE) {
    # get artist and country names
    artist_names <- nationality_data$artist_name
    country_names <- nationality_data$country_name

    # dataframe to store artist data
    artist_df <- tibble()

    # loop through each artist
    for (i in seq_along(artist_names)) {
        # specify artist and country name
        artist_name <- artist_names[i]
        country_name <- country_names[i]

        # search for artist
        print(paste0("Fetching data for entry ", i, " of ", length(artist_names)))
        artist_json <- get_artist_by_name(artist_name, 1)

        # parse artist data
        row <- tryCatch(
            {
                artist_json %>%
                    pluck("artists", "items") %>%
                    parse_artist_data() %>%
                    mutate(wikipedia_name = artist_name, country = country_name)
            },
            error = function(e) {
                print(paste0("Error fetching data for ", artist_name, " ; ", e$message))
                return(tibble())
            }
        )

        # add row to dataframe
        artist_df <- rbind(artist_df, row)
    }

    # filter out stray entries
    if (exact_match) {
        artist_df <- filter_stray_artist_entries(artist_df)
    }

    return(artist_df)
}

# function to parse track data into a dataframe
parse_track_data <- function(track_list) {
    tryCatch(
        {
            track_df <- track_list %>%
                select(track_name = name, track_id = id, popularity, artists, album) %>%
                unnest_wider(artists) %>%
                select(track_name, track_id, popularity, artist_names = name, artist_ids = id, album) %>%
                unnest_wider(album) %>%
                select(track_name, track_id, artist_names, artist_ids, release_date, album_name = name, album_id = id, album_total_tracks = total_tracks, album_type, popularity)
            return(track_df)
        },
        error = function(e) {
            print(paste0("Error parsing track data: ", e$message))
            return(tibble())
        }
    )
}



# function to search for tracks by artist name
get_tracks_by_artist_name <- function(artist, max_tracks = max_limit) {
    type <- "&type=track"
    limit <- paste0("&limit=", max_tracks)

    # format query
    query <- URLencode(paste0("artist:", artist))

    # put URL together
    url <- paste0(base_url, "search?q=", query, type, limit)
    print(paste0("Fetching data from ", url))

    # fetch data
    track_data <- GET(url, add_headers(Authorization = paste0("Bearer ", access_token)))

    # convert to JSON object
    track_data_json <- content(track_data, , as = "text") %>%
        fromJSON()

    # return JSON object
    return(track_data_json)
}

# function to get artist's top tracks
get_tracks_by_artist_id <- function(artist_id, market = "US") {
    # format API path
    url <- paste0(base_url, "artists/", artist_id, "/top-tracks?market=", market)

    # fetch data
    print(paste0("Fetching data from ", url))
    track_data <- GET(url, add_headers(
        Authorization = paste0("Bearer ", access_token)
    ))

    # convert to JSON object
    track_data_json <- content(track_data, , as = "text") %>%
        fromJSON()

    # return JSON object
    return(track_data_json)
}

# function to fetch Spotify data for all tracks by all artists
get_track_data <- function(artist_data, resume_index = 1) {
    # get artist names and IDs
    artist_names <- artist_data$name
    artist_ids <- artist_data$id

    # dataframe to store track data
    track_df <- tibble()

    # loop through each artist
    for (i in resume_index:length(artist_names)) {
        # specify artist info
        artist_name <- artist_names[i]
        artist_id <- artist_ids[i]

        print(paste0("Fetching track data for entry ", i, " of ", length(artist_names)))

        # get artist's top tracks
        track_json <- get_tracks_by_artist_id(artist_id)

        # parse track JSON data into a dataframe
        artist_id_tracks <- track_json %>%
            pluck("tracks") %>%
            parse_track_data()

        # search for other tracks by artist
        track_json <- get_tracks_by_artist_name(artist_name)

        # parse track JSON data into a dataframe
        artist_name_tracks <- track_json %>%
            pluck("tracks", "items") %>%
            parse_track_data()

        # combine both track dataframes and filter out duplicates
        artist_tracks <- artist_id_tracks %>%
            rbind(artist_name_tracks) %>%
            distinct(track_id, .keep_all = TRUE)

        # remove entries already in main dataframe
        if (nrow(track_df) > 0) {
            artist_tracks <- artist_tracks %>%
                filter(!track_id %in% track_df$track_id)
        }

        # append track dataframe to main dataframe
        track_df <- rbind(track_df, artist_tracks)
    }
    return(track_df)
}


## Tests

# read artist nationality data from CSV
# artist_data <- read_csv("data/artists/nationalities.csv")

# get Spotify data for all artists
# artist_spotify_data <- get_artist_data(artist_data, exact_match = TRUE)
# View(artist_spotify_data)

# write data to CSV
# write_csv(artist_spotify_data, "data/artists/spotify_data.csv")

# read artist Spotify data from CSV
artist_spotify_data <- read_csv("data/artists/spotify_data.csv")

# prioritize the most popular artists from each country for rate limiting purposes
ranked_artist_data <- artist_spotify_data %>%
    group_by(country) %>%
    mutate(popularity_rank = rank(desc(popularity))) %>%
    ungroup() %>%
    arrange(popularity_rank)

# this function is prone to rate limiting, so we'll set up a resume index for when it fails
resume_index <- 1

# loop until we get all track data
track_spotify_data <- get_track_data(ranked_artist_data, resume_index) %>%
    distinct(track_id, .keep_all = TRUE)
View(track_spotify_data)

# combine all different dataframes written to file into one

# specify the directory where the CSV files are located
path_to_tracks <- "data/tracks/"

# get the filenames matching the prefix "spotify_"
file_names <- list.files(path_to_tracks, pattern = "^spotify_data_.*\\.csv$", full.names = TRUE)

# read and combine the data from CSV files into a single dataframe
combined_df <- map_df(file_names, read.csv) %>%
    distinct(track_id, .keep_all = TRUE)

# write the combined dataframe to file
# note: the vector columns have to be converted to strings before writing to CSV. I did not initially do this and it caused problems when reading the CSV back in. I have fixed this in the parsing code above.
write_csv(combined_df, paste0(path_to_tracks, "spotify_data.csv"))
