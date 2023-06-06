## Libraries
library(rvest)
library(stringr)
library(tidyverse)

# base URLs
wikipedia_url <- "https://en.wikipedia.org"
african_artists_url <- paste0(wikipedia_url, "/wiki/List_of_African_musicians")

# read in HTML
african_artists_html <- read_html(african_artists_url)

# extract h2 elements (contain country names)
h2_elements <- african_artists_html %>%
    html_nodes("h2")

# inspect country names
h2_elements %>%
    html_node("a") %>%
    html_text() %>%
    str_trim()


# function to check if h2 element contains a valid country name
contains_valid_country_name <- function(h2_element) {
    # extract country name
    country_name <- h2_element %>%
        html_node("a") %>%
        html_text() %>%
        str_trim()

    # from inspection, we have determined that invalid country names are either `NA` values or "edit".
    is_invalid <- country_name %in% c("edit") | is.na(country_name)
    return(!is_invalid)
}

# filter out h2_elements with invalid country names
valid_indices <- map_lgl(h2_elements, contains_valid_country_name)
h2_elements <- h2_elements[valid_indices]

# function to extract country and artist names/links from HTML into dataframes
# for (h2_element in h2_elements) {

scrape_artist_names <- function(h2_element, artist_names_df) {
    # extract country name
    country_name <- h2_element %>%
        html_node("a") %>%
        html_text() %>%
        str_trim()

    # check the type of the next element
    next_element <- h2_element %>%
        html_node(xpath = "./following-sibling::ul[1] | ./following-sibling::p[1] | ./following-sibling::div[1]")

    # extract link to artist names
    if (html_name(next_element) %in% c("div", "p")) {
        artist_names_link <- next_element %>%
            html_nodes("a") %>%
            html_attr("href") %>%
            paste0(wikipedia_url, .)

        # add link to artist_names_df
        row <- tibble(country_name, artist_name = NA, source_link = artist_names_link)
        artist_names_df <- artist_names_df %>%
            bind_rows(row)
    } else if (html_name(next_element) == "ul") {
        artist_names <- next_element %>%
            html_nodes("li") %>%
            html_text() %>%
            str_trim()

        # add names to artist_names_df
        for (artist_name in artist_names) {
            row <- tibble(country_name, artist_name, source_link = african_artists_url)
            artist_names_df <- artist_names_df %>%
                bind_rows(row)
        }
    }

    return(artist_names_df)
}

# specify dataframe to store names and links
artist_names_df <- tibble()

# loop through h2 elements and extract artist names/links
for (h2_element in h2_elements) {
    artist_names_df <- scrape_artist_names(h2_element, artist_names_df)
}

# subset to countries with no artist names
country_with_no_artists_df <- artist_names_df[is.na(artist_names_df$artist_name), ]

# extract links to countries with no artist names
country_with_no_artists_links <- country_with_no_artists_df$source_link
country_with_no_artists_names <- country_with_no_artists_df$country_name

# For these countries, the list af artist names isn't as clearly structured as the `List of African artists` page. Artist names are listed in <ul> <li> elements, but so is much more irrelevant information. We will need to manually specify a start index for the artist names, and an end index.

artist_name_ranges <- list(
    c("Abderrahmane Abdelli", "Zaho"),
    c("Ngola Ritmos", "Dog Murras"),
    c("Njacko Backo", "Sam Fan Thomas"),
    c("Abeti Masikini", "Youssoupha"),
    c("Abou El Leef", "Zizi Adel"),
    c("Aminé", "Gildo Kassa"),
    c("Guy Warren", "Philip Gbeho"),
    c("Akothee", "Wyre"),
    c("Soulful Dynamics", "Joe Woyee"),
    c("AmbondronA", "Nicolas Vatomanga"),
    c("Mouha Oulhoussein Achiban", "Malika Zarra"),
    c("2face Idibia", "Zoro"),
    c("Afrotraction", "DJ Zinhle"),
    c("Allan Toniks", "GNL Zamba"),
    c("Alick Nkhata", "Zone Fam"),
    c("Barura Express", "Mudiwa Hood")
)

# function to extract artist names from country links
scrape_artist_names_secondary <- function(index) {
    # define country name and URL
    country_name <- country_with_no_artists_names[[index]]
    country_url <- country_with_no_artists_links[[index]]

    # scrape all listed items from the HTML
    country_listed_items <- country_url %>%
        read_html() %>%
        html_nodes("ul li") %>%
        map(function(li_element) {
            return(li_element %>% html_node("a"))
        }) %>%
        map(html_text) %>%
        map(str_trim) %>%
        unlist()

    # specify range of indices for artist names
    country_range <- artist_name_ranges[[index]] %>%
        str_trim() %>%
        {
            which(country_listed_items %in% .)
        }

    # subset to artist names
    country_artist_names <- country_listed_items[min(country_range):max(country_range)]

    # create dataframe with valid country name and artist names
    country_artist_df <- tibble(
        country_name = country_name,
        artist_name = country_artist_names,
        source_link = country_url
    ) %>%
        # filter out reference links (see Ethiopia's list of artists)
        filter(!grepl("\\[\\d+\\]", artist_name))

    return(country_artist_df)
}

# specify dataframe to store artist names scraped from country links
artist_names_secondary_df <- tibble()

# loop through country links and extract artist names
for (index in seq_along(country_with_no_artists_links)) {
    artist_names_secondary_df <- artist_names_secondary_df %>%
        bind_rows(scrape_artist_names_secondary(index))
}

# combine artist names from both sources
artist_names_df <- artist_names_df %>%
    bind_rows(artist_names_secondary_df) %>%
    filter(!is.na(artist_name)) %>%
    arrange(country_name, artist_name) %>%
    distinct()

# inspect final output
View(artist_names_df)

# write to CSV
write.csv(artist_names_df, file = "data/artists/nationalities.csv", row.names = FALSE)
