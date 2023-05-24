# African Music Dataset Project

A very brief overview of the scripts present in this repository:

- `scrape_nationality_data` : scrapes Wikipedia pages for information on artist nationalities and compiles the information into a central dataset.
- `fetch_api_data` : collects all the track and artist data from the Spotify API for each artist in the nationality dataset.
- `parse_json` : some functions to extract metadata from the API responses in JSON format to tidy dataframes.
