## this script selects the main offices that didn't geocode the first time
## and tries to re-geocode just using zip

#### load libraries ####
library(tidyverse)
library(ggmap)

# add google api key for better geocoding
register_google(
  key = Sys.getenv("GOOGLE_API_KEY"),
  account_type = "standard"
)

# define function to read in and select columns of geocoded cus
read_in_geo <- function(x) {
  read_rds(x) %>%
    select(
      cu_number,
      cu_name,
      zip_code,
      lon,
      lat
    ) %>%
    mutate_all(as.character)
}

# read in geocoded branches and combine into 1 df
office_geo <- paste0("data/geocode/geo_main_", 0:2, ".rds") %>%
  map_df(read_in_geo) 

# select the branches that didn't geocode properly
office_no_geo <- office_geo %>%
  filter(is.na(lat)) %>%
  select(-(lon:lat))

# now let's just geocode based on zip -- should be good enough for our purposes
office_regeo <- mutate_geocode(
  data = office_no_geo,
  location = zip_code,
  output = "latlona",
  source = "google"
) 

# select columns to join with 1st geocoded branches
re_geo_to_join <- office_regeo %>%
  select(-address) %>%
  mutate_all(as.character)

# join back up with the properly geocoded batch
office_all_geo <- office_geo %>%
  filter(!is.na(lat)) %>%
  bind_rows(re_geo_to_join) %>%
  select(cu_number, cu_name, lon, lat)

### yayaya, we have all the branches in the us geocoded!!
write_rds(office_all_geo, "data/geocode/office_geo_all.rds")





