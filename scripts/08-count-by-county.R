library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

counties <- get_acs(
  geography = "county",
  variables = "B01001_001",
  survey = "acs5",
  year = 2016,
  geometry = TRUE,
  output = "wide"
  ) %>%
  separate(NAME, into = c("county", "state"), sep = ", ") %>%
  select(
    geoid = GEOID,
    county,
    state,
    pop_tot = "B01001_001E"
  ) %>%
  filter(state != "Puerto Rico") %>%
  st_transform(4326)

cu_office <- read_rds("data/geocode/office_geo_all.rds") %>%
  mutate_at(vars(lat, lon), as.double) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326)

# note: drops non-50 state credit unions
cu_county <- st_join(cu_office, counties, join = st_within, left = FALSE)

cu_county %>%
  st_set_geometry(NULL) %>%
  group_by(geoid) %>%
  summarise(
    n_cu = n()
    )
