## this script loads in all cu main offices from ncua call report,
## cleans up the files and geocodes each branch via google
## google api limits geocodes to 2500 per day, so file is split
## into chunks of 2450 address -- geocode 1 chunk per day

#### load libraries ####
library(tidyverse)
library(ggmap)
library(janitor)
library(lubridate)

# add google api key for better geocoding
register_google(
  key = Sys.getenv("GOOGLE_API_KEY"),
  account_type = "standard"
)

# read in foicu data and clean up to geocode
foicu <- read_csv(
  "data/call-report-data-2017-12/foicu.txt",
  col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  mutate(
    addr_long = paste0(
      street, ", ",
      city, ", ",
      state, " ",
      zip_code, " USA")
    ) %>%
  select(
    cu_number,
    cu_name,
    addr_long,
    city,
    state,
    zip_code
  )

# create chunks of 2,450 address to geocode -- 1 chunk per day for google
geo_chunked <- split(foicu, (seq(nrow(foicu)) - 1) %/% 2450)

#### google geocode #### 
# geocode chunks and save each chunk
geo_0 <- mutate_geocode(
  data = geo_chunked[["0"]],
  location = addr_long,
  output = "latlona",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_0, "data/geocode/geo_main_0.rds")

geo_1 <- mutate_geocode(
  data = geo_chunked[["1"]],
  location = addr_long,
  output = "latlona",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_1, "data/geocode/geo_main_1.rds")

geo_2 <- mutate_geocode(
  data = geo_chunked[["2"]],
  location = addr_long,
  output = "latlona",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_2, "data/geocode/geo_main_2.rds")
