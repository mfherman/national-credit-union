## this script loads in all cu branches from ncua, cleans up the files
## and geocodes each branch via both dstk and google
## google api limits geocodes to 2500 per day, so file is split
## into chunks of 2450 address -- geocode 1 chunk per day
## dstk geocoder does not have a daily limit, but is less reliable
## than google geocoder

#### load libraries ####
library(tidyverse)
library(ggmap)

# add google api key for better geocoding
register_google(
  key = Sys.getenv("GOOGLE_API_KEY"),
  account_type = "standard"
)

#### data import ####
# read in all cu branch data, remove non-US branches and guam, pr, virgin islands
branches <- read_csv(
  "data/call-report-data-2017-12/Credit Union Branch Information.txt",
  col_types = cols(.default = "c")
) %>%
  filter(
    PhysicalAddressCountry == "United States" &
      !(PhysicalAddressStateCode %in% c("GU", "PR", "VI"))
  )

# create long form address to geocode
branches_to_geocode <- branches %>%
  mutate(addr_long = paste0(PhysicalAddressLine1, ", ",
                            PhysicalAddressCity, ", ",
                            PhysicalAddressStateCode, " ",
                            PhysicalAddressPostalCode, " USA")) %>%
  select(CU_NUMBER, SiteId, CU_NAME, addr_long)

# create chunks of 2,450 address to geocode -- 1 chunk per day for google
geo_chunked <- split(branches_to_geocode, (seq(nrow(branches_to_geocode)) - 1) %/% 2450)

#### dstk geocode #### 
# geocode chunks and save each chunk
geo_0 <- mutate_geocode(
  data = geo_chunked[["0"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "dsk"
)
write_rds(geo_0, "data/geocode/geo_0.rds")

geo_1 <- mutate_geocode(
  data = geo_chunked[["1"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "dsk"
)
write_rds(geo_1, "data/geocode/geo_1.rds")

geo_2 <- mutate_geocode(
  data = geo_chunked[["2"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "dsk"
)
write_rds(geo_2, "data/geocode/geo_2.rds")

geo_3 <- mutate_geocode(
  data = geo_chunked[["3"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "dsk"
)
write_rds(geo_3, "data/geocode/geo_3.rds")

geo_4 <- mutate_geocode(
  data = geo_chunked[["4"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "dsk"
)
write_rds(geo_4, "data/geocode/geo_4.rds")

geo_5 <- mutate_geocode(
  data = geo_chunked[["5"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "dsk"
)
write_rds(geo_5, "data/geocode/geo_5.rds")

geo_6 <- mutate_geocode(
  data = geo_chunked[["6"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "dsk"
)
write_rds(geo_6, "data/geocode/geo_6.rds")

geo_7 <- mutate_geocode(
  data = geo_chunked[["7"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "dsk"
)
write_rds(geo_7, "data/geocode/geo_7.rds")

geo_8 <- mutate_geocode(
  data = geo_chunked[["8"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "dsk"
)
write_rds(geo_8, "data/geocode/geo_8.rds")

# take out extra postal_code column
geo_1 <- geo_1 %>%
  select(-postal_code)

geo_6 <- geo_6 %>%
  select(-postal_code)

geo_8 <- geo_8 %>%
  select(-postal_code)

# bind geocoded chunks into dataframe
geocode_return <- bind_rows(
  geo_0, geo_1, geo_2, geo_3, geo_4,
  geo_5, geo_6, geo_7, geo_8
)



write_rds(geocode_return, "data/geocode/all_geo_dstk.rds")
geocode_return <- read_rds("data/geocode/all_geo_dstk.rds")

#### google geocode #### 
# do it again with google maps geocoder
geo_goog_0 <- mutate_geocode(
  data = geo_chunked[["0"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_goog_0, "data/geocode/geo_goog_0.rds")

geo_goog_1 <- mutate_geocode(
  data = geo_chunked[["1"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_goog_1, "data/geocode/geo_goog_1.rds")

geo_goog_2 <- mutate_geocode(
  data = geo_chunked[["2"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_goog_2, "data/geocode/geo_goog_2.rds")

geo_goog_3 <- mutate_geocode(
  data = geo_chunked[["3"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_goog_3, "data/geocode/geo_goog_3.rds")

geo_goog_4 <- mutate_geocode(
  data = geo_chunked[["4"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_goog_4, "data/geocode/geo_goog_4.rds")

geo_goog_5 <- mutate_geocode(
  data = geo_chunked[["5"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_goog_5, "data/geocode/geo_goog_5.rds")

geo_goog_6 <- mutate_geocode(
  data = geo_chunked[["6"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_goog_6, "data/geocode/geo_goog_6.rds")

geo_goog_7 <- mutate_geocode(
  data = geo_chunked[["7"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_goog_7, "data/geocode/geo_goog_7.rds")

geo_goog_8 <- mutate_geocode(
  data = geo_chunked[["8"]],
  location = addr_long,
  output = "more",
  override_limit = TRUE,
  source = "google"
)
write_rds(geo_goog_8, "data/geocode/geo_goog_8.rds")

