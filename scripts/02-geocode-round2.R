## this script selects the branches that didn't geocode the first time
## writes them out to csv for manual seraching and then read back in
## and geocode again with new clean address via google

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
      CU_NUMBER,
      SiteId,
      lon,
      lat,
      loctype
    ) %>%
    mutate_all(as.character)
}

# read in geocoded branches and combine into 1 df
branches_geo <- paste0("data/geocode/geo_goog_", 0:8, ".rds") %>%
  map_df(read_in_geo) 

# read in orginal branch data
us_branches <- read_csv(
  "data/call-report-data-2017-12/Credit Union Branch Information.txt",
  col_types = cols(.default = "c")
  ) %>%
  filter(
    PhysicalAddressCountry == "United States" &
      !(PhysicalAddressStateCode %in% c("GU", "PR", "VI"))
  )

# append geocoded data to branch data
branches_all <- branches_geo %>%
  left_join(us_branches, by = c("CU_NUMBER", "SiteId"))

# select the branches that didn't geocode properly and write to csv
# maybe i can manually reformat the address and try again to geocode
branches_no_geo <- branches_all %>%
  filter(is.na(lat)) %>%
  select(
    CU_NUMBER,
    SiteId,
    CU_NAME,
    PhysicalAddressLine1:MailingAddressPostalCode
  )

write_csv(branches_no_geo, "output/no_geo.csv")

# i manually searched those credit union branches that didn't geocode
# and gave them new addresses that the google geocoder should handle
# let's read that back in and then try to geocode those branches
branches_to_regeo <- read_csv(
  file = "output/no_geo.csv",
  col_types = cols(.default = "c")
  ) %>%
  select(
    CU_NUMBER,
    SiteId,
    addr_new
    )

# geocode via google maps
branches_regeo <- mutate_geocode(
  data = branches_to_regeo,
  location = addr_new,
  output = "more",
  source = "google"
  ) 

# select columns to join with 1st geocoded branches
re_geo_to_join <- branches_regeo %>%
  select(
    CU_NUMBER,
    SiteId,
    lon,
    lat,
    loctype
  ) %>%
  mutate_all(as.character)

# join back up with the properly geocoded batch
branches_geo_round2 <- branches_geo %>%
  filter(!is.na(lat)) %>%
  bind_rows(re_geo_to_join)

branches_geo_all <- branches_all %>%
  select(
   CU_NUMBER,
   SiteId,
   JOIN_NUMBER,
   CU_NAME,
   SiteName,
   SiteTypeName,
   MainOffice
   ) %>%
  left_join(branches_geo_round2, by = c("CU_NUMBER", "SiteId")) %>%
  clean_names()

### yayaya, we have all the branches in the us geocoded!!
write_rds(branches_all_geo, "data/geocode/branches_geo_all.rds")





