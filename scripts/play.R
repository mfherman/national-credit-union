#### load libraries ####
library(tidyverse)
library(sf)
library(tmap)
library(mapview)

#### maps ####
branches_sf <- read_rds("data/geocode/branches_geo_all.rds") %>%
  mutate_at(vars(lat, lon), as.double) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  mutate_at(vars(site_name), funs(iconv(., "UTF-8", "UTF-8")))

mapview(branches_sf)

us_states <- albersusa::usa_sf() %>%
  filter(!(name %in% c("Alaska", "Hawaii")))

tmap_mode("view")
map_cu <- branches_sf %>%
  st_transform(2163) %>%
  tm_shape() +
  tm_dots(alpha = 0.4, size = 0.025, col = "navy") +
  tm_shape(us_states) +
  tm_polygons(alpha = 0, col = "darkgray") +
  tm_add_legend(
    type = "symbol",
    labels = " 1 dot = 1 Credit Union Branch",
    size = 0.05,
    shape = 19,
    col = "navy"
  ) +
  tm_layout(
    main.title = "Credit Union Branches in the United States, 2017",
    main.title.size = 1,
    main.title.position = "center",
    frame = FALSE
  )


acct_meta <- read_csv("data/call-report-data-2017-12/AcctDesc.txt")

foi_meta <- read_csv("data/call-report-data-2017-12/FOICUDES.txt", col_types = cols(.default = "c"))

foicu <- read_csv("data/call-report-data-2017-12/foicu.txt", col_types = cols(.default = "c")) %>%
  mutate(CU_NUMBER = as.integer(CU_NUMBER))

fs220 <- read_csv("data/call-report-data-2017-12/fs220.txt", col_types = cols(.default = "c"))

fs220d <- read_csv("data/call-report-data-2017-12/fs220d.txt", col_types = cols(.default = "c"))

mdi <- read_csv("data/mdi-ncua2.csv", col_types = cols(.default = "c")) %>%
  janitor::clean_names() %>%
  filter(region %in% c("1", "2", "3", "4", "5")) %>%
  separate(
    minority_type,
    into = c("minority1", "minority2", "minority3", "minority4"),
    sep = ", ",
    fill = "right"
    ) %>%
  mutate(
    mdi = 1L,
    black = case_when(
      (minority1 == "Black American" |
        minority2 == "Black American" |
        minority3 == "Black American" |
        minority4 == "Black American")
      ~ 1L,
      TRUE ~ 0L
    ),
    asian = case_when(
      (minority1 == "Asian American" |
         minority2 == "Asian American" |
         minority3 == "Asian American" |
         minority4 == "Asian American")
      ~ 1L,
      TRUE ~ 0L
    ),
    hispanic = case_when(
      (minority1 == "Hispanic American" |
         minority2 == "Hispanic American" |
         minority3 == "Hispanic American" |
         minority4 == "Hispanic American")
      ~ 1L,
      TRUE ~ 0L
    ),
    native = case_when(
      (minority1 == "Native American" |
         minority2 == "Native American" |
         minority3 == "Native American" |
         minority4 == "Native American")
      ~ 1L,
      TRUE ~ 0L
    ),
    cu_number = as.integer(cu_number)
  ) %>%
  select(region, cu_number, cu_name, mdi:native)

hi <- inner_join(foicu, mdi, by = c("CU_NUMBER" = "cu_number"))

non_match <- mdi %>%
  filter(!(cu_number %in% hi$CU_NUMBER)) %>%
  mutate(cu_number = as.integer(cu_number))

inner_join(non_match, foicu, by = c("cu_number" = "CU_NUMBER"))

mdi %>%
  filter(region == "1")

fs220d %>%
  select(matches("eligible|minority|member")) %>%
  count(MemberMinorityStatus)

fs220d %>%
  select(matches("eligible|minority|member")) %>%
  count(MemberAfricanAmerican)


fs220d %>%
  select(matches("eligible|minority|member")) %>%
  count(EligibleMinorityStatus)

foicu %>%
  count(LIMITED_INC)

foicu %>%
  select(CU_NUMBER, RSSD, CU_NAME, STREET, CITY, STATE, ZIP_CODE)

library(tidyverse)
library(readxl)
library(janitor)
