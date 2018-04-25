### download basic census demographics and shapefiles for all counties in US
### join with nchs urban/rural and make a couple simple maps

library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(rmapshaper)
library(RColorBrewer)

options(tigris_use_cache = TRUE)

# total population B01001_001
# % non hispanic white B03002_003
# % non hispanic black B03002_004
# % non hispanic asian B03002_006
# % hispanic B03002_012
# median hh income B19013_001
# gini B19083_001

# select variables
variables <- c(
  "B01001_001",
  "B03002_003",
  "B03002_004",
  "B03002_006",
  "B03002_012",
  "B19013_001",
  "B19083_001"
  )

# download acs 5 year estimates by county
county_acs <- get_acs(
  geography = "county",
  variables = variables,
  survey = "acs5",
  year = 2016,
  geometry = TRUE,
  output = "wide",
  shift_geo = TRUE
  ) %>%
  separate(NAME, into = c("county", "state"), sep = ", ")

# percentage and select variables
county_stat <- county_acs %>%
  mutate(
    pop_other = B01001_001E - (B03002_003E + B03002_004E + B03002_012E + B03002_006E),
    white_pct = B03002_003E / B01001_001E * 100,
    black_pct = B03002_004E / B01001_001E * 100,
    hisp_pct = B03002_012E / B01001_001E * 100,
    asian_pct = B03002_006E / B01001_001E * 100,
    other_pct = 100 - (white_pct + black_pct + hisp_pct + asian_pct),
    ) %>%
  select(
    geoid = GEOID,
    county,
    state,
    pop_tot = B01001_001E,
    med_hhinc = B19013_001E,
    gini = B19083_001E,
    pop_white = B03002_003E,
    pop_black = B03002_004E,
    pop_hisp = B03002_012E,
    pop_asian = B03002_006E,
    pop_other,
    white_pct:other_pct
  ) %>%
  ms_simplify(keep = 0.07, keep_shapes = TRUE) # simplify geography for faster plotting

# check out a simple map
tm_shape(county_stat) +
  tm_fill(
    col = "med_hhinc",
    n = 5,
    style = "jenks"
  ) +
  tm_borders(col = "black", lwd = 0.3, alpha = 0.6)


# read in nhcs rural urban designations
nchs <- read_rds("data/nchs_desig.rds") %>%
  mutate(geoid = paste0(state_fips, county_fips)) %>%
  select(geoid, state, nchs_code, nchs_desc) %>%
  add_row(
    geoid = c("46102", "02158"),
    state = c("SD", "AK"),
    nchs_code = "6",
    nchs_desc = "Rural Area"
    )

# join nchs designations with county stats
county <- left_join(county_stat, nchs, by = "geoid")

# map of rural urban designations
tm_shape(county) +
  tm_fill(
    col = "nchs_desc",
    palette = rev(brewer.pal(6, "Accent")),
    title = "NCHS Urban/Rural\nClassification"
    ) +
  tm_shape(county) +
  tm_borders(col = "black", lwd = 0.3, alpha = 0.6)

write_rds(county, "data/county_stat.rds")
  
