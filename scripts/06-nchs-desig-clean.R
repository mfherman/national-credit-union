### this little script reads the 2013 nchs county urban/rural
### classification codes and gets them cleaned up to join with census data
### file from ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/OAE/urbanrural/NCHSURCodes2013.txt
### more info here: https://www.cdc.gov/nchs/data_access/urban_rural.htm

library(tidyverse)

nchs_desig <- read_fwf(
  "data/nchs-code-2013.txt",
  fwf_cols(
    state_fips = c(1, 2),
    county_fips = c(3, 5),
    state = c(7, 8),
    county = c(10, 45),
    cbsa = c(47, 96),
    cbsa_pop = c(98, 105),
    county_pop = c(107, 114),
    nchs_2013 = c(116, 116),
    nchs_2006 = c(118, 118),
    nchs_1990 = c(120)
  ),
  col_types = cols(.default = "c")
) %>%
  select(state_fips:county, nchs_code = nchs_2013) %>%
  mutate(
    nchs_desc = 
      factor(
        case_when(
          nchs_code == "1" ~ "Urban Center",
          nchs_code == "2" ~ "Suburban",
          nchs_code == "3" ~ "Medium City",
          nchs_code == "4" ~ "Small City",
          nchs_code == "5" ~ "Town",
          nchs_code == "6" ~ "Rural Area"
          ),
        levels = c(
          "Urban Center",
          "Suburban",
          "Medium City",
          "Small City",
          "Town",
          "Rural Area"
          ),
        ordered = TRUE
        )
    )

write_rds(nchs_desig, "data/nchs_desig.rds")
