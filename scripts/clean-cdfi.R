## read in and clean cdfi spreadsheet to join with ncua call report data

library(tidyverse)
library(readxl)
library(janitor)

cdfi_cu <- read_excel("data/cdfi_2018.xlsx", col_types = "text", skip = 4) %>%
  clean_names() %>%
  filter(financial_institution_type == "Credit Union") %>%
  mutate(
    zipcode = str_remove(zipcode, "-.*"),
    cu_name = str_to_upper(organization_name)
    ) %>%
  select(cu_name, address1, city, state, zipcode) %>%
  write_csv("data/cdfi_cu.csv")

### mh manually compared this file to the ncua credit union list because names don't match perfectly

## read in manually coded credit unions with cdfi indicator columns
cdfi_all <- read_csv("data/all_cu.csv", col_types = cols(.default = "c"))