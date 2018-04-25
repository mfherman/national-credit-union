## this script reads in, cleans up and selects relevant variables
## from the ncua call report files and combines into one master data frame

#### load libraries ####
library(tidyverse)
library(janitor)
library(lubridate)

# create look up table for type of membership field
tom_code_lookup <- tribble(
  ~tom_code, ~tom_descrip1, ~tom_descrip2,
  0L, "Community credit unions", "urban or rural, other than those designated low income",
  1L, "Associational", "religious, other than those designated low-income",
  2L, "Associational", "fraternal, other than those designated low-income",
  3L, "Associational", "other than religious or fraternal, or low-income",
  4L, "Educational", NA,
  5L, "Military", NA,
  6L, "Federal, state, local government", NA,
  10L, "Manufacturing", "chemicals",
  11L, "Manufacturing", "petroleum refining",
  12L, "Manufacturing", "primary and fabricated metals",
  13L, "Manufacturing", "machinery",
  14L, "Manufacturing", "transportation equipment",
  15L, "Manufacturing", "all other",
  20L, "Service", "finance, insurance, real estate, trade",
  21L, "Service", "health care",
  22L, "Service", "transportation",
  23L, "Service", "communications and utilities",
  24L, "Single common bond", "other",
  34L, "Multiple group", "primarily educational",
  35L, "Multiple group", "primarily military",
  36L, "Multiple group", "primarily federal, state, local government",
  40L, "Multiple group", "primarily chemical",
  41L, "Multiple group", "primarily petroleum refining",
  42L, "Multiple group", "primarily primary and fabricated metals",
  43L, "Multiple group", "primarily machinery",
  44L, "Multiple group", "primarily transportation equipment",
  49L, "Multiple group", "primarily other manufacturing",
  50L, "Multiple group", "primarily finance, insurance, real estate, trade",
  51L, "Multiple group", "primarily health care",
  52L, "Multiple group", "primarily transportation",
  53L, "Multiple group", "primarily communications and utilities",
  54L, "Multiple common bond", "other",
  66L, "Community credit unions", "rural district",
  98L, "Multiple group", "other",
  99L, "Non federal credit union", NA
  )

# read in foicu data and clean
foicu <- read_csv(
  "data/call-report-data-2017-12/foicu.txt",
  col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  select(
    cu_number,
    join_number,
    rssd,
    cu_type,
    cu_name,
    street,
    city,
    state,
    zip_code,
    ncua_region = region,
    issue_date,
    lid = limited_inc,
    peer_group,
    tom_code
  ) %>%
  separate(issue_date, into = "issue_date", sep = " ", extra = "drop") %>%
  mutate(
    lid = as.integer(lid),
    issue_date = mdy(issue_date),
    ncua_region = case_when(
      ncua_region == "1" ~ "Northeast",
      ncua_region == "2" ~ "Mid-Atlantic",
      ncua_region == "3" ~ "Southeast",
      ncua_region == "4" ~ "Midwest",
      ncua_region == "5" ~ "West",
      ncua_region == "8" ~ "ONES"
      ),
    peer_group = case_when(
      peer_group == "1" ~ "less than 2m",
      peer_group == "2" ~ "2m to 10m",
      peer_group == "3" ~ "10m to 50m",
      peer_group == "4" ~ "50m to 100m",
      peer_group == "5" ~ "100m to 500m",
      peer_group == "6" ~ "more than 500m"
      ),
    cu_type = case_when(
      cu_type == "1" ~ "fcu",
      cu_type == "2" ~ "fiscu",
      cu_type == "3" ~ "nfiscu"
      ),
    tom_code = as.integer(tom_code)
    ) %>%
  left_join(tom_code_lookup, by = "tom_code") %>%
  mutate(
    tom_rcd = 
      case_when(
        tom_code %in% c(0, 66) ~ "area",
        between(tom_code, 1, 3) ~ "associational",
        between(tom_code, 4, 53) ~ "employment",
        tom_code %in% c(54, 98) ~ "multiple",
        tom_code == 99 ~ "state"
        )
    )

# read in table with cdfi indicator var
cdfi_cu <- read_csv("data/all_cu.csv", col_types = cols(.default = "c")) %>%
  select(cu_number, cdfi, cdcu) %>%
  mutate(
    cdfi = if_else(cdfi == "1", 1L, 0L, missing = 0L),
    cdcu = if_else(cdcu == "1", 1L, 0L, missing = 0L)
    )

# read in ncua call report table by table, select vars, change to numeric
fs220 <- read_csv(
  "data/call-report-data-2017-12/fs220.txt",
  col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  select(
    cu_number,
    n_current_members = acct_083,
    n_potential_members = acct_084,
    tot_assets = acct_010,
    tot_deposits = acct_018,
    tot_non_member_deposit = acct_880,
    n_non_member_deposits = acct_457,
    tot_loans_leases = acct_025b,
    n_loans_leases = acct_025a
    ) %>%
  mutate_at(vars(-cu_number), as.double)

fs220a <- read_csv(
  "data/call-report-data-2017-12/fs220a.txt",
  col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  select(
    cu_number,
    n_accounts = acct_460,
    tot_interest_income = acct_115,
    tot_invest_income = acct_120,
    tot_trade_income = acct_124,
    tot_fee_income = acct_131,
    n_ft_empl = acct_564a,
    n_pt_empl = acct_564b,
    tot_net_worth = acct_997,
    ratio_net_worth = acct_998
    ) %>%
  mutate_at(vars(-cu_number), as.double)

fs220c <- read_csv(
  "data/call-report-data-2017-12/fs220c.txt",
  col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  select(
    cu_number,
    n_branches = acct_566
    ) %>%
  mutate_at(vars(-cu_number), as.double)

fs220d <- read_csv(
  "data/call-report-data-2017-12/fs220d.txt",
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  select(cu_number, member_minority_status:eligible_native_american) %>%
  mutate(
    mdi = if_else(
      member_minority_status == "1" & eligible_minority_status == "1",
      1L,
      0L
    )
  ) %>%
  mutate_at(vars(member_minority_status:eligible_native_american), as.integer)

# join all tables to one by cu_number
cu_all <- reduce(
  list(foicu, cdfi_cu, fs220, fs220a, fs220c, fs220d),
  left_join, by = "cu_number"
  ) %>%
  select(
    cu_number:issue_date,
    peer_group:tom_rcd,
    lid,
    cdfi,
    cdcu,
    mdi,
    member_minority_status:eligible_native_american,
    everything()
  )






tabyl(cu_all, mdi)
tabyl(cu_all, cdfi)
tabyl(cu_all, cdcu)
tabyl(cu_all, lid)
tabyl(cu_all, cu_type, tom_code)

cu_all %>%
  group_by(ncua_region) %>%
  summarise(
    n = n(),
    med_assets = median(tot_assets),
    mean_assets = mean(tot_assets)
    ) %>%
  arrange(desc(med_assets))


tabyl(cu_all, tom_descrip2) %>%
  arrange(desc(n))



cu_all %>% filter(ncua_region == "ONES")

acct_meta <- read_csv("data/call-report-data-2017-12/AcctDesc.txt")

foi_meta <- read_csv(
  "data/call-report-data-2017-12/FOICUDES.txt",
  col_types = cols(.default = "c")
  )


tabyl(cu_all, tom_code) %>%
  arrange(desc(n))

hist(cu_all$tom_code)

## distriubtion of national tom code

  ### overlap/differences between cdcu and cdfi
  ### 
  ### distribtuion by common bond type
  ### 
  ### join employment based, by separate labor unions  
  