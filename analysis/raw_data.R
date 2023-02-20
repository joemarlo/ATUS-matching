## raw data request from Marc ##
## run matching.R until line 63

dem <- demographics_prepped |> 
  select(ID, year, has_partner, sex) |> 
  left_join(select(respondents_with_children, ID, primary_childcare, secondary_childcare),
            by = 'ID')

dem |> 
  filter(year == 2019) |> 
  pull(primary_childcare) -> PCC_2019
dem |> 
  filter(year == 2019) |> 
  pull(secondary_childcare) -> SCC_2019
dem |> 
  filter(year == 2019) |> 
  pull(has_partner) -> partner_2019
dem |> 
  filter(year == 2019) |> 
  pull(sex) -> sex_2019

dem |> 
  filter(year == 2020) |> 
  pull(primary_childcare) -> PCC_2020
dem |> 
  filter(year == 2020) |> 
  pull(secondary_childcare) -> SCC_2020
dem |> 
  filter(year == 2020) |> 
  pull(has_partner) -> partner_2020
dem |> 
  filter(year == 2020) |> 
  pull(sex) -> sex_2020

SCC_diff <- readRDS("SCC_diff")
PCC_diff <- readRDS("PCC_diff")

save(list = c("PCC_2019", "SCC_2019", "partner_2019", "PCC_2020", "SCC_2020", "partner_2020", "SCC_diff", "PCC_diff"),
     file = 'raw_data.RData')

load("raw_data.Rdata")

plot(density(SCC_2019))
lines(density(SCC_2020))

load('sex.Rdata')
save(list = c('sex_2019', 'sex_2020', 'PCC_diff_2019', 'SCC_diff_2019'),
     file = 'raw_data_update.RData')

# create raw data 3 with the 2021 versions
dem |> 
  filter(year == 2021) |> 
  pull(primary_childcare) -> PCC_2021
dem |> 
  filter(year == 2021) |> 
  pull(secondary_childcare) -> SCC_2021
dem |> 
  filter(year == 2021) |> 
  pull(has_partner) -> partner_2021
dem |> 
  filter(year == 2021) |> 
  pull(sex) -> sex_2021

# need to run matching.R through line 145
childcare_pairs_diffs |> group_by(pair_id) |> filter(row_number() == 1) |> pull(diff) -> SCC_diff_2021
childcare_pairs_diffs |> group_by(pair_id) |> filter(row_number() == 1) |> pull(diff) -> PCC_diff_2021

save(list = c("PCC_2021", "SCC_2021", "partner_2021", "SCC_diff_2021", "PCC_diff_2021", "sex_2021"),
     file = 'raw_data_2021.RData')
load("raw_data_2021.Rdata")
