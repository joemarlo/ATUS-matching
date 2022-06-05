####
# This script cleans the input data and outputs the relevant data to /data
###


library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
options(mc.cores = min(parallel::detectCores(), 6))

# functions
purrr:::walk(list.files('tmp/R', full.names = TRUE), source)


# essential industries ----------------------------------------------------

# data inputs
url_CDC <- 'https://www.cdc.gov/vaccines/covid-19/categories-essential-workers.html'
NAICS_2017 <- readxl::read_xlsx('inputs/2017_NAICS_Index_File.xlsx')
Census_2012_changes <- readxl::read_xlsx('inputs/crosswalk_2017_Census_to_2012_Census.xlsx',
                                         col_types = 'text')

# create dataframe of essential industries
essential_industries <- create_industries(url_CDC, NAICS_2017, Census_2012_changes)
readr::write_csv(essential_industries, 'data/essential_industries.csv')


# FIPS state codes --------------------------------------------------------

url_FIPS <- 'https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696'
FIPS <- create_fips(url_FIPS)
readr::write_csv(FIPS, 'data/FIPS.csv')


# core data ---------------------------------------------------------------

# ATUS codes
specific.codes <- readr::read_delim("inputs/specific_codes.csv", 
                             "+", escape_double = FALSE, trim_ws = TRUE)
simple.codes <- readr::read_delim("inputs/simple_codes.csv", 
                           "+", escape_double = FALSE, trim_ws = TRUE)

# ATUS files
atus_files <- read_atus()
atussum_0320 <- atus_files$atussum_0320
atusrost_0320 <- atus_files$atusrost_0320
atuscps_0320 <- atus_files$atuscps_0320
atusact_0320 <- atus_files$atusact_0320
atusresp_0320 <- atus_files$atusresp_0320
rm(atus_files)

# all codes matched to description
descriptions <- atussum_0320 %>% 
  select(matches('^t[0-9].')) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = 'activity') %>%
  select(activity) %>% 
  distinct() %>% 
  fuzzyjoin::regex_left_join(y = curated.codes) %>% 
  select(activity = activity.x, description)

# add indicator for work day to summary file
atussum_0320 <- atussum_0320 %>% 
  select(contains('t05')) %>% 
  rowSums() %>% 
  tibble::enframe() %>% 
  mutate(work.status = value >= 120) %>% 
  select(work.status) %>% 
  bind_cols(atussum_0320)

readr::write_tsv(atussum_0320, 'data/atussum_0320.tsv')


# clean ATUS with childcare -----------------------------------------------

data_atus <- clean_atus(atusact_0320)
readr::write_tsv(data_atus$ATUS_1, 'data/atus_SSC.tsv')
readr::write_tsv(data_atus$ATUS_30, 'data/atus_30min_SSC.tsv')


# clean demographics ------------------------------------------------------

demographic_vars <- clean_demographics(
  data_atus$ATUS30, 
  atusrost_0320, 
  atuscps_0320, 
  atussum_0320, 
  atusresp_0320
)
readr::write_tsv(demographic_vars, 'data/demographic.tsv')
