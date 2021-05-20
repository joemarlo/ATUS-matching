require(tidyverse)
require(rvest)

# import 2003-2018 files ----------------------------------------------------
# download data here: https://www.bls.gov/tus/#data
#  and store in inputs/ATUS-2003-2018

dat.files <- list.files(file.path('inputs', 'ATUS-2003-2018', '*.dat'))
files <- lapply(dat.files, function(file) read_csv(file.path("inputs", "ATUS-2003-2018", file)))
names(files) <- str_remove(dat.files, ".dat")
list2env(files, envir = .GlobalEnv)
rm(files, dat.files)


# import field labels -------------------------------------------------------------
# also see: https://www.bls.gov/tus/lexiconwex2018.pdf

specific.codes <- read_delim("inputs/specific_codes.csv", 
                             "+", escape_double = FALSE, trim_ws = TRUE)

simple.codes <- read_delim("inputs/simple_codes.csv", 
                           "+", escape_double = FALSE, trim_ws = TRUE)

# custom mapping for activities
curated.codes <- tribble(
  ~activity, ~description,
  't0101.*',  'Sleep',
  't010[2-9].*','Personal Care',
  't019.*',   'Personal Care',
  't1801.*',  'Personal Care',
  't02.*',    'Household Activities',
  't1802.*',  'Household Activities',
  't03.*',    'Caring For Household Member',
  't1803.*',  'Caring For Household Member',
  't04.*',    'Caring For Nonhousehold Members',
  't1804.*',  'Caring For Nonhousehold Members',
  't05.*',    'Work',
  't1805.*',  'Work',
  't06.*',    'Education',
  't1806.*',  'Education',
  't07.*',    'Consumer Purchases',
  't1807.*',  'Consumer Purchases',
  't08.*',    'Professional & Personal Care Services',
  't1808.*',  'Professional & Personal Care Services',
  't09.*',    'Other',
  't1809.*',  'Other',
  't10.*',    'Other',
  't1810.*',  'Other',
  't11.*',    'Eating and Drinking',
  't1811.*',  'Eating and Drinking',
  't12.*',    'Socializing, Relaxing, and Leisure',
  't1812.*',  'Socializing, Relaxing, and Leisure',
  't13.*',    'Sports, Exercise, and Recreation',
  't1813.*',  'Sports, Exercise, and Recreation',
  't14.*',    'Religious and Spiritual',
  't1814.*',  'Religious and Spiritual',
  't15.*',    'Volunteer',
  't1815.*',  'Volunteer',
  't16.*',    'Other',
  't1816.*',  'Other',
  't1818.*',  'Other',
  't1819.*',  'Other',
  't189.*',   'Other',
  't50.*',    'Other'
)

# all codes matched to description
descriptions <- atussum_0318 %>% 
  select(matches('^t[0-9].')) %>% 
  pivot_longer(cols = everything(), names_to = 'activity') %>%
  select(activity) %>% 
  distinct() %>% 
  fuzzyjoin::regex_left_join(y = curated.codes) %>% 
  select(activity = activity.x, description)


# additions to data -------------------------------------------------------

# add indicator for work day to 2003-2018 summary file
atussum_0318 <- atussum_0318 %>% 
  select(contains('t05')) %>% 
  rowSums() %>% 
  enframe() %>% 
  mutate(work.status = value >= 120) %>% 
  select(work.status) %>% 
  bind_cols(atussum_0318)


# income ------------------------------------------------------------------

# family income CPS data is HEFAMINC
# HUFAMINC: >=2010
# HEFAMINC: <2010
income.levels <- tribble(
  ~HUFAMINC, ~HEFAMINC, ~HH.income.new,
  1, 1, 0,
  2, 2, mean(5000, 7499),
  3, 3, mean(7500, 9999),
  4, 4, mean(10000, 12499),
  5, 5, mean(12500, 14999),
  6, 6, mean(15000, 19999),
  7, 7,mean(20000, 24999),
  8, 8, mean(25000, 29999),
  9, 9, mean(30000, 34999),
  10, 10, mean(35000, 39999),
  11, 11, mean(40000, 49999),
  12, 12, mean(50000, 59999),
  13, 13, mean(60000, 74999),
  14, 14, mean(75000, 99999),
  15, 15, mean(100000, 149999),
  16, 16, 150000
)


# FIPS state codes --------------------------------------------------------

# scrape FIPS state codes if it doesn't exist
if (!file.exists('inputs/FIPS.csv')) {
  xml2::read_html('https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696') %>% 
  rvest::html_nodes(xpath = '//table[contains(@class, "data")]') %>% 
  rvest::html_table() %>% 
  .[[1]] %>% 
  rename(State = 'Postal Code') %>% 
  write_csv('inputs/FIPS.csv')
}

# read in the data
FIPS <- read_csv('inputs/FIPS.csv')


# state regions -----------------------------------------------------------

# read in the data
state.regions <- read_csv('inputs/state_regions.csv')


# essential industries ----------------------------------------------------

# read in the data
essential_industries <- read_csv('data/essential_industries.csv',
                                 col_types = list(col_character(), col_character()))
