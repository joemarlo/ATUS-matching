require(tidyverse)
require(rvest)

# import 2003-2018 files ----------------------------------------------------
# download data here: https://www.bls.gov/tus/datafiles-2018.htm
#  and store in inputs/ATUS-2018

dat.files <- list.files('inputs/ATUS-2003-2018', '*.dat')
files <- lapply(dat.files, function(file) read_csv(paste0("inputs/ATUS-2003-2018/", file)))
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
# TODO: review this
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
  't189.*',  'Other',
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
  16, 15, 150000
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

# scrape essential industries codes if it doesn't exist
if (!file.exists('data/essential_industries.csv')) {
  
  # get essential industries table from CDC
  url_CDC <- 'https://www.cdc.gov/vaccines/covid-19/categories-essential-workers.html'
  essential_industries <- read_html(url_CDC) %>% 
    html_table() %>%
    map(function(tbl) mutate(tbl, across(everything(), as.character))) %>% 
    bind_rows() %>% 
    setNames(c('NAICS_2017', 'name_2017', 'CISA_sector', 'vax_phase', 'workforce_category'))
  
  # expand the xxx codes to explicit codes
  essential_industries$NAICS_2017 <- stringr::str_replace_all(essential_industries$NAICS_2017, "x", ".")
  essential_industries$NAICS_2017 <- stringr::str_remove_all(essential_industries$NAICS_2017, "§")
  essential_industries$NAICS_2017 <- paste0("^", essential_industries$NAICS_2017, "$")
  NAICS_2017 <- readxl::read_xlsx('inputs/2017_NAICS_Index_File.xlsx')
  NAICS_2017 <- NAICS_2017 %>% select(-DESCRIPTION) %>% distinct()
  essential_industries <- fuzzyjoin::regex_left_join(NAICS_2017, essential_industries, by = 'NAICS_2017')
  essential_industries <- essential_industries %>% 
    na.omit() %>% 
    select(NAICS_2017 = NAICS_2017.x,
           essential_group = vax_phase) %>% 
    mutate(essential_group = recode(essential_group, "1a" = 1, "1b" = 2, "1c" = 3))
  
  # crosswalk with the 2017 Census Industry codes
  Census_2017 <- readxl::read_xlsx('inputs/crosswalk_2017_Census_to_2017_NAICS.xlsx')
  colnames(Census_2017) <- c("Description", "Census_2017", "Restriction", "NAICS_2017")
  Census_2017 <- Census_2017[, c('Census_2017', 'NAICS_2017')]
  Census_2017 <- na.omit(Census_2017)
  Census_2017 <- distinct(Census_2017)
  
  ## a common issue is that the NAICS code will be "3118 exc. 311811" so we're 
  ##   going to naively remove the exclusions
  Census_2017$NAICS_2017 <- stringr::str_remove_all(Census_2017$NAICS_2017, "[[:alpha:]].*$")
  
  ## unnest the codes. Where there are multiple codes in one row they will be converted to one per row
  Census_2017 <- tidytext::unnest_tokens(Census_2017, output = 'NAICS_2017', input = 'NAICS_2017')
  Census_2017 <- distinct(Census_2017)
  
  ## expand the xxx NAICS codes to explicit codes
  Census_2017$NAICS_2017 <- substr(paste0(Census_2017$NAICS_2017, "....."), 0, 6)
  Census_2017 <- fuzzyjoin::regex_left_join(NAICS_2017, Census_2017, by = 'NAICS_2017')
  Census_2017 <- dplyr::select(Census_2017, Census_2017, NAICS_2017 = NAICS_2017.x) %>% distinct()
  
  # crosswalk with the 2012 Census Industry codes
  Census_2012_changes <- readxl::read_xlsx('inputs/crosswalk_2017_Census_to_2012_Census.xlsx',
                                           col_types = 'text')
  Census_to_NAICS <- left_join(Census_2017, Census_2012_changes, by = 'Census_2017') %>% 
    mutate(Census_2012 = if_else(is.na(Census_2012), Census_2017, Census_2012))
  
  # join back with list of essential industries
  essential_industries <- left_join(Census_to_NAICS, essential_industries, by = 'NAICS_2017') %>% 
    mutate(essential_group = if_else(is.na(essential_group), 4, essential_group))
  essential_industries %>%
    group_by(Census_2012) %>% 
    summarize(non_perfect_match = n_distinct(essential_group)) %>% 
    pull(non_perfect_match) %>% 
    {table(.) / sum(table(.))}
  # this tells us that 90% of the 2012 Census codes match to on essential worker group
  # so we're going to map each Census_2012 group to the NAICS_2017 code (consequently essential_group)
  #   by its average match. This loses a lot of information; there doesn't seem to be a better solution
  essential_industries <- essential_industries %>% 
    group_by(Census_2012) %>% 
    summarize(essential_group = round(mean(essential_group)))

  # write out
  write_csv(essential_industries, 'data/essential_industries.csv')
  rm(essential_industries, url_CDC, NAICS_2017, Census_to_NAICS, Census_2012_changes)
}

# read in the data
essential_industries <- read_csv('data/essential_industries.csv',
                                 col_types = list(col_character(), col_character()))
