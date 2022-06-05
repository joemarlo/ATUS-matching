# require(tidyverse)
# require(rvest)

# import 2003-2018 files ----------------------------------------------------
# download data here: https://www.bls.gov/tus/#data
#  and store in inputs/ATUS-2003-2018

read_atus <- function(){
  dat.files <- list.files(file.path('inputs', 'ATUS-2003-2020'), pattern = '*.dat')
  files <- lapply(dat.files, function(file) readr::read_csv(file.path("inputs", "ATUS-2003-2020", file)))
  names(files) <- stringr::str_remove(dat.files, ".dat")
  # list2env(files, envir = .GlobalEnv)
  # rm(files, dat.files)
  # rm(atusrost_0318, atuswgts_0318)
  
  return(files)
}


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

gc()