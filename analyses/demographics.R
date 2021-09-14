library(tidyverse)
library(patchwork) # for stitching together multiple plots
source('analyses/plots/ggplot_settings.R')
set.seed(44)

# read in the demographics data
demographics <- read_delim(file = file.path("data", "demographic.tsv"),
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           col_types = cols(metropolitan = col_character()))


# data cleaning -----------------------------------------------------------

# set time periods if not in batch mode
if (!isTRUE(get0('in_batch_mode'))){
  time1 <- 2019
  time2 <- time1 + 1
}

# for 2019:2020, throw out observations who interview was before May 10th
# this is when the data collection started again
# if (time1 == 2019){
  # read in data with diary date
  atusresp_0320 <- read_csv(file.path("inputs", "ATUS-2003-2020", "atusresp_0320.dat"))
  
  # filter to just 2019:2020 data occurring in May or later
  post_may_respondents <- atusresp_0320 %>% 
    mutate(dairy_date = lubridate::ymd(TUDIARYDATE),
           dairy_month = lubridate::month(dairy_date)) %>% 
    filter(TUYEAR %in% c(time1, time2),
           dairy_month >= 5) %>% 
    select(ID = TUCASEID, year = TUYEAR)
  
  # filter demographics to only include this observations
  demographics <- inner_join(demographics, 
                             post_may_respondents, 
                             by = c('ID', 'year'))
  rm(post_may_respondents)
# }


# filter to just time1 and time2, just weekdays and non-holidays
boolean <- demographics$year %in% c(time1, time2) &
  demographics$day_of_week %in% 2:6 &
  demographics$holiday == 0
demographics <- demographics[boolean,]
rm(boolean)

# factorize education
demographics$education <- factor(
  demographics$education,
  levels = c(
    'Did not graduate from HS',
    'HS',
    'Some college',
    'Bachelors',
    'Masters',
    'Doctoral'
  )
)

# recode sex
demographics$sex <- recode(demographics$sex, `1` = 'male', `2` = 'female')

# add geographic region
state_regions <- read_csv("inputs/state_regions.csv")
colnames(state_regions) <- tolower(colnames(state_regions))
demographics <- left_join(demographics, y = state_regions, by = 'state')
rm(state_regions)

# replace age_youngest NAs with 0s (these are NA b/c they don't have a child)
demographics$age_youngest[is.na(demographics$age_youngest)] <- 0

# replace partner_working NA with 'NA' (b/c matching)
demographics$partner_working[is.na(demographics$partner_working)] <- 'NA'

# replace metropolitan NA with 'NA' (b/c matching)
# demographics$metropolitan[is.na(demographics$metropolitan)] <- 'NA'

# add indicator if child in household
demographics$child_in_HH <- demographics$n_child > 0

# TODO: should we remove these observations?
sum(is.na(demographics$fam_income))
# do we need impute values for the top-coded income? https://www.epi.org/data/methodology/

# essential worker percentage
# ggplot(demographics, aes(x = essential_industry, group = 1)) +
#   geom_histogram(color = 'white') +
#   labs(title = "Count of workers in essential industries",
#        subtitle = paste0(time1, "/", time2, ' data; 4 = No CDC classification; 5 = No job'),
#        x = "Essential worker group",
#        y = 'n')
# ggsave('analyses/plots/essential_workers.png', height = 6, width = 9)


# demographic var selection -----------------------------------------------

# matching: follow this but try a few different (SES is primary goal)
# hard match: gender, race, urban/rural, region, partnership, essential worker status, labor force status
# soft match: age, income, number of children, education (+/- 1); is there  relative within X miles, is spouse working
# covariate: is there an elder in the household; **health**, neighborhood attributes (eg crime), length of commute (or public transport?)

matching_vars <- c('age', 'sex', 'race', 'fam_income', 'has_partner', 
                   'education', 'child_in_HH', 'n_child', 'age_youngest', 'region', 
                   'partner_working', 'elder_in_HH', 'metropolitan', 'labor_force_status')
demographics <- demographics[, c('ID', 'year', matching_vars)]

# remove NAs b/c income/metropolitan unknown for 13%
demographics <- na.omit(demographics)


# set blocking variables --------------------------------------------------

# blocking_vars <- c('sex', 'race', 'metropolitan', 'region', 'has_partner', 'labor_force_status') #'essential_worker # add child_in_HH?
blocking_vars <- c('sex', 'race', 'metropolitan', 'has_partner') #'essential_worker
# demographics %>% group_by_at(all_of(blocking_vars)) %>% tally() %>% arrange(desc(n)) %>% View

# create list of numeric vars
vars_numeric <- demographics %>% 
  dplyr::select(all_of(matching_vars)) %>% 
  dplyr::select(where(is.numeric)) %>% 
  colnames()


# overlap -----------------------------------------------------------------

# histograms showing overlaps
# overlap_continuous <- demographics %>%
#   select(-ID) %>% 
#   dplyr::select_if(is.numeric) %>%
#   pivot_longer(cols = -year) %>%
#   ggplot(aes(x = value, fill = as.factor(year))) +
#   geom_histogram(alpha = 0.7, position = 'identity') +
#   facet_wrap(~name, scales = 'free') +
#   labs(title = "Overlap of key demographic variables by year",
#        subtitle = "These data are before any matching is performed",
#        x = NULL,
#        fill = NULL) +
#   theme(plot.background = element_rect(color = NA))
# overlap_categorical <- demographics %>% 
#   dplyr::select(where(~!is.numeric(.x))) %>%
#   mutate(across(everything(), as.character)) %>% 
#   bind_cols(year = demographics$year) %>% 
#   pivot_longer(cols = -year) %>%
#   ggplot(aes(x = value, fill = as.factor(year))) +
#   geom_bar(alpha = 0.7, position = 'dodge') +
#   facet_wrap(~name, scales = 'free') +
#   labs(title = NULL,
#        x = NULL,
#        fill = NULL) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.background = element_rect(color = NA))
# overlap_continuous / overlap_categorical + plot_layout(heights = c(2, 3))
# ggsave('analyses/plots/overlap_raw.png', height = 12, width = 9)


# basic stats -------------------------------------------------------------

# counts by group
# demographics %>% 
#   group_by(year) %>% 
#   tally() %>% 
#   mutate(year = as.character(year)) %>% 
#   add_row(tibble(year = 'Total',
#                  n = sum(.$n))) %>% 
#   write_csv('data/summary_stats_raw_n.csv')


# sizing up key population ------------------------------------------------

# how big are our groups
# demographics %>% 
#   filter(sex == 'female') %>% 
#   mutate(age = cut(age, breaks = c(0, 18, 60, 100))) %>% 
#   group_by(year, age, race) %>% 
#   tally() %>% 
#   arrange(race, age, year)
