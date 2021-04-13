library(tidyverse)
library(patchwork) # for stiching together multiple plots
source('analyses/plots/ggplot_settings.R')
set.seed(44)

# read in the demographics data
demographics <- read_delim(file = "data/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           col_types = cols(metropolitan = col_character()))


# data cleaning -----------------------------------------------------------

# filter to just 2007 and 2009, just weekdays and non-holidays
boolean <- demographics$year %in% c(2007, 2009) &
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

# replace partner_working NA with 'NA' (b/c propensity score matching)
demographics$partner_working[is.na(demographics$partner_working)] <- 'NA'

# replace metropolitan NA with 'NA' (b/c propensity score matching)
# demographics$metropolitan[is.na(demographics$metropolitan)] <- 'NA'

# add indicator if child in household
demographics$child_in_HH <- demographics$n_child > 0

# TODO: should we remove these observations?
sum(is.na(demographics$fam_income))

# essential worker percentage
ggplot(demographics, aes(x = essential_industry, group = 1)) + 
  geom_bar(aes(y = ..prop..),) +
  labs(title = "Proportion of workers in essential industries",
       subtitle = '2007/2009 data',
       x = NULL,
       y = 'Proportion')
# ggsave('analyses/plots/essential_workers.png', height = 6, width = 9)


# demographic var selection -----------------------------------------------

# TODO:
# matching: follow this but try a few different (SES is primary goal)
# hard match: gender, race, urban/rural, region, partnership, essential worker status, labor force status
# soft match: age, income, number of children, education (+/- 1); is there  relative within X miles, is spouse working
# covariate: is there an elder in the household; **health**, neighborhood attributes (eg crime), length of commute (or public transport?)
# come up with list and run by Marc

matching_vars <- c('age', 'sex', 'race', 'fam_income', 'has_partner', 
                   'education', 'child_in_HH', 'n_child', 'age_youngest', 'region', 
                   'labor_force_status', 'partner_working', 'elder_in_HH',
                   'metropolitan')
demographics <- demographics[, c('ID', 'year', matching_vars)]

# remove NAs b/c income unknown for 13%
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
overlap_continuous <- demographics %>%
  select(-ID) %>% 
  dplyr::select_if(is.numeric) %>%
  pivot_longer(cols = -year) %>%
  ggplot(aes(x = value, fill = as.factor(year))) +
  geom_histogram(alpha = 0.7, position = 'identity') +
  facet_wrap(~name, scales = 'free') +
  labs(title = "Overlap of key demographic variables by year",
       subtitle = "These data are before any matching is performed",
       x = NULL,
       fill = NULL) +
  theme(plot.background = element_rect(color = NA))
overlap_categorical <- demographics %>% 
  dplyr::select(where(~!is.numeric(.x))) %>%
  mutate(across(everything(), as.character)) %>% 
  bind_cols(year = demographics$year) %>% 
  pivot_longer(cols = -year) %>%
  ggplot(aes(x = value, fill = as.factor(year))) +
  geom_bar(alpha = 0.7, position = 'dodge') +
  facet_wrap(~name, scales = 'free') +
  labs(title = NULL,
       x = NULL,
       fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(color = NA))
overlap_continuous / overlap_categorical + plot_layout(heights = c(2, 3))
# ggsave('analyses/plots/overlap_raw.png', height = 12, width = 9)


# basic stats -------------------------------------------------------------

# counts by group
demographics %>% 
  group_by(year) %>% 
  tally() %>% 
  mutate(year = as.character(year)) %>% 
  add_row(tibble(year = 'Total',
                 n = sum(.$n))) %>% 
  write_csv('data/summary_stats_raw_n.csv')


# sizing up key population ------------------------------------------------

# how big are our groups
demographics %>% 
  filter(sex == 'female') %>% 
  mutate(age = cut(age, breaks = c(0, 18, 60, 100))) %>% 
  group_by(year, age, race) %>% 
  tally() %>% 
  arrange(race, age, year)
