library(tidyverse)
source('analyses/plots/ggplot_settings.R')
set.seed(44)

# read in the demographics data
demographics <- read_delim(file = "data/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE)

# TOOO: need to keep the participant ID intact for later merging with time use data

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

# recode married
demographics$married <- recode(demographics$married, `1` = 'married', `0` = 'not married')


# demographic var selection -----------------------------------------------

# TODO:
# matching: follow this but try a few different (SES is primary goal)
# hard match: gender, race, urban/rural, region, partnership
# soft match: age, income, number of children, education (+/- 1); is there  relative within X miles
# covariate: is there an elder in the household; health
# come up with list and run by Marc

matching_vars <- c('age', 'sex', 'race', 'HH_income', 'married', 'education')
demographics <- demographics[, c('year', matching_vars)]

# remove NAs
demographics <- na.omit(demographics)


# overlap -----------------------------------------------------------------

# histograms showing overlaps
demographics %>%
  dplyr::select_if(is.numeric) %>%
  pivot_longer(cols = -year) %>%
  ggplot(aes(x = value, fill = as.factor(year))) +
  geom_histogram(alpha = 0.7, position = 'identity') +
  facet_wrap(~name, scales = 'free') +
  labs(title = "Overlap of key demographic variables by year",
       x = NULL,
       fill = NULL)
demographics %>% 
  dplyr::select(where(~!is.numeric(.x))) %>%
  bind_cols(year = demographics$year) %>% 
  pivot_longer(cols = -year) %>%
  ggplot(aes(x = value, fill = as.factor(year))) +
  geom_bar(alpha = 0.7, position = 'dodge') +
  facet_wrap(~name, scales = 'free') +
  labs(title = "Overlap of key demographic variables by year",
       x = NULL,
       fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# sizing up key population ------------------------------------------------

# how big are our groups
demographics %>% 
  filter(sex == 'female') %>% 
  mutate(age = cut(age, breaks = seq(0, 100, 10))) %>% 
  group_by(year, age, race) %>% 
  tally() %>% 
  arrange(race, age, year)
