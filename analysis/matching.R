###
# This script performs the matching analysis 
###


library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# functions
purrr:::walk(list.files('R', full.names = TRUE), source)

# read in the demographics data
demographics <- readr::read_delim(file = file.path("data", "demographic.tsv"),
                                  delim = "\t",
                                  escape_double = FALSE,
                                  trim_ws = TRUE,
                                  col_types = readr::cols(metropolitan = readr::col_character()))

# read in data containing the respondents that have children
# determined in analysis.timeseries.R
respondents_with_children <- readr::read_csv(file.path('data', 'respondents_with_children.csv'))

# read in data with diary date
atusresp_0321 <- readr::read_csv(file.path("inputs","ATUS-2003-2021", "atusresp_0321.dat"))

# regions for matching
state_regions <- readr::read_csv("inputs/state_regions.csv")


# matching ----------------------------------------------------------------

m_matches <- local({

  # NOTE: this is only for plots; must adjust code in matching_prep_demographics() and matching_mahalanobis()
  # if you want to change the matching and blocking vars
  matching_vars <- c('age', 'sex', 'race', 'fam_income', 'has_partner', 
                     'education', 'child_in_HH', 'n_child', 'age_youngest', 'region', 
                     'partner_working', 'elder_in_HH', 'metropolitan')#, 'labor_force_status')
  # blocking_vars <- c('sex', 'race', 'metropolitan', 'region', 'has_partner', 'labor_force_status') #'essential_worker # add child_in_HH?
  blocking_vars <- c('sex', 'race') #, 'has_partner') #'essential_worker
  
  year1 <- 2019
  year2 <- 2020
  demographics_prepped <- matching_prep_demographics(atusresp_0321, demographics, state_regions, year1, year2, matching_vars)
  vars_numeric <- demographics_prepped$vars_numeric
  demographics_prepped <- demographics_prepped$demographics
  
  # only include households with children
  # for child care ITT matching
  demographics_prepped <- filter(demographics_prepped, demographics_prepped$ID %in% respondents_with_children$ID)
  demographics_prepped <- select(demographics_prepped, -child_in_HH)
  
  # add quarter identifier and filter to Q3 and Q4
  demographics_prepped <- atusresp_0321 %>%
    mutate(diary_date = lubridate::ymd(TUDIARYDATE),
           diary_month = lubridate::month(diary_date),
           quarter = ceiling(diary_month / 3)) %>%
    select(ID = TUCASEID, quarter) %>%
    right_join(demographics_prepped, by = 'ID') %>%
    filter(quarter %in% 3:4)
  
  matching_vars <- setdiff(c(matching_vars, 'quarter'), 'child_in_HH')
  
  # matching
  # TODO: make sure matching method is right
  # -Mahalanobis distance on age, sex, race, fam_income, has_partner, education, child_in_HH, n_child, age_youngest, region, partner_working, elder_in_HH, metropolitan  
  # -Stratified on sex, race, +/- 2 age  
  # -Matching performed from time1 to time2 (many-to-1) 
  # -2020v2019
  m_matches <- matching_mahalanobis(demographics_prepped, year1, year2, matching_vars, method = '1-to-many')
  
  # confirm that matching is correct
  year_treated <- m_matches$demographics_treated$year[[1]]
  year_control <- m_matches$demographics_control$year[[1]]
  i <- which.max(c(
    n_distinct(m_matches$demographics_treated$ID),
    n_distinct(m_matches$demographics_control$ID)
  ))
  cli::cli_alert("Within the matching process, year {c(year_treated, year_control)[i]} has the greater number of unique respondents")

  return(m_matches)
})


# analysis ----------------------------------------------------------------

# matches
pair_IDs <- m_matches$demographics_treated %>% 
  bind_rows(m_matches$demographics_control) %>% 
  select(ID, pair_id, year) %>% 
  distinct()

# filter SSC to just these years
childcare_df <- respondents_with_children %>% 
  filter(year %in% 2019:2020) %>%
  # filter(year %in% 2018:2019) %>% 
  select(ID, survey_weight, year, sex, secondary_childcare)

# matching 
childcare_matched <- left_join(childcare_df, select(pair_IDs, -year), by = 'ID')

# restrict to just pairs
childcare_pairs <- childcare_matched %>% 
  na.omit() %>% 
  group_by(pair_id) %>% 
  add_tally() %>% 
  ungroup() %>% 
  filter(n == 2) %>% 
  select(-n) %>% 
  arrange(pair_id)

# densities of matches
childcare_pairs %>% 
  group_by(year) %>% 
  mutate(scc_mean = mean(secondary_childcare)) %>% 
  ggplot(aes(x = secondary_childcare, group = year, fill = as.factor(year))) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = scc_mean, color = as.factor(year)),
             linetype = 'dashed') +
  scale_x_continuous(limits = c(0, 60*20), 
                     breaks = seq(0, 60*20, by = 120),
                     labels = format_hour_minute) +
  scale_y_continuous(limits = c(0, 0.0025)) +
  scale_color_discrete(guide = 'none') +
  labs(title = 'Mean daily time spent on secondary childcare for household children under 13',
       subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
                         scales::comma_format()(nrow(na.omit(childcare_pairs)))),
       caption = 'Matched 2019:2020 Q3 and Q4; 1-to-many',
       x = 'Hour:minutes on secondary childcare',
       fill = NULL) +
  theme(legend.position = 'bottom')
# ggsave(file.path('outputs', 'matching', 'density-matched-2019:2020-q3q4-1tomany.png'),
#        width = 9, height = 6)


### within pair difference, faceted by sex
childcare_pairs_diffs <- childcare_pairs %>% 
  group_by(pair_id) %>% 
  arrange(year) %>% 
  mutate(diff = secondary_childcare[2] - secondary_childcare[1]) %>% 
  arrange(pair_id)
density_scale_factor <- 65
density_scale <- function(x) x * density_scale_factor / max(x)
childcare_pairs_diff_density <- childcare_pairs_diffs %>% 
  group_by(sex) %>% 
  summarize(density_x = density(diff)$x, 
            density_y = density(diff)$y) %>% 
  mutate(density_y = density_scale(density_y))
childcare_pairs_diffs_means <- childcare_pairs_diffs %>% group_by(sex) %>% summarize(mean = mean(diff))
childcare_pairs_diffs %>% 
  ggplot(aes(x = diff)) +
  geom_histogram(fill = 'grey60', bins = 40) +
  geom_line(data = childcare_pairs_diff_density,
            aes(x = density_x, y = density_y)) +
  geom_vline(data = childcare_pairs_diffs_means,
             aes(xintercept = mean), color = 'grey30', linetype = 'dashed') +
  geom_text(data = childcare_pairs_diffs_means,
            aes(x = mean, y = 20), label = 'Mean', angle = 270,
            nudge_x = 30) +
  facet_wrap(~sex, ncol = 1) +
  scale_x_continuous(labels = format_hour_minute,
                     breaks = seq(-60*20, 60*14, by = 60*2)) +
  labs(title = 'Distribution of differences between matched pairs',
       subtitle = paste0('Only includes respondents with household children under 13\n', 
                         scales::comma_format()(n_distinct(childcare_pairs$pair_id)), " pairs"),
       caption = 'Matched 2019:2020 Q3 and Q4; 1-to-many',
       x = 'Difference in hour:minutes on secondary childcare',
       y = 'Count / scaled density')
# ggsave(file.path('outputs', 'matching', 'diff-matched-2019:2020-q3q4-sex-1tomany.png'),
#        width = 9, height = 10)


# who is experiencing the 8 hour increase ---------------------------------


# filter to just people in these two groups

childcare_pairs_groups <- local({
  # +/- 2 hours around hour 0 and hour 8
  margin <- 2 * 60
  group1 <- 0
  group2 <- 8 * 60
  
  childcare_pairs_diffs %>% 
    ungroup() %>% 
    mutate(group1 = diff > group1 - margin & diff < group1 + margin,
           group2 = diff > group2 - margin & diff < group2 + margin) %>% 
    select(ID, pair_id, diff, group1, group2) %>% 
    pivot_longer(-c(ID, pair_id, diff)) %>% 
    filter(value) %>% 
    select(-value) %>% 
    rename(group = name)
})

# childcare_pairs_groups %>%
#   ggplot(aes(x = diff, fill = group)) +
#   geom_histogram() +
#   scale_x_continuous(labels = format_hour_minute,
#                      breaks = seq(-60*20, 60*14, by = 60*2))


demographics %>% 
  right_join(childcare_pairs_groups, by = 'ID') %>% 
  select(ID, group, pair_id, sex, age_youngest, n_child, labor_force_status, 
         partner_working, has_partner, fam_income, elder_in_HH, 
         essential_industry, education, metropolitan) %>% 
  mutate(across(everything(), as.character)) %>%
  mutate(sex = recode(sex, `1` = 'male', `2` = 'female'),
         group = recode(group, group1 = '1. None: 0 hours +/- 2', group2 = '2. Large: 8 hours +/- 2')) %>% 
  select(group, age_youngest, n_child, fam_income) %>%
  # select(group, sex, labor_force_status, partner_working, has_partner, elder_in_HH, education) %>%
  pivot_longer(-group) %>%
  group_by(group, name, value) %>%
  tally() %>%
  group_by(group, name) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = value, y = prop, group = group, fill = group)) +
  geom_col(position = 'dodge') +
  facet_wrap(~name, scales = 'free') +
  labs(title = 'Demographics variables across respondents who experience no and large increases in SCC',
       subtitle = 'Normalized by group sample size',
       x = NULL,
       y = 'Count',
       fill = NULL) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0),
        legend.position = 'bottom')
# ggsave(file.path('outputs', 'matching', 'group-num-diff-matched-2019:2020-q3q4-sex-1tomany.png'),
#        width = 9, height = 8)

# only look at wealthy and sex
demographics %>% 
  right_join(childcare_pairs_groups, by = 'ID') %>% 
  select(ID, group, pair_id, sex, age_youngest, n_child, labor_force_status, 
         partner_working, has_partner, fam_income, elder_in_HH, 
         essential_industry, education, metropolitan) %>% 
  mutate(across(everything(), as.character)) %>%
  mutate(sex = recode(sex, `1` = 'male', `2` = 'female'),
         group = recode(group, group1 = '1. None: 0 hours +/- 2', group2 = '2. Large: 8 hours +/- 2'),
         `wealthy (>$60k)` = glue::glue("Wealthy(>$60k):{as.character(as.numeric(fam_income) > 60000)}")) %>% 
  select(group, sex, `wealthy (>$60k)`) %>%
  # pivot_longer(-group) %>%
  group_by(group, sex, `wealthy (>$60k)`) %>%
  tally() %>%
  # group_by(group, name) %>%
  # mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = group, y = n, group = group, fill = group)) +
  geom_col(position = 'dodge') +
  facet_wrap(sex~`wealthy (>$60k)`, scales = 'free') +
  labs(title = 'Demographics variables across respondents who experience no and large increases in SCC',
       # subtitle = 'Normalized by group sample size',
       x = NULL,
       y = 'Count',
       fill = NULL) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0),
        legend.position = 'bottom')

