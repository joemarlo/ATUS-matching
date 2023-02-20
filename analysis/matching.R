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

year1 <- 2019
year2 <- year1 + 1

m_matches <- local({

  # NOTE: this is only for plots; must adjust code in matching_prep_demographics() and matching_mahalanobis()
  # if you want to change the matching and blocking vars
  matching_vars <- c('age', 'sex', 'race', 'fam_income', 'has_partner', 
                     'education', 'child_in_HH', 'n_child', 'age_youngest', 'region', 
                     'partner_working', 'elder_in_HH', 'metropolitan')#, 'labor_force_status')
  # blocking_vars <- c('sex', 'race', 'metropolitan', 'region', 'has_partner', 'labor_force_status') #'essential_worker # add child_in_HH?
  blocking_vars <- c('sex', 'race') #, 'has_partner') #'essential_worker
  
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
  filter(year %in% c(year1, year2)) %>%
  # filter(year %in% 2018:2019) %>% 
  select(ID, survey_weight, year, sex, primary_childcare, secondary_childcare, has_partner)

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

# restrict to just couples
childcare_pairs <- childcare_pairs |> 
  group_by(pair_id) |> 
  mutate(keep = has_partner[year == 2020]) |> 
  ungroup() |> 
  filter(keep)

# densities of matches
childcare_pairs %>% 
  group_by(year) %>% 
  mutate(scc_mean = mean(secondary_childcare)) %>% 
  ggplot(aes(x = secondary_childcare, group = year, color = as.factor(year))) +
  geom_density(linewidth = 1.6) +
  geom_vline(aes(xintercept = scc_mean, color = as.factor(year)),
             linetype = 'dashed') +
  scale_x_continuous(limits = c(0, 60*20),
                     breaks = seq(0, 60*20, by = 120),
                     labels = format_hour_minute) +
  # scale_y_continuous(limits = c(0, 0.0025)) +
  # scale_color_discrete(guide = 'none') +
  scale_color_brewer(palette = 2, type = 'qual') +
  labs(title = 'Mean daily time spent on secondary childcare for household children under 13',
       subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
                         scales::comma_format()(nrow(na.omit(childcare_pairs)))),
       caption = 'Matched 2019:2020 Q3 and Q4; 1-to-many',
       x = 'Hour:minutes on secondary childcare',
       fill = NULL,
       color = NULL) +
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
  # geom_line(data = childcare_pairs_diff_density,
  #           aes(x = density_x, y = density_y)) +
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
       # caption = glue::glue('Matched {year1}:{year2} Q3 and Q4; 1-to-many'),
       x = 'Difference in hour:minutes on secondary childcare',
       y = 'Count / scaled density')
# ggsave(file.path('outputs', 'matching', 'diff-matched-2019:2020-q3q4-sex-1tomany.png'),
#        width = 9, height = 10)
# ggsave(file.path('outputs', 'matching', 'diff-matched-2019:2020-q3q4-sex-1tomany-couples.png'),
#        width = 9, height = 10)

# mean difference by gender
childcare_pairs_diffs |> 
  group_by(sex) |> 
  summarize(mean = mean(diff))


# is match quality conditional on PCC/SCC ---------------------------------

# scatter of m-distance vs within pair difference
childcare_pairs_diffs |> 
  group_by(pair_id) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  dplyr::select(pair_id, diff) |> 
  left_join(m_matches$pair_distance, by = 'pair_id') |> 
  filter(distance < 1000000) |> # outliers
  ggplot(aes(x = diff, y = distance)) +
  geom_point() +
  geom_smooth(method = 'lm')
  

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

# what percent of women are in +8 bump
childcare_pairs_groups |> 
  filter(group == 'group2') |> 
  distinct(pair_id) |> 
  left_join(childcare_pairs_diffs |> ungroup() |> distinct(pair_id, sex),
            by = 'pair_id') |> 
  filter(sex == 'Female') |> 
  nrow() %>% 
  {. / (childcare_pairs_diffs |> distinct(pair_id, sex) |> filter(sex == 'Female') |> nrow())}

# childcare_pairs_groups %>%
#   ggplot(aes(x = diff, fill = group)) +
#   geom_histogram() +
#   scale_x_continuous(labels = format_hour_minute,
#                      breaks = seq(-60*20, 60*14, by = 60*2))

# what percent of +8 hour bump are single women
childcare_pairs_groups |> 
  filter(group == 'group2') |> 
  distinct(pair_id) |> 
  left_join(childcare_pairs_diffs |> ungroup() |> distinct(ID, pair_id, sex),
            by = 'pair_id') |> 
  left_join(select(demographics, ID, has_partner),
            by = 'ID') |> 
  count(sex, has_partner) |> 
  mutate(prop = n / sum(n)) |> 
  arrange(desc(prop))


demographics %>% 
  right_join(childcare_pairs_groups, by = 'ID') %>% 
  select(ID, group, pair_id, sex, age_youngest, n_child, labor_force_status, 
         partner_working, has_partner, fam_income, elder_in_HH, 
         essential_industry, education, metropolitan) %>% 
  mutate(across(everything(), as.character)) %>%
  mutate(sex = recode(sex, `1` = 'male', `2` = 'female'),
         group = recode(group, group1 = '1. None: 0 hours +/- 2', group2 = '2. Large: 8 hours +/- 2')) %>% 
  select(group, age_youngest, n_child, fam_income, has_partner) %>%
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

# densities
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
  mutate(value = as.numeric(value)) %>% 
  ggplot(aes(x = value, group = group, fill = group)) + 
  geom_density(alpha = 0.8) + 
  facet_wrap(~ name, scales = 'free') +
  labs(title = 'Demographics variables across respondents who experience no and large increases in SCC',
       x = NULL,
       fill = NULL) +
  theme(legend.position = 'bottom')

# densities just 2019 vs 2020; no diff
demographics %>% 
  right_join(childcare_pairs_groups, by = 'ID') %>% 
  select(ID, year, age_youngest, n_child, fam_income) %>% 
  pivot_longer(c(age_youngest, n_child, fam_income)) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = value, group = year, fill = year)) + 
  geom_density(alpha = 0.8) +
  scale_x_continuous(labels = scales::comma_format()) +
  facet_wrap(~name, scales = 'free') +
  labs(title = 'Demographic variables across all matched pairs',
       x = NULL)

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

demographics %>% 
  right_join(childcare_pairs_groups, by = 'ID') %>% 
  select(ID, group, pair_id, sex) %>% 
  mutate(sex = recode(sex, `1` = 'male', `2` = 'female')) %>% 
  group_by(group, sex) %>% 
  tally()



# seqI plots of the 0/8 hour  groups --------------------------------------

# read in ATUS data
atus_raw <- readr::read_tsv(file.path("data", "atus_30min_SSC.tsv"))

# create second color mapping with extraneous tokens greyed out
color_mapping_grey <- c('grey20', '#d9d955', '#db324b', 'grey55')
names(color_mapping_grey) <- c('Sleep', 'Work w/o SCC', 'Work with SCC', 'Other activities')

.year <- 2020 #2019
atus_raw %>% 
  semi_join(demographics %>% filter(year == .year),
            by = 'ID') %>% 
  inner_join(childcare_pairs_groups, by = 'ID') %>% 
  mutate(ID = as.character(ID),
         group = recode(group, group1 = '1. None: 0 hours +/- 2', group2 = '2. Large: 8 hours +/- 2'),
         description = case_when(
           description == 'Sleep : No SCC' ~ 'Sleep',
           description == 'Work : No SCC' ~ 'Work w/o SCC',
           description == 'Work : SCC' ~ 'Work with SCC',
           TRUE ~ 'Other activities')) %>% 
  group_by(ID) %>% 
  mutate(entropy = sum(description == 'Work with SCC')) %>% 
  ggplot(aes(x = period, y = reorder(ID, entropy), fill = description)) +
  geom_tile() +
  scale_fill_manual(values = color_mapping_grey) +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  facet_wrap(~group, scales = 'free_y') +
  labs(title = 'Sequence index of activities across respondents who experience no and large increases in SCC',
       subtitle = .year,
       y = NULL,
       fill = NULL)


# which clusters are the matched individuals in ---------------------------

clustering_results <- readRDS('outputs/SA/backtest_2021.rds')

clusters_2020 <- clustering_results$`2019:2020`$clusters %>% 
  mutate(in_matched = ID %in% pair_IDs$ID)

table(
  clusters_2020$cluster, 
  clusters_2020$year, 
  clusters_2020$in_matched
)



