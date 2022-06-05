### ITT of matched pairs ###

# current matching
# -Mahalanobis distance on age, sex, race, fam_income, has_partner, education, child_in_HH, n_child, age_youngest, region, partner_working, elder_in_HH, metropolitan  
# -Stratified on sex, race, +/- 2 age  
# -Matching performed from time1 to time2 (many-to-1) 
# -2020v2019

library(dplyr)
library(readr)
library(ggplot2)
source('analyses/plots/ggplot_settings.R')
source('analyses/helpers_analyses.R')

# if not running in batch mode, then create a file path to the analyses subfolder
# for use when saving plots and dataframes
# if (!isTRUE(get0('in_batch_mode'))) file_path <- "analyses"

# read in the demographics data
demographics <- read_delim(file = file.path("data", "demographic.tsv"),
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           col_types = cols(metropolitan = col_character()))

# read in the matches (ie output of matching_mahalanobis.R)
demographics_t1 <- read_csv(file = file.path(file_path, 'data', 'matched_time1_mahalanobis.csv'))
demographics_t2_raw <- read_csv(file = file.path(file_path, 'data', 'matched_time2_mahalanobis.csv'))

# remove duplicates for clustering
# demographics_t2 <- distinct(demographics_t2_raw, across(-pair_id), .keep_all = TRUE)

# matches
pair_IDs <- demographics_t1 %>% 
  bind_rows(demographics_t2_raw) %>% 
  select(ID, pair_id, year) %>% 
  distinct()

# pair_IDs %>%
#   na.omit() %>%
#   group_by(year) %>%
#   tally()

# from child_care.R
respondents_with_children <- readr::read_csv(file.path('analyses', 'data', 'respondents_with_children.csv'))

# filter SSC to just these years
childcare_df <- respondents_with_children %>% 
  filter(year %in% 2019:2020) %>%
  # filter(year %in% 2018:2019) %>% 
  select(ID, survey_weight, year, sex, secondary_childcare)

# why so few?
# - I think because filtering out half of 2020, filtering out weekends, etc.
mean(childcare_df$ID %in% pair_IDs$ID)

# density of secondary childcare
childcare_df %>% 
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
                         scales::comma_format()(nrow(childcare_df))),
       caption = 'No matching',
       x = 'Hour:minutes on secondary childcare',
       fill = NULL) +
  theme(legend.position = 'top')
# ggsave(file.path('analyses', 'plots', 'matched-ITT', 'density-unmatched-2019:2020.png'),
#        width = 9, height = 6)

# matching 
childcare_matched <- left_join(childcare_df, select(pair_IDs, -year), by = 'ID')
# TODO why the NAs; something with matching process?
childcare_matched %>% 
  group_by(year) %>% 
  summarize(sum(is.na(pair_id)))

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
  theme(legend.position = 'top')
# ggsave(file.path('analyses', 'plots', 'matched-ITT', 'density-matched-2019:2020-q3q4-1tomany.png'),
#        width = 9, height = 6)

childcare_pairs_wide <- childcare_pairs %>% 
  select(pair_id, year, secondary_childcare, sex) %>%
  tidyr::pivot_wider(names_from = year, values_from = secondary_childcare) %>% 
  mutate(diff = `2020` - `2019`)
  # mutate(diff = `2019` - `2018`)

childcare_pairs %>% 
  group_by(pair_id) %>% 
  arrange(year) %>% 
  mutate(diff = secondary_childcare[2] - secondary_childcare[1]) %>%
  arrange(pair_id) %>% 
  ggplot() +
  geom_linerange(data = childcare_pairs_wide,
                 aes(xmin = `2019`, xmax = `2020`, y = reorder(pair_id, -diff)),
                 color = 'grey60', linetype = 'solid', size = 0.2) +
  geom_point(aes(x = secondary_childcare, y = reorder(pair_id, -diff), fill = as.factor(year)),
             color = 'grey60', pch = 21, stroke = 0.6, alpha = 0.9, size = 2) +
  scale_x_continuous(labels = format_hour_minute) +
  scale_y_discrete(labels = NULL) +
  facet_wrap(~sex, scales = 'free_y') +
  labs(title = '2019 vs 2020 secondary childcare among matched pairs',
       subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
                         scales::comma_format()(nrow(childcare_pairs)),
                         "\n",
                         scales::percent_format()(mean(childcare_pairs_wide$`2020` > childcare_pairs_wide$`2019`)),
                         ' of pairs have greater SCC in 2020'),
       caption = 'Matched 2019:2020 Q3 and Q4; 1-to-many',
       x = 'Hour:minutes on secondary childcare',
       y = 'Matched pair',
       fill = NULL) +
  theme(legend.position = 'top', 
        panel.grid.major.y = element_blank())
# ggsave(file.path('analyses', 'plots', 'matched-ITT', 'diff-matched-2019:2020-q3q4-1tomany-sex.png'),
#        width = 9, height = 12)

childcare_pairs_wide %>% 
  ggplot(aes(x = diff)) +
  geom_density() +
  geom_vline(xintercept = mean(childcare_pairs_wide$diff)) +
  geom_vline(xintercept = median(childcare_pairs_wide$diff)) +
  scale_x_continuous(labels = format_hour_minute,
                     breaks = seq(-60*20, 60*14, by = 60*2)) +
  labs(title = 'Distribution of differences between matched pairs',
       caption = 'Matched 2019:2020 Q3 and Q4; 1-to-many',
       x = 'Difference in hour:minutes on secondary childcare')

# density of differences by sex
# see chunk/mode on right hand side of female
# males slightly less than 0
# add this plot to google doc; explain what it is; tag sharon marc

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
# ggsave(file.path('analyses', 'plots', 'matched-ITT', 'diff-matched-2019:2020-q3q4-sex-1tomany.png'),
#        width = 9, height = 12)


# match quality -----------------------------------------------------------

match_distances <- readr::read_csv('analyses/data/pair_distance.csv')
childcare_pairs_diffs %>% 
  ungroup() %>% 
  distinct(pair_id, sex, diff) %>% 
  left_join(match_distances, by = 'pair_id') %>% 
  ggplot(aes(x = diff, y = distance, fill = sex)) +
  geom_point(alpha = 0.8, size = 2, shape = 21, color = 'grey50') +
  # geom_density_2d() +
  labs(title = ' Match distance vs time difference between matched pairs',
       caption = 'Matched 2018:2019 Q3 and Q4; 1-to-many',
       x = 'Difference in hour:minutes on secondary childcare',
       y = 'Mahalanobis distance between matched pairs',
       fill = NULL)

childcare_pairs_diffs %>% 
  ungroup() %>% 
  distinct(pair_id, sex, diff) %>% 
  left_join(match_distances, by = 'pair_id') %>% 
  mutate(bin = cut(distance, breaks = c(0, 3, 6, 10))) %>% 
  ggplot(aes(x = diff)) +
  geom_density() +
  facet_grid(bin ~ sex)
# include as footnote

# call notes --------------------------------------------------------------

# look at match quality by diff
# can double check matching results by monte carlo / bootstrap the pairwise matching
  # see how those differences compare to the actual differences
# could parse out seasonality using historical means (maybe back to ~2016; look at sawtooth plot in paper)
# matching should be 2020 backwards. I.e. make sure all 2020 people have a match
# stick to quarters (q3 to q3 and then q4 to q4). Stratify by quarters and check the n
# diff density shows second mode around 8:00; could be a full work day



# should super impose density on histogram DONE
# should repeat for 2018-vs 2019 as a reference DONE