library(tidyverse)
source('analyses/plots/ggplot_settings.R')
source('analyses/helpers_analyses.R')

# noise analysis ----------------------------------------------------------

# read in data denoting the mahalonobis distance between pairs
pair_distance <- read_csv(file.path('analyses', 'data', 'pair_distance.csv'))

# read in data on cluster assignments
pair_clusters <- read_csv(file.path('data', 'cluster_pairs.csv'))

# read in demographic data
demographics_t1 <- read_csv(file.path('data', 'matched_time1_mahalanobis.csv'))
demographics_t2 <- read_csv(file.path('data', 'matched_time2_mahalanobis.csv'))

time1 <- demographics_t1$year[[1]]
time2 <- demographics_t2$year[[1]]


# match rate at k ---------------------------------------------------------

# pivot data wider and merge to get distance by paid
pairs <- pair_clusters %>% 
  select(-ID) %>% 
  mutate(cluster = str_extract(cluster, 'Cluster \\d')) %>% 
  pivot_wider(names_from = time, values_from = cluster) %>% 
  left_join(pair_distance, by = 'pair_id')

# calculate and plot the match rate at k
pairs %>%
  replace_na(list(t2 = 'None')) %>% 
  arrange(distance) %>% 
  mutate(rank = row_number(),
         match_rate = cumsum(t1 == t2) / rank) %>%  
  ggplot(aes(x = rank, y = match_rate)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 7000, by = 1000),
                     labels = scales::comma_format()) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     limits = c(0, 1)) +
  labs(title = 'Proportion of matched pairs that transition to like cluster inter-year',
       subtitle = paste0(time1, "/", time2),
       x = 'Matched pairs ranked by Mahalanobis distance',
       y = 'Proportion at k')
ggsave(file.path('analyses', 'plots', 'noise', 'match_rate_at_k.png'),
       width = 9, height = 6)

# calculate match rate by binned score
pairs %>% 
  mutate(distance_binned = cut(distance, seq(-1, 10, by = 1))) %>% 
  group_by(distance_binned) %>% 
  summarize(match_rate = mean(t1 == t2)) %>% 
  ggplot(aes(x = distance_binned, y = match_rate)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     limits = c(0, 1)) +
  labs(title = 'Proportion of matched pairs that transition to like cluster inter-year',
       subtitle = paste0(time1, "/", time2),
       x = 'Binned Mahalanobis distance',
       y = 'Proportion')
ggsave(file.path('analyses', 'plots', 'noise', 'match_rate_binned.png'),
       width = 9, height = 6)

# match rate at k per cluster
pairs %>%
  replace_na(list(t2 = 'None')) %>% 
  arrange(distance) %>% 
  group_by(t1) %>% 
  mutate(rank = row_number(),
         match_rate = cumsum(t1 == t2) / rank) %>%  
  ggplot(aes(x = rank, y = match_rate, color = t1)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 7000, by = 1000),
                     labels = scales::comma_format()) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     limits = c(0, 1)) +
  labs(title = 'Proportion of matched pairs that transition to like cluster inter-year',
       subtitle = paste0(time1, "/", time2),
       x = 'Matched pairs ranked by Mahalanobis distance',
       y = 'Proportion at k',
       color = NULL) +
  theme(legend.position = 'bottom')
ggsave(file.path('analyses', 'plots', 'noise', 'match_rate_at_k_per_cluster.png'),
       width = 9, height = 6)


# match rate conditioned on day of week and month -------------------------

# read in demographics data to get the date
demographics <- read_delim(file = "data/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           col_types = cols(metropolitan = col_character()))

# merge to get ID / pair_id
pairs <- pairs %>% 
  left_join(select(demographics_t1, ID_t1 = ID, pair_id),
            by = 'pair_id') %>% 
  left_join(select(demographics_t2, ID_t2 = ID, pair_id),
            by = 'pair_id')

# merge to get the date and day of week
pairs <- pairs %>% 
  left_join(select(demographics, ID, date_t1 = date, dow_t1 = day_of_week),
            by = c('ID_t1' = 'ID')) %>% 
  left_join(select(demographics, ID, date_t2 = date, dow_t2 = day_of_week),
            by = c('ID_t2' = 'ID'))  

# clean up
pairs <- pairs %>% 
  mutate(month_t1 = str_sub(date_t1, 5, 6),
         month_t2 = str_sub(date_t2, 5, 6))

# function returns a logical indicating if the rolling season of month1 and month2 
# are the same
# e.g. Jan and Feb == TRUE; Jan and Dec == TRUE; Jan and Mar == FALSE
match_season <- function(month_1, month_2, window = 3){
  if (length(month_1) != 1) stop('month_1 must be length 1')
  if (window %notin% c(3, 5, 7, 9, 12)) stop('window must be 3, 5, 7, 9, 12')
  half_window <- (window - 1) / 2
  months <- 1:36 %% 12
  months[months == 0] <- 12
  month_t1_index <- which(months == month_1)[2]
  months_in_window <- months[seq(month_t1_index - half_window, month_t1_index + half_window)]
  month_t2_in_window <- month_2 %in% months_in_window
  return(month_t2_in_window)
}
# match_season(1, 1:12)
# match_season(6, 1:12)

# cluster match rate by season
pairs %>% 
  mutate(month_t1 = as.numeric(month_t1),
         month_t2 = as.numeric(month_t2)) %>% 
  rowwise() %>% 
  mutate(season_match = match_season(month_t1, month_t2)) %>%  
  group_by(season_match) %>% 
  summarize(rate = mean(t1 == t2))

# cluster match rate by day of week
pairs %>% 
  mutate(dow_match = dow_t1 == dow_t2) %>% 
  group_by(dow_match) %>% 
  summarize(rate = mean(t1 == t2))


# match quality -----------------------------------------------------------

# examine the matches by quality
demographics_t1 %>% 
  bind_rows(demographics_t2) %>% 
  left_join(pair_distance, by = 'pair_id') %>% 
  arrange(distance, pair_id) %>% 
  View

# distribution of match distance
pair_distance %>% 
  ggplot(aes(x = distance)) +
  geom_histogram(color = 'white')


# age ---------------------------------------------------------------------

# mean stationary rate and distance by age
pairs %>% 
  select(pair_id, t1, t2, ID_t1, ID_t2) %>% 
  left_join(select(demographics, ID, age),
            by = c('ID_t1' = 'ID')) %>% 
  left_join(pair_distance, by = 'pair_id') %>% 
  mutate(age_bin = cut(age, seq(0, 100, 5))) %>% 
  group_by(age_bin) %>% 
  summarize(Proportion = mean(t1 == t2),
            `Mean distance` = mean(distance)) %>% 
  pivot_longer(-age_bin) %>% 
  ggplot(aes(x = age_bin, y = value)) +
  geom_point() +
  facet_wrap(~name, ncol = 1, scales = 'free_y') +
  labs(title = 'Distance b/t matched pair and proportion that transition to like cluster inter-year',
       subtitle = paste0(time1, "/", time2),
       x = '\nBinned age of individual in time 1',
       y = NULL) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0))
ggsave(file.path('analyses', 'plots', 'noise', 'match_rate_by_age.png'),
       width = 9, height = 6)

demographics %>% 
  select(ID, age, labor_force_status, education) %>% 
  right_join(pair_clusters, by = 'ID') %>% 
  filter(str_detect(cluster, 'Cluster 2')) %>%
  ggplot(aes(x = age)) +
  geom_histogram(breaks = 1:100, color='white') + 
  facet_wrap(~time)



# sex ---------------------------------------------------------------------

pairs %>% 
  select(pair_id, t1, t2, ID_t1, ID_t2) %>% 
  left_join(select(demographics, ID, sex),
            by = c('ID_t1' = 'ID')) %>% 
  left_join(pair_distance, by = 'pair_id') %>% 
  mutate(sex = recode(sex, '1' = 'Male', '2' = 'Female')) %>% 
  group_by(t1, sex) %>% 
  summarize(Proportion = mean(t1 == t2),
            `Mean distance` = mean(distance)) %>% 
  pivot_longer(c('Proportion', 'Mean distance')) %>%
  ggplot(aes(x = sex, y = value, fill = t1)) +
  geom_col(position = 'dodge') +
  facet_wrap(~name, ncol = 1, scales = 'free_y') +
  labs(title = 'Distance b/t matched pair and proportion that transition to like cluster inter-year',
       subtitle = paste0(time1, "/", time2),
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme(legend.position = 'bottom')


# seqI across time --------------------------------------------------------

# read in ATUS data
atus_raw <- read_tsv(file.path("data", "atus_30min.tsv"))

# expand data to get sequences by ID
pair_activities <- pairs %>% 
  select(pair_id ,t1, ID_t1, ID_t2) %>% 
  mutate(y_id = paste0(pair_id, t1)) %>% 
  pivot_longer(c('ID_t1', 'ID_t2'),
               names_to = 'time',
               values_to = 'ID') %>% 
  mutate(time = recode(time, 'ID_t1' = 'Time 1', 'ID_t2' = 'Matched pair in time 2'),
         time = factor(time, levels = c('Time 1', 'Matched pair in time 2'))) %>% 
  left_join(atus_raw, by = "ID")

# calculate of entropy of t1 sequence
entropies <- pair_activities %>% 
  filter(time == 'Time 1') %>% 
  group_by(pair_id) %>% 
  summarize(entropy = sequenchr::shannon_entropy(description))

# plot the sequences side by side
pair_activities %>% 
  left_join(entropies, by = 'pair_id') %>% 
  ggplot(aes(x = period, y = reorder(y_id, -entropy), fill = description)) +
  geom_tile() +
  scale_y_discrete(labels = NULL) +
  facet_grid(t1~time, scales = 'free')  +
  labs(title = 'Sequence index of matched pairs',
       subtitle = paste0(
         'Cluster represents cluster in time 1',
         '\nSorted by Shannon entropy in time 1',
         '\n',  paste0(time1, "/", time2)),
       x = 'Period',
       y = NULL,
       fill = NULL)
ggsave(file.path('analyses', 'plots', 'noise', 'matched_seqI.png'),
       width = 9, height = 6)
 

# demographics predicting stability ---------------------------------------

# get demographics data for calculating differentials b/t subgroups
differential_data <- pairs %>% 
  mutate(cluster_match = t1 == t2) %>% 
  select(pair_id, ID = ID_t1, cluster_match) %>% 
  left_join(demographics, by = 'ID') %>% 
  select(cluster_match, age, sex, age_youngest, n_child, labor_force_status, 
         partner_working, fam_income, elder_in_HH, has_partner, race, 
         education, metropolitan) %>% 
  mutate(age = cut(age, seq(0, 100, by = 5)),
         fam_income = cut(fam_income, seq(0, 150000, by = 10000)),
         sex = recode(sex, "1" = "Male", "2" = "Female")) 
  
# logistic regression to understand drivers of cluster match
differential_data %>% 
  glm(cluster_match ~ ., data = .) %>%
  broom::tidy() %>% 
  filter(term != '(Intercept)') %>% 
  mutate(min = estimate - (1.96 * std.error),
         max = estimate + (1.96 * std.error),
         term = fct_reorder(term, estimate)) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey50') +
  geom_linerange(aes(xmin = min, xmax = max, 
                     y = term)) +
  geom_point(aes(x = estimate, y = term)) +
  labs(title = 'Log reg estimates predicting (cluster in t1) == (cluster in t2)',
       subtitle = paste0(
         'Range represents 95% confidence interval',
         '\n', paste0(time1, "/", time2)),
       x = NULL,
       y = 'Varible in time 1')
ggsave(file.path('analyses', 'plots', 'noise', 'log_estimates.png'),
       width = 9, height = 8)

# differential of cluster match
differential_data %>% 
  select(cluster_match, age, sex, n_child, labor_force_status, partner_working, 
         fam_income, has_partner, race, education, metropolitan, elder_in_HH) %>% 
  mutate(across(-cluster_match, as.character)) %>% 
  pivot_longer(-cluster_match) %>% 
  group_by(name, value) %>% 
  summarize(match_rate = mean(cluster_match),
            .groups = 'drop') %>% 
  ggplot(aes(x = value, y = match_rate)) +
  geom_col() +
  facet_wrap(~name, scales = 'free_x') +
  labs(title = 'Proportion of matched pairs that transition to like cluster inter-year',
       subtitle = paste0(
         'Variable in time 1',
         '\n', paste0(time1, "/", time2)),
       caption = 'All variables are matching variables except labor_force_status',
       x = NULL,
       y = 'Proportion') +
  theme(axis.text.x = element_text(angle = -50, hjust = 0, size = 7))
ggsave(file.path('analyses', 'plots', 'noise', 'match_rate_variable.png'),
       width = 9, height = 7)

# match quality by group
pairs %>% 
  mutate(cluster_match = t1 == t2) %>% 
  select(pair_id, distance, ID = ID_t1) %>% 
  left_join(select(demographics, ID, sex, labor_force_status),
            by = "ID") %>% 
  mutate(sex = recode(sex, '1' = 'Male', '2' = "Female")) %>% 
  group_by(sex, labor_force_status) %>% 
  mutate(mean = mean(distance)) %>% 
  ggplot(aes(x = distance)) +
  geom_vline(aes(xintercept = mean), 
             linetype = 'dashed', color = 'grey50') +
  geom_histogram(aes(y=..ncount..), color = 'white') +
  scale_x_continuous(breaks = 0:10) +
  facet_grid(sex ~ labor_force_status) +
  labs(title = 'Distribution of match quality',
       subtitle = paste0(
         'Variable in time 1',
         '\n', paste0(time1, "/", time2)),
       x = "Distance (greater == worse quality match)",
       y = 'Normalized count')
ggsave(file.path('analyses', 'plots', 'noise', 'match_quality_sex_labor.png'),
       width = 9, height = 5)
