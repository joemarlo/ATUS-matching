library(tidyverse)
source('analyses/plots/ggplot_settings.R')
source('analyses/helpers_analyses.R')

# noise analysis ----------------------------------------------------------

# read in data denoting the mahalonobis distance between pairs
pair_distance <- read_csv(file.path('data', 'pair_distance.csv'))

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
ggsave(file.path('analyses', 'plots', 'match_rate_at_k.png'),
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
ggsave(file.path('analyses', 'plots', 'match_rate_binned.png'),
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
ggsave(file.path('analyses', 'plots', 'match_rate_at_k_per_cluster.png'),
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
  if (!(window %in% c(3, 5, 7, 9, 12))) stop('window must be 3, 5, 7, 9, 12')
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
ggsave(file.path('analyses', 'plots', 'match_rate_by_age.png'),
       width = 9, height = 6)

demographics %>% 
  select(ID, age, labor_force_status, education) %>% 
  right_join(pair_clusters, by = 'ID') %>% 
  filter(str_detect(cluster, 'Cluster 2')) %>%
  ggplot(aes(x = age)) +
  geom_histogram(breaks = 1:100, color='white') + 
  facet_wrap(~time)

pairs %>% 
  left_join(select(demographics_t1, ID_t1 = ID, age),
            by = 'ID_t1') %>% 
  mutate(age_binned = cut(age, seq(0, 100, by = 1))) %>% 
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
