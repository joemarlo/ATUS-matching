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
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  labs(title = 'Proportion of matched pairs that stay in the same matched cluster',
       subtitle = paste0(time1, "/", time2),
       x = 'Matched pairs ranked by Mahalanobis distance',
       y = 'Match rate at k')
ggsave(file.path('analyses', 'plots', 'match_rate_at_k.png'),
       width = 9, height = 6)



# match rate conditioned on day of week and month -------------------------



# match rate intra-year ---------------------------------------------------


