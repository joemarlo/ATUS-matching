library(tidyverse)

# list all the backtests previously run
path_years <- list.dirs(file.path('analyses', 'backtest'), recursive = FALSE)
years_run <- str_extract(path_years, "[0-9]*_[0-9]*$")


# mean stationary rate between like clusters ------------------------------

# read in the data
transition_dfs <- map_dfr(path_years, function(path){
  transition_df <- read_csv(file.path(path, "data", "transition_matrix.csv"))
  years <- str_extract(path, "[0-9]*_[0-9]*$")
  years <- str_split(years, '_')
  transition_df$year1 <- years[[1]][1]
  transition_df$year2 <- years[[1]][2]
  return(transition_df)
})

# calculate mean rates
stationary_rate <- transition_dfs %>%
  mutate(year = paste0(year1, "_", year2)) %>% 
  group_by(year) %>% 
  group_split() %>% 
  map(function(df){
    t_mat <- df %>% 
      pivot_wider(values_from = n, names_from = t2) %>% 
      select(-t1, -year1, -year2, -year) %>% 
      as.matrix()
    mean_stationary <- sum(diag(t_mat)) / sum(t_mat)
    return(mean_stationary)
  })

# plot rate
tibble(rate = unlist(stationary_rate)) %>% 
  mutate(year = as.numeric(str_extract(years_run, "[0-9]{4}"))) %>% 
  ggplot(aes(x = year, y = rate)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2004:2017,
                     labels = paste0(2004:2017, '\n-\n', 2005:2018)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = 'Mean rate that observations transition to like cluster',
       subtitle = 'One year lag',
       x = NULL,
       y = 'Mean transition rate')
ggsave(file.path('analyses', 'backtest', 'plots', 'mean_transition_rate.png'),
       width = 9, height = 6)

# TODO: how does this compare to baseline noise? 
# e.g. if it was totally random, what would be the transition rate
# needs to account for k clusters and n per cluster


# how many years have the same number of clusters -------------------------

transition_dfs %>% 
  group_by(year1) %>% 
  summarize(t1 = n_distinct(t1),
            t2 = n_distinct(t2)) %>%
  pivot_longer(-year1) %>% 
  ggplot(aes(x = year1, y = value, fill = name)) +
  geom_col(position = 'dodge', width = 0.7) +
  scale_x_discrete(labels = paste0(2004:2017, '\n-\n', 2005:2018)) +
  scale_y_continuous(breaks = 0:8) +
  labs(title = 'k clusters by year',
       x = NULL,
       y = 'k clusters',
       fill = NULL) +
  theme(legend.position = 'bottom')
ggsave(file.path('analyses', 'backtest', 'plots', 'k_clusters.png'),
       width = 9, height = 6)
