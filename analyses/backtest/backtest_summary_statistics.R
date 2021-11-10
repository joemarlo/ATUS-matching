library(tidyverse)
source('analyses/plots/ggplot_settings.R')

# list all the backtests previously run
# path_to_batch <- list.dirs(file.path('analyses', 'backtest'), recursive = FALSE)[1]
path_years <- list.dirs(path_to_batch, recursive = FALSE)
path_years <- path_years[str_detect(path_years, "\\d$")]
years_run <- str_extract(path_years, "[0-9]*_[0-9]*$")


# mean stationary rate between like clusters ------------------------------

if (!include_matching){
  
  # read in the data denoting cluster membership
  cluster_pairs <- map_dfr(path_years, function(path){
    cluster_pairs <- read_csv(file.path(path, "data", "cluster_pairs.csv"))
    years <- str_extract(path, "[0-9]*_[0-9]*$")
    years <- str_split(years, '_')
    cluster_pairs$year1 <- years[[1]][1]
    cluster_pairs$year2 <- years[[1]][2]
    return(cluster_pairs)
  })
  
  # number of clusters per year
  cluster_pairs %>%
    group_by(year1, time) %>% 
    summarize(n_clusters = n_distinct(cluster)) %>% 
    ggplot(aes(x = year1, y = n_clusters, fill = time)) +
    geom_col(position = 'dodge', width = 0.7) +
    scale_x_discrete(labels = paste0(
      sort(unique(cluster_pairs$year1)),
      '\n-\n',
      as.numeric(sort(unique(cluster_pairs$year1))) + 1)) +
    # scale_y_continuous(breaks = 0:8) +
    labs(title = 'k clusters by year',
         x = NULL,
         y = 'k clusters',
         fill = NULL) +
    theme(legend.position = 'bottom')
  ggsave(file.path(path_to_batch, 'plots', 'k_clusters.png'),
         width = 9, height = 6)
  
} else {

  # read in the data
  transition_dfs <- map_dfr(path_years, function(path){
    transition_df <- read_csv(file.path(path, "data", "transition_matrix.csv"))
    years <- str_extract(path, "[0-9]*_[0-9]*$")
    years <- str_split(years, '_')
    transition_df$year1 <- years[[1]][1]
    transition_df$year2 <- years[[1]][2]
    return(transition_df)
  })
  
  # calculate mean rates by year
  stationary_rate <- transition_dfs %>%
    group_by(year = year1) %>% 
    summarize(rate = sum((t1 == t2) * n) / sum(n),
              .groups = 'drop')
  
  # calculate rate by cluster by year
  stationary_rate_cluster <- transition_dfs %>% 
    group_by(year = year1, t1) %>% 
    summarize(rate = sum((t1 == t2) * n) / sum(n),
              .groups = 'drop')
  
  # plot rate
  stationary_rate %>% 
    mutate(year = as.numeric(year)) %>% 
    ggplot(aes(x = year, y = rate)) +
    geom_line() +
    geom_point() +
    geom_point(data = stationary_rate_cluster,
               aes(x = as.numeric(year), y = rate, group = t1),
               shape = 5) +
    scale_x_continuous(breaks = 2004:2019,
                       labels = paste0(2004:2019, '\n-\n', 2005:2020)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    labs(title = 'Rate that observations transition to like cluster inter-year',
         subtitle = 'Line represents mean rate. ⋄ represent individual cluster rates',
         x = NULL,
         y = 'Rate')
  ggsave(file.path(path_to_batch, 'plots', 'mean_transition_rate.png'),
         width = 9, height = 6)
  
  # how does this compare to baseline noise? 
  # e.g. if it was totally random, what would be the transition rate
  # needs to account for k clusters and n per cluster
  
  # unique pairs of ks
  k_pairs <- transition_dfs %>%
    group_by(year = year1) %>% 
    summarize(t1 = n_distinct(t1),
              t2 = n_distinct(t2),
              .groups = 'drop')
  
  # find n per cluster per time per year
  n_t1 <- transition_dfs %>%
    group_by(year = year1, t1) %>% 
    summarize(n_t1 = sum(n)) %>%
    group_by(year) %>% 
    nest() %>% 
    ungroup()
  n_t2 <- transition_dfs %>%
    group_by(year = year1, t2) %>% 
    summarize(n_t2 = sum(n)) %>%
    group_by(year) %>% 
    nest() %>% 
    ungroup()
  k_pairs <- k_pairs %>% 
    full_join(n_t1, by = 'year') %>% 
    full_join(n_t2, by = 'year') %>% 
    rename(k_t1 = t1, k_t2 = t2, data_t1 = data.x, data_t2 = data.y)
  
  # simulate the match rate if outcomes were completely random
  simulated_rates <- apply(X = k_pairs, MARGIN = 1, FUN = function(row){
    
    # proportion of observations in each cluster at t1 and t2
    prop_t1 <- row$data_t1$n_t1 / row$k_t1
    # prop_t2 <- row$data_t2$n_t2 / row$k_t2
    prop_t2 <- rep(1/row$k_t2, row$k_t2)
    #TODO: should the t2 proportions be derived or random?
    
    # cluster labels from t1 and t2
    labels_t1 <- row$data_t1$t1
    labels_t2 <- row$data_t2$t2
    
    # sim
    baseline_rate <- mean(
      sample(labels_t1, size = 100000, replace = TRUE, prob = prop_t1)
      ==
      sample(labels_t2, size = 100000, replace = TRUE, prob = prop_t2)
    )
  })
  
  # plot rate
  stationary_rate %>% 
    mutate(year = as.numeric(year),
           simulated_rate = simulated_rates) %>% 
    ggplot(aes(x = year, y = rate)) +
    geom_line() +
    geom_point() +
    geom_point(aes(y = simulated_rate), 
               shape = 4) +
    scale_x_continuous(breaks = 2004:2019,
                       labels = paste0(2004:2019, '\n-\n', 2005:2020)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    labs(title = 'Mean rate that observations transition to like cluster inter-year',
         subtitle = 'x point represents simulated rate if t1/t2 transitions were random',
         caption = 'Simulated points calculated from random samples with same k and proportion by year',
         x = NULL,
         y = 'Rate')
  ggsave(file.path(path_to_batch, 'plots', 'mean_transition_rate_simulated.png'),
         width = 9, height = 6)
  
  
  # how many years have the same number of clusters -------------------------
  
  transition_dfs %>% 
    group_by(year1) %>% 
    summarize(t1 = n_distinct(t1),
              t2 = n_distinct(t2)) %>%
    pivot_longer(-year1) %>% 
    ggplot(aes(x = year1, y = value, fill = name)) +
    geom_col(position = 'dodge', width = 0.7) +
    scale_x_discrete(labels = paste0(
      sort(unique(transition_dfs$year1)), 
      '\n-\n', 
      as.numeric(sort(unique(transition_dfs$year1))) + 1)) +
    scale_y_continuous(breaks = 0:8) +
    labs(title = 'k clusters by year',
         x = NULL,
         y = 'k clusters',
         fill = NULL) +
    theme(legend.position = 'bottom')
  ggsave(file.path(path_to_batch, 'plots', 'k_clusters.png'),
         width = 9, height = 6)
  
  
  
  # how many observations not used in time 2 due to matching ----------------
  
  # read in the data
  obs_unmatched <- map_dfr(path_years, function(path){
    n_unmatched_observations <- read_csv(file.path(path, "data", "t2_unmatched_observations.csv")) %>% 
      n_distinct(.$ID)
    df <- tibble(year = str_extract(path, "[0-9]*_[0-9]*$"),
                 n = n_unmatched_observations)
    return(df)
  })
  
  # plot it
  obs_unmatched %>% 
    ggplot(aes(x = year, y = n)) +
    geom_col() +
    scale_x_discrete(labels = paste0(2004:2019, '\n-\n', 2004:2020)) +
    scale_y_continuous() +
    labs(title = 'Number of observations thrown out due to lack of match',
         x = NULL,
         y = 'n observations without a match')
  ggsave(file.path(path_to_batch, 'plots', 'no_matches.png'),
         width = 9, height = 6)
  
  
  # bad matching ------------------------------------------------------------
  
  # how many observations were thrown out due to stratified matching
  
  # read in the data
  obs_with_no_match <- map_dfr(path_years, function(path){
    df <- read_csv(file.path(path, "data", "IDs_with_no_match.csv"),
             col_types = cols(ID = col_character()))
    df$year <- str_extract(path, "[0-9]*_[0-9]*$")
    return(df)
  })
  
  # plot it
  obs_with_no_match %>% 
    group_by(year) %>% 
    tally() %>% 
    full_join(tibble(year = years_run),
              by = 'year') %>% 
    ggplot(aes(x = year, y = n)) +
    geom_col() +
    scale_x_discrete(labels = paste0(2004:2019, '\n-\n', 2005:2020)) +
    scale_y_continuous() +
    labs(title = 'Number of observations thrown out due to stratified matching',
         x = NULL,
         y = 'n observations without a match')
  ggsave(file.path(path_to_batch, 'plots', 'no_matches_due_to_stratify.png'),
         width = 9, height = 6)
}


# labor force status ------------------------------------------------------

# read in the data
# obs_with_no_match <- map_dfr(path_years, function(path){
#   df <- read_csv(file.path(path, "data", "IDs_with_no_match.csv"),
#                  col_types = cols(ID = col_character()))
#   df$year <-   years <- str_extract(path, "[0-9]*_[0-9]*$")
#   return(df)
# })

