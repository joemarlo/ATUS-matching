# library(tidyverse)
library(arm)
# source('analyses/plots/ggplot_settings.R')
source('analyses/demographics.R')
source('analyses/helpers_analyses.R')
set.seed(44)

# convert year to boolean
demographics$treatment <- demographics$year == 2009


# propensity model --------------------------------------------------------

# remove stratify vars from propensity score model 
propensity_formula_blocked <- reformulate(
  termlabels = setdiff(matching_vars, blocking_vars),
  response = 'treatment',
  intercept = TRUE
)


# blocked propensity scores -----------------------------------------------

# split the data into blocks
demographics_blocked <- demographics %>% 
  group_by_at(all_of(blocking_vars)) %>% 
  mutate(block = cur_group_id()) %>% 
  add_tally() %>% 
  filter(n > 100) %>% # this throws out the smaller groups
  select(-n) %>% 
  group_split()

# calculate propensity scores  
match_k1_blocked <- map(demographics_blocked, function(tbl){
  MatchIt::matchit(
    propensity_formula_blocked,
    data = tbl,
    method = 'nearest', 
    distance = "glm",
    link = "logit",
    replace = TRUE
  )
})

# calculate balance for each block
balance_stats <- map2_dfr(match_k1_blocked, demographics_blocked, function(match_model, match_data){
  propensity_model <- glm(propensity_formula_blocked, family = binomial('logit'), data = match_data)
  balance_stats <- calculate_balance(match_data, match_model, propensity_model)
  balance_df <- tibble(
    block = unique(match_data$block),
    covs = setdiff(balance_stats$covnames, 'treat'),
    unmatched = balance_stats$diff.means.raw[, 'diff.std'],
    NN_with = balance_stats$diff.means.matched[, 'diff.std'],
  )
  return(balance_df)
})

# plot the balance
# should really look at overlap across all groups though
balance_stats %>% 
  group_by(block) %>% 
  summarize(balance = mean(abs(NN_with))) %>% 
  ggplot(aes(x = as.character(block), y = balance)) +
  geom_col() +
  labs(title = 'Balance summary stat by block',
       subtitle = 'Smaller is better',
       x = 'Block',
       y = 'Mean of absolute standardized difference in means')
balance_stats %>% 
  pivot_longer(-c('block', 'covs')) %>% 
  ggplot(aes(x = value, y = covs, group = name, fill = name)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey70') +
  geom_point(alpha = 0.6, size = 4, pch = 21, color = 'grey10') +
  facet_wrap(~block, ncol = 1) +
  labs(title = "Standardized difference in means between treatment and control",
       subtitle = 'Closer to zero is better',
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme(legend.position = 'bottom')

# final matches
final_matches <- map2_dfr(match_k1_blocked, demographics_blocked, function(match_model, match_data){
  
  # create treatment and control dfs that have matching orders
  demographics_treated <- match_data[match_data$treatment,]
  demographics_control <- match_data[match_model$match.matrix,]
  
  # add pair id so its easy to identify the matched pairs
  demographics_treated$pair_id <- paste0(demographics_treated$block, '_', 1:nrow(demographics_treated))
  demographics_control$pair_id <- paste0(demographics_control$block, '_', 1:nrow(demographics_control))
  
  # combine the data
  final_matches <- bind_rows(demographics_treated, demographics_control)
  
  # move id columns to first column
  final_matches <- select(final_matches, 'treatment', 'block', 'pair_id', 'ID', all_of(blocking_vars), everything())
  
  return(final_matches)
})
# final_matches %>% arrange(desc(n), pair_id) %>% View
