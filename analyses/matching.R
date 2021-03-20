# library(tidyverse)
library(arm)
# source('analyses/plots/ggplot_settings.R')
source('analyses/demographics.R')
source('analyses/helpers_analyses.R')
set.seed(44)

# convert year to boolean
demographics$treatment <- demographics$year == 2009


# propensity model --------------------------------------------------------

# TODO: scale inputs?

# construct formula to calculate propensity scores
propensity_formula <- reformulate(
  termlabels = matching_vars,
  response = 'treatment',
  intercept = TRUE
)

# build model
propensity_model <- glm(propensity_formula, family = binomial('logit'), data = demographics)
propensity_scores <- predict(propensity_model, type = 'response')
# range(propensity_scores)
# plot(density(propensity_scores))

# # alternative bayesian method
# propensity_model <- rstanarm::stan_glm(
#   propensity_formula,
#   family = binomial('logit'),
#   data = demographics,
#   algorithm = 'optimizing'
# )
# propensity_scores <- apply(rstanarm::posterior_linpred(propensity_model), 2, mean) # linear predictor scale
# propensity_scores <- apply(rstanarm::posterior_epred(propensity_model), 2, mean) # probability scale


# assessing overlap -------------------------------------------------------

# plot propensity scores by treatment group
tibble(pscore = propensity_scores,
       treatment = demographics$treatment) %>%
  mutate(year = if_else(treatment, 2009, 2007)) %>% 
  ggplot(aes(x = pscore, fill = as.factor(year))) +
  geom_histogram(color = 'grey90', alpha = 0.7, position = 'identity') +
  labs(title = 'Overlap of propensity scores by year',
       x = 'Propensity score',
       fill = NULL)
# ggsave("analyses/plots/overlap_propensity_scores.png", height = 5, width = 9)


# matching ----------------------------------------------------------------

## nearest neighbor -- with replacement
match_k1_w_glm <- MatchIt::matchit(
  propensity_formula,
  data = demographics,
  method = 'nearest', # 'optimal' 'full' 
  distance = "glm",
  link = "logit",
  replace = TRUE
)

## nearest neighbor -- without replacement
match_k1_wo_glm <- MatchIt::matchit(
  propensity_formula,
  data = demographics,
  method = 'nearest', # 'optimal' 'full' 
  distance = "glm",
  link = "logit",
  replace = FALSE
)

## caliper matching
# first search for idea width based on minimum std diff
caliper_widths <- seq(0.05, 0.4, 0.05)
caliper_search <- sapply(caliper_widths, function(width) {
  match_caliper_glm <- MatchIt::matchit(
    propensity_formula,
    data = demographics,
    method = 'nearest', 
    distance = "glm",
    link = "logit",
    caliper = width
  )
  balance_caliper <- calculate_balance(demographics, match_caliper_glm, propensity_model)
  mean_diff <- mean(balance_caliper$diff.means.matched[, 'diff.std'])
  return(mean_diff)
})
# plot(x = caliper_widths, y = caliper_search)
rm(caliper_widths, caliper_search)

# calculate using ideal caliper width
match_caliper_glm <- MatchIt::matchit(
  propensity_formula,
  data = demographics,
  method = 'nearest', # 'optimal' 'full' 
  distance = "glm",
  link = "logit",
  caliper = 0.10
)


## IPTW -- its unclear how to implement this with sequences b/c we would
  # have to weight the sequences
# iptw_weights <- ifelse(demographics$treatment == 1, 
#                        1,
#                        propensity_scores / (1 - propensity_scores))



# assess balance ----------------------------------------------------------

# calculate balance stats
balance_with <- calculate_balance(demographics, match_k1_w_glm, propensity_model)
balance_wo <- calculate_balance(demographics, match_k1_wo_glm, propensity_model)
balance_caliper <- calculate_balance(demographics, match_caliper_glm, propensity_model)

## combine all stats and plot the standardized difference in means
balance_df <- tibble(
  covs = setdiff(balance_with$covnames, 'treat'),
  unmatched = balance_with$diff.means.raw[, 'diff.std'],
  NN_with = balance_with$diff.means.matched[, 'diff.std'],
  NN_wo = balance_wo$diff.means.matched[, 'diff.std'],
  Caliper = balance_caliper$diff.means.matched[, 'diff.std']
)
options(scipen = 9999)
summary_means <- round(apply(balance_df[, 2:5], 2, function(x) mean(abs(x))), 4)
caption <- paste0(names(summary_means), ": ", summary_means)
names(caption) <- names(summary_means)
balance_df %>% 
  pivot_longer(-covs) %>% 
  ggplot(aes(x = value, y = covs, group = name, fill = name)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey70') +
  geom_point(alpha = 0.6, size = 4, pch = 21, color = 'grey10') +
  scale_fill_discrete(labels = caption) +
  labs(title = "Standardized difference in means between treatment and control",
       subtitle = 'Closer to zero is better',
       caption = "Legend values represent the mean of |means|",
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme(legend.position = 'bottom')
# ggsave("analyses/plots/balance_matched.png", height = 6, width = 9)


# final matches -----------------------------------------------------------

# create final matches
which.min(summary_means)
demographics_treated <- demographics[demographics$treatment,]
demographics_control <- demographics[match_k1_w_glm$match.matrix,]

# add pair id so its easy to identify the matched pairs
demographics_treated$pair_id <- 1:nrow(demographics_treated)
demographics_control$pair_id <- 1:nrow(demographics_control)


# examine overlap for key variables ---------------------------------------

# how many control participants are we keeping?
orig_in_control <- demographics$ID[demographics$year == 2007] %in% demographics_control$ID
sum(orig_in_control)
percent_kept <- mean(orig_in_control)
writeLines(paste0("We're losing ", round((1 - percent_kept) * 100, 2), "% of the control data"))
rm(percent_kept)

# what's our n for our target population?
bind_rows(demographics_treated, demographics_control) %>% 
  select(-pair_id) %>% 
  distinct() %>%
  filter(sex == 'female',
         n_child > 0) %>% 
  group_by(treatment, race, married) %>% # add more vars here
  tally() %>% 
  pivot_longer(cols = c('race', 'married')) %>% 
  mutate(year = if_else(treatment, '2009', '2007')) %>% 
  ggplot(aes(x = value, y = n, group = year, fill = year)) +
  geom_col(position = 'dodge') +
  facet_wrap(~name, scales = 'free') +
  labs(title = 'Counts of key groups within matched data',
       subtitle = 'Key groups = has children, is female',
       caption = 'Only includes distinct observations (i.e. removes duplicates due to matching with replacement)',
       x = NULL,
       fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom')
# ggsave("analyses/plots/counts_matched.png", height = 6, width = 9)


# write out matches -------------------------------------------------------

# write_csv(demographics_treated, path = 'data/matched_treatment.csv')
# write_csv(demographics_control, path = 'data/matched_control.csv')
