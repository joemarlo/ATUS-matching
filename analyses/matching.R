library(tidyverse)
source('analyses/plots/ggplot_settings.R')
source('analyses/demographics.R')
set.seed(44)

# convert year to boolean
demographics$treatment <- demographics$year == 2009


# propensity model --------------------------------------------------------

# scale inputs?

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

# alternative bayesian method
# propensity_model <- rstanarm::stan_glm(
#   propensity_formula,
#   family = binomial('logit'),
#   data = demographics,
#   algorithm = 'optimizing'
# )
# propensity_scores <- apply(rstanarm::posterior_linpred(propensity_model), 2, mean) # linear predictor scale
# propensity_scores <- apply(rstanarm::posterior_epred(propensity_model), 2, mean) # probability scale


# assessing overlap -------------------------------------------------------

# propensity scores by treatment group
tibble(pscore = propensity_scores,
       treatment = demographics$treatment) %>%
  mutate(year = if_else(treatment, 2009, 2007)) %>% 
  ggplot(aes(x = pscore, fill = as.factor(year))) +
  geom_histogram(alpha = 0.7, position = 'identity') +
  labs(title = 'Overlap of propensity scores by year',
       x = 'Propensity score',
       fill = NULL)


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

# create df of the treated data and the matched control data
match_indices <- as.numeric(match_k1_w_glm$match.matrix)
control_df <- distinct(demographics[match_indices,])
treated_df <- demographics[demographics$treatment,]
matches <- bind_rows(control_df, treated_df)

# calculate balance stats
balance_with <- arm::balance(demographics, matches, propensity_model)
# plot_diff_means(balance_with)


## nearest neighbor -- without replacement
match_k1_wo_glm <- MatchIt::matchit(
  propensity_formula,
  data = demographics,
  method = 'nearest', # 'optimal' 'full' 
  distance = "glm",
  link = "logit",
  replace = FALSE
)

# create df of the treated data and the matched control data
match_indices <- as.numeric(match_k1_wo_glm$match.matrix)
control_df <- distinct(demographics[match_indices,])
matches <- bind_rows(control_df, treated_df)

# calculate balance stats
balance_wo <- arm::balance(demographics, matches, propensity_model)


## caliper matching
match_caliper_glm <- MatchIt::matchit(
  propensity_formula,
  data = demographics,
  method = 'nearest', # 'optimal' 'full' 
  distance = "glm",
  link = "logit",
  caliper = 0.1
)

# create df of the treated data and the matched control data
match_indices <- as.numeric(match_caliper_glm$match.matrix)
control_df <- distinct(demographics[match_indices,])
matches <- bind_rows(control_df, treated_df)

# calculate balance stats 
balance_caliper <- arm::balance(demographics, matches, propensity_model)

## IPTW


# assess balance ----------------------------------------------------------

# combine all and plot the standardized difference in means
balance_df <- tibble(
  covs = setdiff(balance_with$covnames, 'treat'),
  unmatched = balance_with$diff.means.raw[, 'diff.std'],
  NN_with = balance_with$diff.means.matched[, 'diff.std'],
  NN_wo = balance_wo$diff.means.matched[, 'diff.std'],
  Caliper = balance_caliper$diff.means.matched[, 'diff.std']
)

options(scipen=9999)
summary_means <- apply(balance_df[, 2:5], 2, mean) %>% round(4)
caption <- paste0(names(summary_means), ": ", summary_means)
names(caption) <- names(summary_means)
balance_df %>% 
  pivot_longer(-covs) %>% 
  ggplot(aes(x = value, y = covs, group = name, color = name)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey70') +
  geom_point(alpha = 0.6, size = 4) +
  scale_color_discrete(labels = caption) +
  labs(title = "Standardized difference in means",
       caption = "Legend values represent the mean of means",
       x = NULL,
       y = NULL,
       color = NULL) +
  theme(legend.position = 'bottom')
ggsave("analyses/plots/balance_assessment.png", height = 5, width = 9)

