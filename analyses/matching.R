# library(tidyverse)
library(arm)
# source('analyses/plots/ggplot_settings.R')
source('analyses/demographics.R')
source('analyses/helpers_analyses.R')
set.seed(44)

# convert year to boolean
demographics$treatment <- demographics$year == 2009


# propensity model --------------------------------------------------------

# TODO: scale inputs? not necessary for regression but may be for other models

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
  geom_histogram(bins = 60, color = 'grey90', alpha = 0.7, position = 'identity') +
  scale_x_continuous(limits = 0:1) +
  labs(title = 'Overlap of propensity scores by year',
       x = 'Propensity score',
       fill = NULL) +
  theme(legend.position = 'bottom')
# ggsave("analyses/plots/overlap_propensity_scores.png", height = 5, width = 9)


# matching ----------------------------------------------------------------

# TODO: implement hard matching via stratification?
# stratify_vars <- c('sex', 'race', 'metropolitan', 'region', 'married', 'labor_force_status') #'essential_worker # add child_in_HH?
stratify_vars <- c('sex', 'race', 'metropolitan', 'married') #'essential_worker
# i think group_by(all_of(stratify_vars)) %>% group_split() %>% fit model
demographics %>% group_by_at(all_of(stratify_vars)) %>% tally() %>% arrange(desc(n)) %>% View



# test: blocked prop scores -----------------------------------------------

# remove stratify vars from propensity score model 
propensity_formula_blocked <- reformulate(
  termlabels = setdiff(matching_vars, stratify_vars),
  response = 'treatment',
  intercept = TRUE
)

# split the data into blocks
demographics_blocked <- demographics %>% 
  group_by_at(all_of(stratify_vars)) %>% 
  add_tally() %>% 
  mutate(block = cur_group_id()) %>% 
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
  demographics_treated <- match_data[match_data$treatment,]
  demographics_control <- match_data[match_model$match.matrix,]
  
  # add pair id so its easy to identify the matched pairs
  demographics_treated$pair_id <- paste0(demographics_treated$block, '_', 1:nrow(demographics_treated))
  demographics_control$pair_id <- paste0(demographics_control$block, '_', 1:nrow(demographics_control))
  
  # combine the data
  final_matches <- bind_rows(demographics_treated, demographics_control)
  
  # move id columns to first
  final_matches <- select(final_matches, 'block', 'pair_id', everything())
  
  return(final_matches)
})
# final_matches %>% arrange(desc(n), pair_id) %>% View


# end test ----------------------------------------------------------------

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
caliper_widths <- seq(0.05, 0.3, 0.05)
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
  caliper = 0.15
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
legend_stats <- paste0(names(summary_means), ": ", summary_means)
names(legend_stats) <- names(summary_means)
balance_df %>% 
  pivot_longer(-covs) %>% 
  ggplot(aes(x = value, y = covs, group = name, fill = name)) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey70') +
  geom_point(alpha = 0.6, size = 4, pch = 21, color = 'grey10') +
  scale_fill_discrete(labels = legend_stats) +
  labs(title = "Standardized difference in means between treatment and control",
       subtitle = 'Closer to zero is better',
       caption = "Legend values represent the mean of |means|",
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme(legend.position = 'bottom')
# ggsave("analyses/plots/balance_matched.png", height = 9, width = 9)


# final matches -----------------------------------------------------------

# create final matches
which.min(summary_means)
demographics_treated <- demographics[demographics$treatment,]
demographics_control <- demographics[match_k1_w_glm$match.matrix,]

# add pair id so its easy to identify the matched pairs
demographics_treated$pair_id <- 1:nrow(demographics_treated)
demographics_control$pair_id <- 1:nrow(demographics_control)

## plot the matches
demographics_treated %>% 
  bind_rows(demographics_control) %>% 
  dplyr::select(ID, pair_id, treatment) %>% 
  left_join(tibble(p_score = propensity_scores, ID = demographics$ID),
            by = 'ID') %>% 
  mutate(year = if_else(treatment, '2009', '2007')) %>% 
  left_join(pivot_wider(., id_cols = 'pair_id', values_from = p_score, names_from = year), 
            by = 'pair_id', copy = TRUE) %>%
  ggplot(aes(x = p_score, y = year, fill = year)) +
  geom_segment(aes(x = `2009`, xend = `2007`,
                   y = '2009', yend = '2007'),
               alpha = 0.01, lineend = 'round', linejoin = 'mitre', color = 'grey20',
               size = 1.2, arrow = arrow(length = unit(0.06, "npc"))) +
  geom_point(shape = 21, color = 'grey40', size = 3, stroke = 1, alpha = 0.05) +
  scale_x_continuous(limits = 0:1) +
  labs(title = 'The arrows show how the treatment observations are matched to the control observations\n',
       x = 'Propensity score',
       y = NULL,
       fill = NULL)


# examine overlap for key variables ---------------------------------------

# how many control participants are we keeping?
orig_in_control <- demographics$ID[demographics$year == 2007] %in% demographics_control$ID
sum(orig_in_control)
percent_kept <- mean(orig_in_control)
writeLines(paste0("We're losing ", round((1 - percent_kept) * 100, 2), "% of the control data through matching"))
rm(percent_kept)

## what's our n for our target population?
target_pop <- bind_rows(demographics_treated, demographics_control) %>% 
  dplyr::select(-pair_id) %>% 
  distinct() %>%
  filter(sex == 'female',
         n_child > 0) %>% 
  dplyr::select(-ID, -treatment, -sex) #year, race, married, n_child, age_youngest, region, fam_income) # add more vars here
subtitle <- paste0(
  'Data only includes respondents that are female and have children in the household', 
  '\nn 2007 = ', scales::comma_format()(sum(target_pop$year == 2007)),
  ';   n 2009 = ', scales::comma_format()(sum(target_pop$year == 2009))
)
# plot the counts
target_pop %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(cols = -year) %>% 
  mutate(value = factor(
    value, 
    levels = c(0:99, 
               'asian', 'black', 'white', 'other',
               'married', 'not married',
               'Midwest', 'Northeast', 'South', 'West',
               'TRUE', 'FALSE',
               'employed', 'not employed',
               unique(target_pop$labor_force_status),
               levels(target_pop$education),
               unique(target_pop$metropolitan),
               sort(unique(target_pop$fam_income))[-1])
    )
  ) %>% 
  ggplot(aes(x = value, group = year, fill = year)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~name, scales = 'free', ncol = 3) +
  labs(title = 'Counts of key groups within matched data',
       subtitle = subtitle,
       caption = 'Only includes distinct observations (i.e. removes duplicates due to matching with replacement)',
       x = NULL,
       fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom')
# ggsave("analyses/plots/counts_matched.png", height = 12, width = 9)


# write out matches -------------------------------------------------------

# write_csv(demographics_treated, path = 'data/matched_treatment.csv')
# write_csv(demographics_control, path = 'data/matched_control.csv')
