# library(tidyverse)
library(arm)
# source('analyses/plots/ggplot_settings.R')
source('analyses/demographics.R')
source('analyses/helpers_analyses.R')
set.seed(44)


# pre-processing ----------------------------------------------------------

# convert year to boolean
demographics$treatment <- demographics$year == 2009

# remove unnecessary columns
demographics_trimmed <- demographics[, matching_vars]

# split into treat and control
demographics_treatment <- demographics_trimmed[demographics$treatment,]
demographics_control <- demographics_trimmed[!demographics$treatment,]

# dummy code categoricals
demographics_treatment <- fastDummies::dummy_cols(
  demographics_treatment, 
  remove_selected_columns = TRUE,
  remove_first_dummy = TRUE)
demographics_control <- fastDummies::dummy_cols(
  demographics_control, 
  remove_selected_columns = TRUE,
  remove_first_dummy = TRUE)

# clean up column names
col_names <- colnames(demographics_treatment)
col_names <- stringr::str_replace_all(col_names, '[.]|[-]', " ")
col_names <- stringr::str_squish(col_names)
col_names <- stringr::str_replace_all(col_names, "[ ]", "_")
colnames(demographics_treatment) <- col_names
colnames(demographics_control) <- col_names


# weighting ---------------------------------------------------------------

# TODO: this is all TBD; unsure how to weight mahalanobis distance
# prescaling and pre-weighting before calcualting mdistance doesn't make sense
# http://isl.anthropomatik.kit.edu/pdf/Woelfel2005.pdf
# https://rdrr.io/cran/WMDB/man/wmahalanobis.html
# https://www.researchgate.net/publication/290037437_Application_of_weighted_Mahalanobis_distance_discriminant_analysis_method_to_classification_of_rock_mass_quality
# just weight the covariance matrix before inverting?
  # cov_weighted <- weight %*% solve(cov)
  # mdist <- diag(x %*% cov_weighted %*% t(x))

# scale variables
# demographics_treatment <- as_tibble(scale(demographics_treatment))
# demographics_control <- as_tibble(scale(demographics_control))
# colnames(demographics_treatment) <- col_names
# colnames(demographics_control) <- col_names

# check colmeans and variance
# tibble(
#   column = col_names,
#   means = round(colMeans(demographics_treatment), 10),
#   sd = apply(demographics_treatment, 2, sd)
# ) %>% View
  
# set variable weights
var_weights <- c(
  'age' = 1,
  'fam_income' = 1,
  'child_in_HH' = 1,
  'n_child' = 1,
  'age_youngest' = 1,
  'elder_in_HH' = 1,
  'sex_male' = 1,
  'race_black' = 1,
  'race_other' = 1,
  'race_white' = 1,
  'married_not_married' = 1,
  'education_HS' = 1,
  'education_Some_college' = 1,
  'education_Bachelors' = 1,
  'education_Masters' = 1,
  'education_Doctoral' = 1,
  'region_Northeast' = 1,
  'region_South' = 1,
  'region_West' = 1,
  'labor_force_status_employed_at_work' = 1,
  'labor_force_status_not_in_labor_force' = 1,
  'labor_force_status_unemployed_looking' = 1,
  'labor_force_status_unemployed_on_layoff' = 1,
  'partner_working_NA' = 1,
  'partner_working_not_employed' = 1,
  'metropolitan_non_metropolitan' = 1
)

# apply weights column-wise
# demographics_treatment <- as_tibble(t(t(demographics_treatment) * var_weights))

# apply weights to covariance matrix and calculate mdistance
X <- as.matrix(demographics_treatment[, names(var_weights)])
X_cov <- cov(X)
cov_weighted <- diag(var_weights) %*% solve(X_cov)
X <- sweep(X, 2, colMeans(X))
mdist <- diag(X %*% cov_weighted %*% t(X))
mdist_baseR <- rowSums(X %*% cov_weighted * X) #stats::mahalanobis
all.equal(mdist, mdist_baseR)

# TODO mdist_weighted()


# non weighted method -----------------------------------------------------

# calculate mahalanobis distance
demographics_mdistance <- StatMatch::mahalanobis.dist(
  data.x = demographics_treatment,
  data.y = demographics_control,
  vc = cov(demographics_treatment)
)

# get match by choosing row with smallest distance
# this matches all of the treatments to some of the controls
#   akin to matching with replacement
match_indices <- apply(demographics_mdistance, 1, which.min)
# length(match_indices) == nrow(demographics_treatment)


# create dataframe of the matches -----------------------------------------

# create treatment and control dfs that have matching orders
demographics_treated <- demographics[demographics$treatment,]
demographics_control <- demographics[!demographics$treatment,][match_indices,]

# add pair id so its easy to identify the matched pairs
demographics_treated$pair_id <- 1:nrow(demographics_treated)
demographics_control$pair_id <- 1:nrow(demographics_control)

# combine the data
final_matches <- bind_rows(demographics_treated, demographics_control)

# move id columns to first column
final_matches <- dplyr::select(final_matches, 'treatment', 'pair_id', 'ID', everything())

# final_matches %>% arrange(pair_id) %>% View
# final_matches %>% arrange_at(c(blocking_vars, 'pair_id')) %>% View


# assess balance and overlap ----------------------------------------------

# TODO: calculate balance

# plot distributions by variable
final_matches %>% 
  dplyr::select(-c('treatment', 'pair_id', 'ID')) %>% 
  distinct() %>% 
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
               unique(final_matches$sex),
               unique(final_matches$labor_force_status),
               levels(final_matches$education),
               unique(final_matches$metropolitan),
               sort(unique(final_matches$fam_income))[-1])
  )
  ) %>% 
  ggplot(aes(x = value, group = year, fill = year)) +
  geom_bar(aes(y = ..prop..), position = 'dodge', stat = 'count') +
  scale_y_continuous(labels = scales::percent_format(1)) +
  facet_wrap(~name, scales = 'free', ncol = 3) +
  labs(title = 'Counts of key groups within matched data',
       subtitle = "Matching via Mahalanobis distance",
       caption = 'Only includes distinct observations (i.e. removes duplicates due to matching with replacement)',
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom')
# ggsave("analyses/plots/counts_matched_mahalanobis.png", height = 12, width = 9)


# privileged variables ----------------------------------------------------

# how many of the pairs perfectly match on each variable
match_summary <- final_matches %>% 
  group_by(pair_id) %>% 
  summarize(across(all_of(matching_vars), 
                   ~ first(.x) == last(.x))) %>%
  dplyr::select(-pair_id) %>% 
  mutate(n_matches = rowSums(.))
match_summary %>% 
  dplyr::select(all_of(blocking_vars)) %>% 
  mutate(n_matches = rowSums(.)) %>% 
  ggplot(aes(x = n_matches)) +
  geom_bar(aes(y = ..prop..)) +
  scale_x_continuous(breaks = 1:length(blocking_vars)) +
  scale_y_continuous(labels = scales::percent_format(1),
                     breaks = seq(0, 1, 0.1)) +
  geom_hline(yintercept = 1, linetype = 'dashed') +
  labs(title = 'Proportion of matches that match perfectly on: ',
       subtitle = paste0(
         paste0(blocking_vars, collapse = ', '),
         '\nMethodology: mahalanobis, no blocking'
       ),
       x = 'Number of matches across all privileged variables',
       y = 'Proportion of all pairs')
# ggsave('analyses/plots/privileged_vars_all_mahalanobis.png', height = 5, width = 9)
match_summary %>%
  summarize(across(all_of(matching_vars), mean)) %>% 
  pivot_longer(everything()) %>% 
  mutate(Privileged = name %in% blocking_vars,
         isNumeric = name %in% vars_numeric,
         isNumeric = if_else(isNumeric, 'Numeric variables', 'Categorical variables')) %>% 
  ggplot(aes(x = reorder(name, -Privileged), y = value, fill = Privileged)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'darkgreen') +
  geom_hline(yintercept = 0.9, linetype = 'dashed', color = 'red') +
  scale_y_continuous(labels = scales::percent_format(1),
                     breaks = seq(0, 1, 0.1)) +
  facet_grid(~isNumeric, scales = 'free_x') +
  labs(title = 'How many pairs matched perfectly for each variable?',
       subtitle = paste0("Yellow variables are not explicitly privileged but are highlighted for emphasis",
                         '\nMethodology: mahalanobis, no blocking'),
       x = NULL,
       y = 'Proportion of all pairs') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')
# ggsave('analyses/plots/perfect_matches_mahalanobis.png', height = 5, width = 9)

# difference within matched pairs for numeric vars
final_matches %>% 
  group_by(pair_id) %>% 
  summarize(across(all_of(vars_numeric), 
                   ~ first(.x) - last(.x))) %>% 
  pivot_longer(-pair_id) %>% 
  ggplot(aes(x = value)) +
  geom_boxplot() +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~name, scales = 'free') +
  labs(title = 'Difference within matched pairs for numeric variables',
       subtitle = 'Methodology: mahalanobis, no blocking',
       x = NULL,
       y = NULL)
# ggsave('analyses/plots/numeric_differences_mahalanobis.png', height = 5, width = 9)

# TODO: deeper dive into privileged vars; e.g look at % match by each race 


# write out matches -------------------------------------------------------

# write_csv(demographics_treated, path = 'data/matched_treatment_mahalanobis.csv')
# write_csv(demographics_control, path = 'data/matched_control_mahalanobis.csv')