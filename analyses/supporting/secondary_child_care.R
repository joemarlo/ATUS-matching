library(tidyverse)
library(rdrobust)
source(file.path('analyses', 'plots', 'ggplot_settings.R'))
source(file.path('data', 'helpers.R'))
source(file.path("analyses", "helpers_analyses.R"))

# read in the demographics data
demographics <- read_delim(file = file.path("data", "demographic.tsv"),
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           col_types = cols(metropolitan = col_character()))

# recode sex so its explicit
demographics$sex <- recode(demographics$sex,
                           '1' = 'Male',
                           '2' = 'Female')


# population --------------------------------------------------------------

# mean (unweighted) minutes per day for respondents with children
respondents_with_children <- demographics %>% 
  filter(age_youngest >= 0,
         n_child >= 1) %>% 
  left_join(select(atusresp_0320, ID = TUCASEID, secondary_childcare = TRTHH, TUDIARYDATE),
            by = 'ID') %>% 
  mutate(diary_date = lubridate::ymd(TUDIARYDATE),
         diary_month = lubridate::month(diary_date))


# secondary childcare -----------------------------------------------------

# household children under 13 -- all day
# respondent file
# Total time spent during diary day providing secondary childcare for household children < 13 (in minutes)
'TRTHH'


respondents_with_children %>% 
  filter(diary_month >= 5,
         year != 2003) %>% 
  group_by(year, sex) %>% 
  summarize(mean_secondary_childcare = mean(secondary_childcare, na.rm = TRUE),
            weighted_secondary_childcare = sum(survey_weight * secondary_childcare) / sum(survey_weight),
            .groups = 'drop') %>% 
  ggplot(aes(x = year, y = weighted_secondary_childcare)) +
  geom_line() +
  geom_point() +
  geom_smooth(data = respondents_with_children %>% 
                filter(diary_month >= 5,
                       year %notin% c(2003, 2020)) %>% 
                group_by(year, sex) %>% 
                summarize(mean_secondary_childcare = mean(secondary_childcare, na.rm = TRUE),
                          weighted_secondary_childcare = sum(survey_weight * secondary_childcare) / sum(survey_weight),
                          .groups = 'drop'),
              method = 'lm', se = FALSE) +
  facet_grid(~sex) +
  scale_y_continuous(labels = format_hour_minute) +
  labs(title = 'Mean daily time spent on secondary childcare for household children under 13',
       subtitle = paste0('Only includes respondents with children. n = ', 
                         scales::comma_format()(nrow(filter(respondents_with_children, year != 2003)))),
       caption = 'Only includes May-December',
       x = NULL,
       y = NULL)

# time series: just trying to add autoregressive error; rho is autocorrelation
secondary_childcare_quarterly <- respondents_with_children %>% 
  filter(year != 2003) %>% 
  mutate(quarter = ceiling(diary_month / 3),
         quarter = as.Date(paste0(year, '-', quarter * 3, '-01')),
         is_covid_era = quarter > as.Date('2020-03-01')) %>% 
  group_by(quarter, sex, is_covid_era) %>% 
  summarize(mean_secondary_childcare = mean(secondary_childcare, na.rm = TRUE),
            weighted_secondary_childcare = sum(survey_weight * secondary_childcare) / sum(survey_weight),
            .groups = 'drop') %>% 
  arrange(quarter, sex)

# assess autocorrelation
# female_ts <- secondary_childcare_quarterly %>% 
#   filter(sex == 'Female')
# ts(female_ts$weighted_secondary_childcare, start = female_ts$quarter)

# fit model
model_secondary_childcare <- nlme::gls(
  weighted_secondary_childcare ~ quarter + sex + is_covid_era,
  data = secondary_childcare_quarterly,
  corr = nlme::corAR1(form = ~ quarter | sex)) #form = ~ quarter | sex)) # TODO corr = corAR1(0.5, form = ~ Quarter|Sex) 
summary(model_secondary_childcare)
  
# plot the model
preds <- mutate(secondary_childcare_quarterly,
                preds = predict(model_secondary_childcare, newdata = secondary_childcare_quarterly))
ggplot(preds, aes(x = quarter, y = weighted_secondary_childcare)) +
  geom_line(color = 'grey50') + 
  geom_point(color = 'grey50') +
  geom_line(data = filter(preds, !is_covid_era),
            aes(y = preds), color = 'blue') + 
  geom_line(data = filter(preds, is_covid_era),
            aes(y = preds), color = 'blue') + 
  facet_wrap(~sex) +
  labs(title = 'Mean daily time spent on secondary childcare for household children under 13',
       subtitle = paste0('Only includes respondents with children. n = ', 
                         scales::comma_format()(nrow(filter(respondents_with_children, year != 2003)))),
       caption = 'Covid era defined as occuring on or after May 12 2020 due to data collection limitations',
       x = NULL,
       y = "Mean daily minutes")
# ggsave(file.path('analyses', 'supporting', 'plots', "model_secondary_childcare.png"),
#        height = 6, width = 10)

# 2019 vs 2020 using 2020 weights only
respondents_with_children %>% 
  filter(year %in% 2019:2020) %>% 
  mutate(quarter = ceiling(diary_month / 3),
         quarter = as.Date(paste0(year, '-', quarter * 3, '-01')),
         is_covid_era = quarter > as.Date('2020-03-01')) %>% 
  group_by(quarter, sex, is_covid_era) %>% 
  summarize(mean_secondary_childcare = mean(secondary_childcare, na.rm = TRUE),
            weighted_secondary_childcare = sum(survey_weight_2020 * secondary_childcare) / sum(survey_weight_2020),
            .groups = 'drop') %>% 
  ggplot(aes(x = quarter, y = weighted_secondary_childcare)) +
  geom_line(color = 'grey50') + 
  geom_point(color = 'grey50') +
  facet_wrap(~sex)
respondents_with_children %>% 
  filter(year %in% 2019:2020) %>% 
  group_by(year, sex) %>% 
  summarize(mean_secondary_childcare = mean(secondary_childcare, na.rm = TRUE),
            weighted_secondary_childcare = sum(survey_weight_2020 * secondary_childcare) / sum(survey_weight_2020),
            .groups = 'drop') %>% 
  ggplot(aes(x = year, y = weighted_secondary_childcare, group = sex, color = sex)) +
  geom_line() + 
  geom_point()

# boxplots by quarter
# TODO no weights for 2020!!!!!
respondents_with_children %>% 
  filter(year != 2003,
         survey_weight != -1) %>% 
  mutate(quarter = ceiling(diary_month / 3),
         quarter = as.Date(paste0(year, '-', quarter * 3, '-01')),
         is_covid_era = quarter > as.Date('2020-03-01'),
         is_covid_era = if_else(is_covid_era, 'Covid era', 'Pre Covid'),
         is_covid_era = factor(is_covid_era, levels = c('Pre Covid', 'Covid era'))) %>% 
  filter(secondary_childcare > 0) %>% 
  ggplot(aes(x = quarter, y = secondary_childcare, group = quarter, fill = is_covid_era)) +
  geom_boxplot(aes(weight = survey_weight)) +
  scale_fill_manual(values = c('grey70', 'steelblue')) +
  facet_wrap(~sex) +
  labs(title = 'Daily time spent on secondary childcare among respondents who provided secondary childcare',
       subtitle = paste0('Only includes respondents with children under age 13. n = ', 
                         scales::comma_format()(nrow(filter(respondents_with_children, year != 2003)))),
       caption = 'Covid era defined as occuring on or after May 12 2020 due to data collection limitations',
       x = NULL,
       y = "Mean daily minutes",
       fill = NULL) +
  theme(legend.position = 'bottom')
# ggsave(file.path('analyses', 'supporting', 'plots', "secondary_childcare_distribution.png"),
#        height = 6, width = 10)

# cut by partner, n child, sex
respondents_with_children %>% 
  filter(year != 2003) %>% 
  mutate(quarter = ceiling(diary_month / 3),
         quarter = as.Date(paste0(year, '-', quarter * 3, '-01')),
         n_child  = case_when(
           n_child == 1 ~ '1 child',
           n_child >= 4 ~ '4+',
           TRUE ~ as.character(n_child)),
         has_partner = case_when(
           has_partner == TRUE ~ 'Has partner',
           has_partner == FALSE ~ 'No partner',
           TRUE ~ 'NA')) %>% 
  group_by(quarter, sex, labor_force_status, has_partner) %>% 
  summarize(mean_secondary_childcare = mean(secondary_childcare, na.rm = TRUE),
            weighted_secondary_childcare = sum(survey_weight * secondary_childcare) / sum(survey_weight),
            n = n(),
            .groups = 'drop') %>% 
  # group_by(sex, has_partner, n_child) %>% 
  # arrange(sex, has_partner, n_child, quarter) %>% 
  # mutate(diff = weighted_secondary_childcare - lag(weighted_secondary_childcare)) %>% 
  ggplot(aes(x = quarter, y = weighted_secondary_childcare, color = sex)) +
  geom_vline(xintercept = as.Date('2020-06-01'),
             linetype = 'dashed', color = 'grey30') +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm', se = FALSE) +
  ggplot2::scale_color_discrete() +
  scale_y_continuous(labels = format_hour_minute) +
  facet_grid(has_partner~labor_force_status) +
  labs(title = 'Mean daily time spent on secondary childcare for household children under 13',
       subtitle = paste0('Only includes respondents with children. n = ', 
                         scales::comma_format()(nrow(filter(respondents_with_children, year != 2003)))),
       x = NULL,
       y = NULL,
       color = NULL) +
  theme(legend.position = 'bottom')


# model it
model <- rdrobust(
  y = tweet_tally$proportion,
  x = tweet_tally$index,
  c = cutpoint,
  p = 1,
  bwselect = 'msetwo'
)

# extract bandwidths
bandwidths <- model$bws['h', ]

# household children under 13 -- within a certain activity
# activity file
#Total time spent during activity providing secondary childcare for household children < 13 (in minutes)
'TRTHH_LN'

# all children under 13
# activity file
# Total time spent during activity providing secondary childcare for all children < 13 (in minutes)
'TRTCCTOT_LN'

# time spent with family
# Respondent File
# Total nonwork-related time respondent spent with household family members (in minutes
'TRTHHFAMILY'




# matching ----------------------------------------------------------------

# do matching here or functionize matching script?

