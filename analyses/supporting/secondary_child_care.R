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
  mutate(dairy_date = lubridate::ymd(TUDIARYDATE),
         dairy_month = lubridate::month(dairy_date))


# secondary childcare -----------------------------------------------------

# household children under 13 -- all day
# respondent file
# Total time spent during diary day providing secondary childcare for household children < 13 (in minutes)
'TRTHH'


respondents_with_children %>% 
  filter(dairy_month >= 5,
         year != 2003) %>% 
  group_by(year, sex) %>% 
  summarize(mean_secondary_childcare = mean(secondary_childcare, na.rm = TRUE),
            weighted_secondary_childcare = sum(survey_weight * secondary_childcare) / sum(survey_weight),
            .groups = 'drop') %>% 
  ggplot(aes(x = year, y = weighted_secondary_childcare)) +
  geom_line() +
  geom_point() +
  geom_smooth(data = respondents_with_children %>% 
                filter(dairy_month >= 5,
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
  mutate(quarter = ceiling(dairy_month / 3),
         quarter = as.Date(paste0(year, '-', quarter * 3, '-01')),
         is_covid_era = quarter > as.Date('2020-03-01')) %>% 
  group_by(quarter, sex, is_covid_era) %>% 
  summarize(mean_secondary_childcare = mean(secondary_childcare, na.rm = TRUE),
            weighted_secondary_childcare = sum(survey_weight * secondary_childcare) / sum(survey_weight),
            .groups = 'drop')
model_secondary_childcare <- nlme::gls(weighted_secondary_childcare ~ quarter + sex + is_covid_era,
          data = secondary_childcare_quarterly)
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

# cut by partner, n child, sex
respondents_with_children %>% 
  filter(year != 2003) %>% 
  mutate(quarter = ceiling(dairy_month / 3),
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

