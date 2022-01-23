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


# childcare definitions ---------------------------------------------------

# household children under 13 -- all day
# respondent file
# Total time spent during diary day providing secondary childcare for household children < 13 (in minutes)
'TRTHH'

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


# count household children under 13 ----------------------------------------

# create var of household children  (own or not) under the age of 13
household_children <- atusrost_0320 %>% 
  group_by(ID = TUCASEID) %>% 
  summarize(n_child_13 = sum(TEAGE < 13 & TERRP != 40))

# add to demographics data
demographics <- left_join(demographics, household_children, by = 'ID')


# population --------------------------------------------------------------

# add secondary childcare (household children only) to demographics data
# remove 1Q and 2Q 2020
respondents <- demographics %>% 
  filter(year %notin% c(2003, 2004)) %>% 
  left_join(select(atusresp_0320, ID = TUCASEID, secondary_childcare = TRTHH, TUDIARYDATE),
            by = 'ID') %>% 
  mutate(diary_date = lubridate::ymd(TUDIARYDATE),
         diary_month = lubridate::month(diary_date),
         quarter = ceiling(diary_month / 3),
         quarter = as.Date(paste0(year, '-', quarter * 3, '-01')),
         quarter = quarter + months(1) - 1,
         is_covid_era = quarter > as.Date('2020-03-31')) %>% 
  filter(quarter %notin% as.Date(c('2020-03-31', '2020-06-30')))

# filter to only respondents with children
respondents_with_children <- filter(respondents, n_child_13 > 0)


# primary childcare -------------------------------------------------------

# TODO: filter these activities to just childcare --> check specific codes
# i think childcare defined as under 18 but we can filter to households with only children < 13 CONFIRM
# probably have to replace this with the activity file detail
c("hh children", "nonhh children")
childcare_cols <- specific.codes
childcare_cols <- c('Caring For Household Member', 'Caring For Nonhousehold Members') 
childcare_cols <- descriptions[descriptions$description %in% childcare_cols,]

childcare_cols <- specific.codes %>%
  filter(str_detect(Description, 'hh children'), 
         !str_detect(Description, 'nonhh children')) %>% 
  transmute(activity = paste0('t', Code), 
            description = Description)
  

# build dataframe that summarizes care by the ID
childcare_summary <- atussum_0320 %>% 
  select(ID = TUCASEID, any_of(childcare_cols$activity)) %>% 
  pivot_longer(-ID) %>% 
  left_join(childcare_cols, by = c('name' = 'activity')) %>% 
  group_by(ID) %>% #, description) %>% 
  summarize(minutes = sum(value),
            .groups = 'drop')

# calculate weighted childcare by quarter
PCC_by_quarter <- respondents_with_children %>% 
  select(ID, survey_weight, quarter, is_covid_era) %>% 
  left_join(childcare_summary, by = "ID") %>% 
  group_by(quarter, is_covid_era) %>% 
  summarize(mean_childcare = mean(minutes, na.rm = TRUE),
            weighted_childcare = sum(survey_weight * minutes) / sum(survey_weight),
            .groups = 'drop') %>% 
  arrange(quarter) %>% 
  mutate(index = round(as.numeric(((quarter - min(quarter)) / 92) + 1)),
         is_covid_era = if_else(is_covid_era, 'Covid era', 'Pre Covid'),
         is_covid_era = factor(is_covid_era, levels = c('Pre Covid', 'Covid era')),
         quarter_ex_year = str_sub(quarter, 6)) %>% 
  rowwise() %>% 
  mutate(quarter_ex_year = which(quarter_ex_year == c('03-31', '06-30', '09-30', '12-31'))) %>% 
  ungroup()

# plot it
# y_min <- 4.25*60
# y_max <- 5.75*60
PCC_by_quarter %>% 
  ggplot(aes(x = quarter, y = weighted_childcare)) +
  geom_line(data = filter(PCC_by_quarter, quarter %in% as.Date(c('2019-12-31', '2020-09-30'))),
            color = 'grey50', linetype = 'dashed') +
  geom_line(aes(color = is_covid_era)) +
  geom_point(aes(color = is_covid_era)) +
  # facet_wrap(~description, ncol = 2, scales = 'free_y') +
  ggplot2::scale_color_discrete() +
  scale_x_date(date_breaks = '1 year', date_labels = "'%y") +
  scale_y_continuous(#limits = c(y_min, y_max),
                     #breaks = seq(y_min, y_max, by = 15),
                     labels = format_hour_minute) +
  labs(title = 'Mean daily time spent on primary childcare for household children', # confirm age group
       subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
                         scales::comma_format()(nrow(respondents_with_children))),
       x = NULL,
       y = 'Hour:minutes on primary childcare',
       color = NULL) +
  theme(legend.position = 'bottom')
# ggsave(file.path('analyses', 'supporting', 'plots', "childcare_primary_timeseries.png"),
#        height = 6, width = 10)

# fit generalized least squares with dummy for quarter
model_primary_childcare <- nlme::gls(
  weighted_childcare ~ index + is_covid_era + quarter_ex_year,
  data = PCC_by_quarter,
  corr = nlme::corAR1(form = ~ index)) #form = ~ quarter | sex)) # TODO corr = corAR1(0.5, form = ~ Quarter|Sex) 
tmp <- summary(model_primary_childcare)
tmp$tTable %>% 
  as_tibble() %>% 
  mutate(Coef = names(tmp$coefficients)) %>% 
  slice(-1) %>% 
  select(Coef, Beta = Value, Std.Error, `p-value`)
rm(tmp)

## split by sex
# calculate weighted childcare by quarter
PCC_by_quarter_sex <- respondents_with_children %>% 
  select(ID, survey_weight, quarter, is_covid_era, sex) %>% 
  left_join(childcare_summary, by = "ID") %>% 
  group_by(quarter, is_covid_era, sex) %>% 
  summarize(mean_childcare = mean(minutes, na.rm = TRUE),
            weighted_childcare = sum(survey_weight * minutes) / sum(survey_weight),
            .groups = 'drop') %>% 
  arrange(quarter) %>% 
  mutate(index = round(as.numeric(((quarter - min(quarter)) / 92) + 1)),
         is_covid_era = if_else(is_covid_era, 'Covid era', 'Pre Covid'),
         is_covid_era = factor(is_covid_era, levels = c('Pre Covid', 'Covid era')),
         quarter_ex_year = str_sub(quarter, 6)) %>% 
  rowwise() %>% 
  mutate(quarter_ex_year = which(quarter_ex_year == c('03-31', '06-30', '09-30', '12-31'))) %>% 
  ungroup()

# plot it
PCC_by_quarter_sex %>% 
  ggplot(aes(x = quarter, y = weighted_childcare)) +
  geom_line(data = filter(PCC_by_quarter_sex, quarter %in% as.Date(c('2019-12-31', '2020-09-30'))),
            color = 'grey50', linetype = 'dashed') +
  geom_line(aes(color = is_covid_era)) +
  geom_point(aes(color = is_covid_era)) +
  # facet_wrap(~description, ncol = 2, scales = 'free_y') +
  ggplot2::scale_color_discrete() +
  scale_x_date(date_breaks = '1 year', date_labels = "'%y") +
  scale_y_continuous(#limits = c(y_min, y_max),
    #breaks = seq(y_min, y_max, by = 15),
    labels = format_hour_minute) +
  facet_wrap(~sex) +
  labs(title = 'Mean daily time spent on primary childcare for household children', # confirm age group
       subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
                         scales::comma_format()(nrow(respondents_with_children))),
       x = NULL,
       y = 'Hour:minutes on primary childcare',
       color = NULL) +
  theme(legend.position = 'bottom')
# ggsave(file.path('analyses', 'supporting', 'plots', "childcare_primary_timeseries_by_sex.png"),
#        height = 6, width = 10)

# fit generalized least squares with dummy for quarter
model_primary_childcare_sex <- nlme::gls(
  weighted_childcare ~ index + is_covid_era + quarter_ex_year + sex,
  data = PCC_by_quarter_sex,
  corr = nlme::corAR1(form = ~ index | sex)) #form = ~ quarter | sex)) # TODO corr = corAR1(0.5, form = ~ Quarter|Sex) 
tmp <- summary(model_primary_childcare_sex)
tmp$tTable %>% 
  as_tibble() %>% 
  mutate(Coef = names(tmp$coefficients)) %>% 
  slice(-1) %>% 
  select(Coef, Beta = Value, Std.Error, `p-value`)
rm(tmp)


# demographics
# age, sex, n_child, labor_force_status, partner_working, has_partner, elder_in_HH, race, education, metropolitan
respondents_with_children %>% 
  filter(is_covid_era) %>% 
  select(ID,  age, sex, n_child, labor_force_status, partner_working, has_partner, 
         elder_in_HH, race, education, metropolitan) %>% 
  left_join(childcare_summary, by = "ID") %>% 
  mutate(age = cut(age, seq(0, 100, by = 20)),
         across(-minutes, as.character)) %>% 
  select(-ID) %>% 
  pivot_longer(-minutes) %>% 
  group_by(name, value) %>% 
  summarize(minutes = mean(minutes, na.rm = TRUE)) %>% 
  ggplot(aes(x = value, y = minutes)) +
  geom_col() +
  scale_y_continuous(labels = format_hour_minute) +
  facet_wrap(~name, scales = 'free_x') +
  labs(title = 'Marginal distributions of daily time spent on primary childcare (2020)',
       subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
                         scales::comma_format()(nrow(respondents_with_children))),
       x = NULL,
       y = 'Mean daily time (unweighted)') +
  theme(axis.text.x = element_text(angle = -40, hjust = 0))
# ggsave(file.path('analyses', 'supporting', 'plots', "childcare_primary_marginals.png"),
#        height = 8, width = 10)

 
# secondary childcare -----------------------------------------------------

# time series by quarter
# add index for regression
SSC_by_quarter <- respondents_with_children %>% 
  group_by(quarter, is_covid_era) %>% 
  summarize(mean_secondary_childcare = mean(secondary_childcare, na.rm = TRUE),
            weighted_secondary_childcare = sum(survey_weight * secondary_childcare) / sum(survey_weight),
            .groups = 'drop') %>% 
  arrange(quarter) %>% 
  mutate(index = round(as.numeric(((quarter - min(quarter)) / 92) + 1)),
         is_covid_era = if_else(is_covid_era, 'Covid era', 'Pre Covid'),
         is_covid_era = factor(is_covid_era, levels = c('Pre Covid', 'Covid era')),
         quarter_ex_year = str_sub(quarter, 6)) %>% 
  rowwise() %>% 
  mutate(quarter_ex_year = which(quarter_ex_year == c('03-31', '06-30', '09-30', '12-31'))) %>% 
  ungroup()

# fit generalized least squares with dummy for quarter
model_secondary_childcare <- nlme::gls(
  weighted_secondary_childcare ~ index + is_covid_era + quarter_ex_year,
  data = SSC_by_quarter,
  corr = nlme::corAR1(form = ~ index))
tmp <- summary(model_secondary_childcare)
tmp$tTable %>% 
  as_tibble() %>% 
  mutate(Coef = names(tmp$coefficients)) %>% 
  slice(-1) %>% 
  select(Coef, Beta = Value, Std.Error, `p-value`)
rm(tmp)


# plot it
y_min <- 4.25*60
y_max <- 5.75*60
SSC_by_quarter %>% 
  ggplot(aes(x = quarter, y = weighted_secondary_childcare)) +
  geom_rect(aes(xmin = as.Date('2020-01-01'),
                xmax = as.Date('2020-09-30'),
                ymin = y_min,
                ymax = y_max),
            fill = 'grey90') +
  geom_line(data = filter(SSC_by_quarter, quarter %in% as.Date(c('2019-12-31', '2020-09-30'))),
            color = 'grey50', linetype = 'dashed') +
  geom_line(aes(color = is_covid_era), alpha = 0.3) +
  geom_point(aes(color = is_covid_era), alpha = 0.3) +
  geom_line(data = tibble(x = SSC_by_quarter$quarter, 
                          y = model_secondary_childcare$fitted,
                          group = SSC_by_quarter$is_covid_era),
            aes(x = x, y = y, group = group)) + 
  annotate('text', x = as.Date('2020-06-30'), y = 275, label = 'No data collection', 
           color = 'grey30', angle = -90, size = 4) +
  ggplot2::scale_color_discrete() +
  scale_x_date(date_breaks = '1 year', date_labels = "'%y") +
  scale_y_continuous(limits = c(y_min, y_max),
                     breaks = seq(y_min, y_max, by = 15),
                     labels = format_hour_minute) +
  labs(title = 'Mean daily time spent on secondary childcare for household children under 13',
       subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
                         scales::comma_format()(nrow(respondents_with_children))),
       x = NULL,
       y = 'Hour:minutes on secondary childcare',
       color = NULL) +
  theme(legend.position = 'bottom')
# ggsave(file.path('analyses', 'supporting', 'plots', "childcare_secondary_timeseries.png"),
#        height = 6, width = 10)


# repeat but split by sex -------------------------------------------------

# time series by quarter
# add index for regression
SSC_by_quarter_sex <- respondents_with_children %>% 
  group_by(quarter, sex, is_covid_era) %>% 
  summarize(mean_secondary_childcare = mean(secondary_childcare, na.rm = TRUE),
            weighted_secondary_childcare = sum(survey_weight * secondary_childcare) / sum(survey_weight),
            .groups = 'drop') %>% 
  arrange(quarter) %>% 
  mutate(index = round(as.numeric(((quarter - min(quarter)) / 92) + 1)),
         is_covid_era = if_else(is_covid_era, 'Covid era', 'Pre Covid'),
         is_covid_era = factor(is_covid_era, levels = c('Pre Covid', 'Covid era')),
         quarter_ex_year = str_sub(quarter, 6)) %>% 
  rowwise() %>% 
  mutate(quarter_ex_year = which(quarter_ex_year == c('03-31', '06-30', '09-30', '12-31'))) %>% 
  ungroup()

# fit generalized least squares with dummy for quarter
model_secondary_childcare_sex <- nlme::gls(
  weighted_secondary_childcare ~ index + is_covid_era + quarter_ex_year + sex,
  data = SSC_by_quarter_sex,
  corr = nlme::corAR1(form = ~ index | sex))
tmp <- summary(model_secondary_childcare_sex)
tmp$tTable %>% 
  as_tibble() %>% 
  mutate(Coef = names(tmp$coefficients)) %>% 
  slice(-1) %>% 
  select(Coef, Beta = Value, Std.Error, `p-value`)
rm(tmp)

# plot it
y_min <- 3*60
y_max <- 7*60
SSC_by_quarter_sex %>% 
  ggplot(aes(x = quarter, y = weighted_secondary_childcare)) +
  geom_rect(aes(xmin = as.Date('2020-01-01'),
                xmax = as.Date('2020-09-30'),
                ymin = y_min,
                ymax = y_max),
            fill = 'grey90') +
  geom_line(data = filter(SSC_by_quarter_sex, quarter %in% as.Date(c('2019-12-31', '2020-09-30'))),
            color = 'grey50', linetype = 'dashed') +
  geom_line(aes(color = is_covid_era), alpha = 0.3) +
  geom_point(aes(color = is_covid_era),  alpha = 0.3) +
  geom_line(data = tibble(x = SSC_by_quarter_sex$quarter, 
                          y = model_secondary_childcare_sex$fitted,
                          group = SSC_by_quarter_sex$is_covid_era,
                          sex = SSC_by_quarter_sex$sex),
            aes(x = x, y = y, group = group)) + 
  geom_text(data = tibble(x = c(as.Date('2020-06-01'), NA),
                          y = c(260, NA),
                          label = c('No data collection', ''),
                          sex = c('Female', 'Male')),
            aes(x = x, y = y, label = label),
            color = 'grey30', angle = -90, size = 4) +
  ggplot2::scale_color_discrete() +
  scale_x_date(date_breaks = '2 years', date_labels = "'%y") +
  scale_y_continuous(limits = c(y_min, y_max),
                     breaks = seq(y_min, y_max, by = 30),
                     labels = format_hour_minute) +
  facet_wrap(~sex, ncol = 2) +
  labs(title = 'Mean daily time spent on secondary childcare for household children under 13',
       subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
                         scales::comma_format()(nrow(respondents_with_children))),
       x = NULL,
       y = 'Hour:minutes on secondary childcare',
       color = NULL) +
  theme(legend.position = 'bottom')
# ggsave(file.path('analyses', 'supporting', 'plots', "childcare_secondary_timeseries_by_sex.png"),
#        height = 6, width = 10)

# demographics
respondents_with_children %>% 
  filter(is_covid_era) %>% 
  select(ID, age, sex, n_child, labor_force_status, partner_working, has_partner, 
         elder_in_HH, race, education, metropolitan, minutes = secondary_childcare) %>% 
  # left_join(childcare_summary, by = "ID") %>% 
  mutate(age = cut(age, seq(0, 100, by = 20)),
         across(-minutes, as.character)) %>% 
  select(-ID) %>% 
  pivot_longer(-minutes) %>% 
  group_by(name, value) %>% 
  summarize(minutes = mean(minutes, na.rm = TRUE)) %>% 
  ggplot(aes(x = value, y = minutes)) +
  geom_col() +
  scale_y_continuous(labels = format_hour_minute) +
  facet_wrap(~name, scales = 'free_x') +
  labs(title = 'Marginal distributions of daily time spent on secondary childcare (2020)',
       subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
                         scales::comma_format()(nrow(respondents_with_children))),
       x = NULL,
       y = 'Mean daily time (unweighted)') +
  theme(axis.text.x = element_text(angle = -40, hjust = 0))
# ggsave(file.path('analyses', 'supporting', 'plots', "childcare_secondary_marginals.png"),
#        height = 8, width = 10)



# previous work -----------------------------------------------------------

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
