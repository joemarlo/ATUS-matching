###
# This script performs the time-series modeling 
###


library(dplyr)
library(purrr)
library(tidyr)

# functions
purrr:::walk(list.files('R', full.names = TRUE), source)

# read in the demographics data
demographics <- readr::read_delim(file = file.path("data", "demographic.tsv"),
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           col_types = readr::cols(metropolitan = readr::col_character()))

# recode sex so its explicit
demographics$sex <- recode(demographics$sex,
                           '1' = 'Male',
                           '2' = 'Female')

# ATUS roster
atusrost_0320 <- readr::read_csv(file.path("inputs", "ATUS-2003-2020", "atusrost_0320.dat"))

# ATUS codes
specific.codes <- readr::read_delim("inputs/specific_codes.csv", 
                                    "+", escape_double = FALSE, trim_ws = TRUE)
simple.codes <- readr::read_delim("inputs/simple_codes.csv", 
                                  "+", escape_double = FALSE, trim_ws = TRUE)


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

# readr::write_csv(respondents_with_children,
#                  file.path('data', 'respondents_with_children.csv'))


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
  mutate(quarter_ex_year = which(quarter_ex_year == c('03-31', '06-30', '09-30', '12-31')),
         quarter_ex_year = factor(quarter_ex_year, levels = 1:4)) %>% 
  ungroup()

# fit mulitple  generalized least squares with dummy for quarter
model_primary_childcare <- nlme::gls(
  weighted_childcare ~ index + is_covid_era + quarter_ex_year,
  data = PCC_by_quarter,
  corr = nlme::corAR1(form = ~ index))
model_primary_childcare_no_seasonality <- nlme::gls(
  weighted_childcare ~ index + is_covid_era,
  data = PCC_by_quarter,
  corr = nlme::corAR1(form = ~ index))
model_primary_childcare_simple <- nlme::gls(
  weighted_childcare ~ index,
  data = PCC_by_quarter,
  corr = nlme::corAR1(form = ~ index))
model_primary_childcare_covid <- nlme::gls(
  weighted_childcare ~ index + quarter_ex_year,
  data = PCC_by_quarter,
  corr = nlme::corAR1(form = ~ index))

### model selection
anova(model_primary_childcare_simple, model_primary_childcare_covid, model_primary_childcare_no_seasonality, model_primary_childcare)

# best model
tmp <- summary(model_primary_childcare)
tmp$tTable %>% 
  as_tibble() %>% 
  mutate(Coef = names(tmp$coefficients)) %>% 
  slice(-1) %>% 
  select(Coef, Beta = Value, Std.Error, `p-value`)
rm(tmp)


# plot it
y_min <- 1.05*60
y_max <- 1.55*60
PCC_by_quarter %>% 
  ggplot(aes(x = quarter, y = weighted_childcare)) +
  geom_rect(aes(xmin = as.Date('2020-01-01'),
                xmax = as.Date('2020-09-30'),
                ymin = y_min,
                ymax = y_max),
            fill = 'grey90') +
  geom_line(data = filter(PCC_by_quarter, quarter %in% as.Date(c('2019-12-31', '2020-09-30'))),
            color = 'grey50', linetype = 'dashed') +
  geom_line(aes(color = is_covid_era), alpha = 0.3) +
  geom_point(aes(color = is_covid_era), alpha = 0.3) +
  geom_line(data = tibble(x = PCC_by_quarter$quarter, 
                          y = model_primary_childcare$fitted,
                          group = PCC_by_quarter$is_covid_era),
            aes(x = x, y = y, group = group)) + 
  annotate('text', x = as.Date('2020-06-30'), y = 72, label = 'No data collection',
           color = 'grey30', angle = -90, size = 4) +
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
# ggsave(file.path('outputs', 'time-series', "childcare_primary_timeseries.png"),
#        height = 6, width = 10)

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
  mutate(quarter_ex_year = which(quarter_ex_year == c('03-31', '06-30', '09-30', '12-31')),
         quarter_ex_year = factor(quarter_ex_year, levels = 1:4)) %>% 
  ungroup()

# fit mulitple generalized least squares with dummy for quarter
model_primary_childcare_sex <- nlme::gls(
  weighted_childcare ~ index + is_covid_era + quarter_ex_year + sex,
  data = PCC_by_quarter_sex,
  corr = nlme::corAR1(form = ~ index | sex))
model_primary_childcare_sex_no_seasonality <- nlme::gls(
  weighted_childcare ~ index + sex + is_covid_era,
  data = PCC_by_quarter_sex,
  corr = nlme::corAR1(form = ~ index | sex))
model_primary_childcare_sex_simple <- nlme::gls(
  weighted_childcare ~ index + sex,
  data = PCC_by_quarter_sex,
  corr = nlme::corAR1(form = ~ index | sex))
model_primary_childcare_sex_covid <- nlme::gls(
  weighted_childcare ~ index + sex + quarter_ex_year,
  data = PCC_by_quarter_sex,
  corr = nlme::corAR1(form = ~ index | sex))

### model selection
anova(
  model_primary_childcare_sex, 
  model_primary_childcare_sex_covid, 
  model_primary_childcare_sex_no_seasonality, 
  model_primary_childcare_sex_simple
)

# best model
tmp <- summary(model_primary_childcare_sex)
tmp$tTable %>% 
  as_tibble() %>% 
  mutate(Coef = names(tmp$coefficients)) %>% 
  slice(-1) %>% 
  select(Coef, Beta = Value, Std.Error, `p-value`)
rm(tmp)

# plot it
PCC_by_quarter_sex %>% 
  ggplot(aes(x = quarter, y = weighted_childcare)) +
  geom_line(data = filter(PCC_by_quarter_sex, quarter %in% as.Date(c('2019-12-31', '2020-09-30'))),
            color = 'grey50', linetype = 'dashed') +
  geom_line(aes(color = is_covid_era), alpha = 0.3) +
  geom_point(aes(color = is_covid_era), alpha = 0.3) +
  geom_line(data = tibble(x = PCC_by_quarter_sex$quarter,
                          y = model_primary_childcare_sex$fitted,
                          group = PCC_by_quarter_sex$is_covid_era,
                          sex = PCC_by_quarter_sex$sex),
            aes(x = x, y = y, group = group)) +
  # annotate('text', x = as.Date('2020-06-30'), y = 72, label = 'No data collection',
  #          color = 'grey30', angle = -90, size = 4) +
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
# ggsave(file.path('outputs', 'time-series', "childcare_primary_timeseries_by_sex.png"),
#        height = 6, width = 10)

# demographics
# age, sex, n_child, labor_force_status, partner_working, has_partner, elder_in_HH, race, education, metropolitan
# respondents_with_children %>% 
#   filter(is_covid_era) %>% 
#   select(ID,  age, sex, n_child, labor_force_status, partner_working, has_partner, 
#          elder_in_HH, race, education, metropolitan) %>% 
#   left_join(childcare_summary, by = "ID") %>% 
#   mutate(age = cut(age, seq(0, 100, by = 20)),
#          across(-minutes, as.character)) %>% 
#   select(-ID) %>% 
#   pivot_longer(-minutes) %>% 
#   group_by(name, value) %>% 
#   summarize(minutes = mean(minutes, na.rm = TRUE)) %>% 
#   ggplot(aes(x = value, y = minutes)) +
#   geom_col() +
#   scale_y_continuous(labels = format_hour_minute) +
#   facet_wrap(~name, scales = 'free_x') +
#   labs(title = 'Marginal distributions of daily time spent on primary childcare (2020)',
#        subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
#                          scales::comma_format()(nrow(respondents_with_children))),
#        x = NULL,
#        y = 'Mean daily time (unweighted)') +
#   theme(axis.text.x = element_text(angle = -40, hjust = 0))
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
  mutate(quarter_ex_year = which(quarter_ex_year == c('03-31', '06-30', '09-30', '12-31')),
         quarter_ex_year = factor(quarter_ex_year, levels = 1:4)) %>%  
  ungroup()

# fit mulitple generalized least squares with dummy for quarter
model_secondary_childcare <- nlme::gls(
  weighted_secondary_childcare ~ index + is_covid_era + quarter_ex_year,
  data = SSC_by_quarter,
  corr = nlme::corAR1(form = ~ index))
model_secondary_childcare_no_seasonality <- nlme::gls(
  weighted_secondary_childcare ~ index + is_covid_era,
  data = SSC_by_quarter,
  corr = nlme::corAR1(form = ~ index))
model_secondary_childcare_simple <- nlme::gls(
  weighted_secondary_childcare ~ index,
  data = SSC_by_quarter,
  corr = nlme::corAR1(form = ~ index))
model_secondary_childcare_no_covid <- nlme::gls(
  weighted_secondary_childcare ~ index + quarter_ex_year,
  data = SSC_by_quarter,
  corr = nlme::corAR1(form = ~ index))

### model selection
anova(
  model_secondary_childcare, 
  model_secondary_childcare_no_covid, 
  model_secondary_childcare_no_seasonality, 
  model_secondary_childcare_simple
)

# best model
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
# ggsave(file.path('outputs/', 'time-series', "childcare_secondary_timeseries.png"),
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
  mutate(quarter_ex_year = which(quarter_ex_year == c('03-31', '06-30', '09-30', '12-31')),
         quarter_ex_year = factor(quarter_ex_year, levels = 1:4)) %>%  
  ungroup()

# fit multiple generalized least squares with dummy for quarter
model_secondary_childcare_sex <- nlme::gls(
  weighted_secondary_childcare ~ index + is_covid_era + quarter_ex_year + sex,
  data = SSC_by_quarter_sex,
  corr = nlme::corAR1(form = ~ index | sex))
model_secondary_childcare_sex_no_seasonality <- nlme::gls(
  weighted_secondary_childcare ~ index + is_covid_era + sex,
  data = SSC_by_quarter_sex,
  corr = nlme::corAR1(form = ~ index | sex))
model_secondary_childcare_sex_simple <- nlme::gls(
  weighted_secondary_childcare ~ index + sex,
  data = SSC_by_quarter_sex,
  corr = nlme::corAR1(form = ~ index | sex))
model_secondary_childcare_sex_no_covid <- nlme::gls(
  weighted_secondary_childcare ~ index + quarter_ex_year + sex,
  data = SSC_by_quarter_sex,
  corr = nlme::corAR1(form = ~ index | sex))

# model selection
anova(
  model_secondary_childcare_sex, 
  model_secondary_childcare_sex_no_covid, 
  model_secondary_childcare_sex_no_seasonality, 
  model_secondary_childcare_sex_simple
)

# best model
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
# ggsave(file.path('outputs', 'time-series', "childcare_secondary_timeseries_by_sex.png"),
#        height = 6, width = 10)

# demographics
# respondents_with_children %>% 
#   filter(is_covid_era) %>% 
#   select(ID, age, sex, n_child, labor_force_status, partner_working, has_partner, 
#          elder_in_HH, race, education, metropolitan, minutes = secondary_childcare) %>% 
#   # left_join(childcare_summary, by = "ID") %>% 
#   mutate(age = cut(age, seq(0, 100, by = 20)),
#          across(-minutes, as.character)) %>% 
#   select(-ID) %>% 
#   pivot_longer(-minutes) %>% 
#   group_by(name, value) %>% 
#   summarize(minutes = mean(minutes, na.rm = TRUE)) %>% 
#   ggplot(aes(x = value, y = minutes)) +
#   geom_col() +
#   scale_y_continuous(labels = format_hour_minute) +
#   facet_wrap(~name, scales = 'free_x') +
#   labs(title = 'Marginal distributions of daily time spent on secondary childcare (2020)',
#        subtitle = paste0('Only includes respondents with household children under 13\nn = ', 
#                          scales::comma_format()(nrow(respondents_with_children))),
#        x = NULL,
#        y = 'Mean daily time (unweighted)') +
#   theme(axis.text.x = element_text(angle = -40, hjust = 0))
# ggsave(file.path('analyses', 'supporting', 'plots', "childcare_secondary_marginals.png"),
#        height = 8, width = 10)
