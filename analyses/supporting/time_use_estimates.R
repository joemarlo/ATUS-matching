library(tidyverse)
source(file.path('analyses', 'plots', 'ggplot_settings.R'))
source(file.path('data', 'helpers.R'))
source(file.path("analyses", "helpers_analyses.R"))


# summary table of activities by sex
get_min_per_part(df = atussum_0318, groups = c('TESEX'), simplify = descriptions) %>% 
  # match based on regex code in curated.codes
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female'),
         weighted.hours = round(weighted.minutes / 60, 2),
         participation.rate = round(participation.rate, 4),
         hours.per.participant = round(minutes.per.participant/ 60, 2)) %>% 
  select(-weighted.minutes, -minutes.per.participant ) %>% 
  View('2003-2018 summary')



# homeschooling -----------------------------------------------------------





# housework, cooking & groceries  --------------------------------------------------------

# codes that are "housework"
# TODO: REVIEW
house.codes <- descriptions %>% 
  filter(description == 'Household Activities') %>%
  pull(activity)
# specific.codes %>% mutate(Code = paste0("t", Code)) %>% inner_join(tibble(Code = house.codes)) %>% View

atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, house.codes, TESEX, TUYEAR, TEAGE) %>% 
  filter(TEAGE > 20) %>% 
  mutate(TEAGE = cut(TEAGE, breaks = c(20, 40, 60, 100))) %>% 
  get_min_per_part(groups = c('TESEX', 'TUYEAR', 'TEAGE'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  select(-minutes.per.participant) %>% 
  pivot_longer(cols = 5:6) %>%
  mutate(name = recode(name, 
                       weighted.minutes = 'Minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Minutes per participant'),
         TEAGE = recode(TEAGE,
                        `(20,40]` = 'Age 20-40',
                        `(40,60]` = '40-60',
                        `(60,100]` = '60+'),
         grouping = paste0(TESEX, '-', TEAGE)) %>%
  ggplot(aes(x = TUYEAR, y = value, group = grouping, color = as.factor(TESEX), )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = loess,
              se = FALSE,
              aes(linetype = TEAGE)) +
  facet_wrap(~name, ncol = 2, scales = 'free_y') + 
  labs(title = 'Time spent in household activities per day',
       caption = "2003-2018 American Time Use Survey",
       x = NULL,
       y = NULL)


# cooking -----------------------------------------------------------------

cook.codes <- c('t020201', 't020202', 't020203', 't020299')
grocery.codes <- c('t070101', 't180701')

# cooking by sex and generation
atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, cook.codes, grocery.codes, TESEX, TUYEAR, TEAGE) %>% 
  filter(TEAGE > 20) %>% 
  mutate(TEAGE = cut(TEAGE, breaks = c(20, 40, 60, 100))) %>% 
  get_min_per_part(groups = c('TESEX', 'TUYEAR', 'TEAGE'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  select(-minutes.per.participant) %>% 
  pivot_longer(cols = 5:6) %>%
  mutate(name = recode(name, 
                       weighted.minutes = 'Minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Minutes per participant'),
         TEAGE = recode(TEAGE,
                        `(20,40]` = 'Age 20-40',
                        `(40,60]` = '40-60',
                        `(60,100]` = '60+'),
         grouping = paste0(TESEX, '-', TEAGE)) %>%
  ggplot(aes(x = TUYEAR, y = value, group = grouping, color = as.factor(TESEX), )) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = loess,
              se = FALSE,
              aes(linetype = TEAGE)) +
  facet_wrap(~name, ncol = 2, scales = 'free_y') + 
  labs(title = 'Time spent in cooking and grocery activities per day',
       caption = "2003-2018 American Time Use Survey",
       x = NULL,
       y = NULL)


# cooking by age and sex
atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, cook.codes, TESEX, TEAGE) %>% 
  get_min_per_part(groups = c('TESEX', 'TEAGE'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  pivot_longer(cols = 4:6) %>% 
  mutate(name = recode(name, 
                       weighted.minutes = 'Minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Minutes per participant')) %>% 
  ggplot(aes(x = TEAGE, y = value, group = TESEX, color = as.factor(TESEX))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = loess,
              se = FALSE,
              linetype = 'dashed') +
  facet_wrap(~name, ncol = 3, scales = 'free_y') +
  labs(title = 'Daily time spent cooking',
       caption = "2003-2018 American Time Use Survey",
       x = 'Age',
       y = NULL)

# groceries by age and sex
atussum_0318 %>% 
  select(TUFNWGTP, TUCASEID, grocery.codes, TESEX, TEAGE) %>% 
  get_min_per_part(groups = c('TESEX', 'TEAGE'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  pivot_longer(cols = 4:6) %>%
  mutate(name = recode(name, 
                       weighted.minutes = 'Minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Minutes per participant')) %>% 
  ggplot(aes(x = TEAGE, y = value, group = TESEX, color = as.factor(TESEX))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = loess,
              se = FALSE,
              linetype = 'dashed') +
  facet_wrap(~name, ncol = 3, scales = 'free_y') +
  labs(title = 'Daily time spent grocery shopping',
       caption = "2003-2018 American Time Use Survey",
       x = 'Age',
       y = NULL)



# examples ----------------------------------------------------------------

# all activities grouped by sex
get_minutes(atussum_0318, 'TESEX', simplify = descriptions)
get_minutes(atussum_0318, 'TESEX', simplify = descriptions, )
get_participation(atussum_0318, 'TESEX', simplify = descriptions)
get_min_per_part(atussum_0318, 'TESEX', simplify = descriptions)

# specific activities
game.codes <- colnames(atussum_0318)[colnames(atussum_0318) %in% paste0('t', 130101:130199)]
grouping <- enframe(game.codes) %>%
  mutate(description = c(rep('yes', 30), rep('no', 7))) %>%
  select(activity = value, description)
get_min_per_part(atussum_0318, 'TESEX', game.codes, simplify = grouping)

# tv by age and sex with 95% confidence interval
get_minutes(df = atussum_0318, groups = c('TEAGE', 'TESEX'), 
            activities = c('t120303', 't120304'), include_SE = TRUE) %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  ggplot(aes(x = TEAGE, y = weighted.minutes, group = TESEX)) +
  geom_line(aes(color = TESEX),
            size = 1.5) +
  geom_ribbon(aes(ymin = weighted.minutes - (1.96*SE), 
                  ymax = weighted.minutes + (1.96*SE),
                  fill = TESEX),
              alpha = 0.2) +
  scale_y_continuous(label = function(x) sprintf("%2d:%02d", as.integer(x %/% 60), as.integer(x %% 60))) +
  labs(title = "Average time watching television by age",
       subtitle = 'Range represents 95% confidence interval',
       x = "Age",
       y = 'Hours:minutes',
       caption = "2003-2018 American Time Use Survey")
