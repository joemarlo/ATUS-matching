library(tidyverse)
source(file.path('analyses', 'plots', 'ggplot_settings.R'))
source(file.path('data', 'helpers.R'))
source(file.path("analyses", "helpers_analyses.R"))

# add metropolitan status 
# based on if respondent is in an MSA
# GTMETSTA is newer than GEMETSTA (MSA definitions updated in 2004)
atussum_0320 <- atussum_0320 %>% 
  mutate(metropolitan = if_else(
    GEMETSTA > 0,
    case_when(
      GEMETSTA == 1 ~ 'metropolitan',
      GEMETSTA == 2 ~ 'non-metropolitan',
      TRUE ~ 'NA'
      ),
    case_when(
      GTMETSTA == 1 ~ 'metropolitan',
      GTMETSTA == 2 ~ 'non-metropolitan',
      TRUE ~ 'NA'
    )))
atussum_0320$metropolitan[atussum_0320$metropolitan == 'NA'] <- NA

    
# summary stats -----------------------------------------------------------

# summary table of activities by sex
get_min_per_part(df = atussum_0320, groups = c('TESEX'), simplify = descriptions) %>% 
  # match based on regex code in curated.codes
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female'),
         weighted.hours = round(weighted.minutes / 60, 2),
         participation.rate = round(participation.rate, 4),
         hours.per.participant = round(minutes.per.participant/ 60, 2)) %>% 
  select(-weighted.minutes, -minutes.per.participant ) %>% 
  View('2003-2018 summary')

# summary table of activities by sex and age
all_activities_by_age_sex <- atussum_0320 %>% 
  get_min_per_part(groups = c('TESEX', 'TEAGE'), simplify = descriptions) %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female'),
         weighted.hours = round(weighted.minutes / 60, 2),
         participation.rate = round(participation.rate, 4),
         hours.per.participant = round(minutes.per.participant/ 60, 2))
all_activities_by_age_sex %>% 
  ggplot(aes(x = TEAGE, y = weighted.hours, fill = activity)) +
  geom_area(color = 'white') +
  scale_y_continuous(breaks = seq(0, 24, by = 2)) +
  facet_wrap(~TESEX) +
  labs(title = 'Average hours spent in activity per day',
       caption = "2003-2018 American Time Use Survey",
       x = 'Age',
       y = 'Hours',
       fill = NULL)
# ggsave(file.path('analyses', 'supporting', 'plots', "all_activites_by_age_sex.png"), 
#        height = 6, width = 10)


# socializing -------------------------------------------------------------

socializing_codes <- c('120101', '120199', '120201', '120202', 
                       '120299', '120501', '120502', '120599', 
                       '129999', '181201', '181202', '181299')
socializing_codes <- paste0('t', socializing_codes)

# plot of socializing activities
atussum_0320 %>% 
  select(TUFNWGTP, TUCASEID, any_of(socializing_codes), TESEX, TEAGE) %>%
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
  labs(title = 'Daily time spent on socializing activities',
       caption = "2003-2018 American Time Use Survey",
       x = 'Age',
       y = NULL,
       color = NULL) +
  theme(legend.position = 'bottom')


# childcare ---------------------------------------------------------------

# TODO: review!!
childcare_codes <- specific.codes %>% 
  filter(str_detect(Description, "child")) %>% 
  mutate(code = paste0("t", Code)) %>% 
  pull(code)
# specific.codes %>% mutate(Code = paste0("t", Code)) %>% inner_join(tibble(Code = childcare_codes)) %>% View
  
# plot of childcare by age and sex
atussum_0320 %>% 
  select(TUFNWGTP, TUCASEID, any_of(childcare_codes), TESEX, TEAGE) %>%
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
  labs(title = 'Daily time spent on child-related activities',
       subtitle = 'Includes household and non-household children',
       caption = "2003-2018 American Time Use Survey",
       x = 'Age',
       y = NULL)


# home schooling ----------------------------------------------------------

home_school_hh_child <- 't030203'
get_min_per_part(df = atussum_0320, activities = home_school_hh_child,
                 groups = c('TESEX', 'TUYEAR'), simplify = TRUE) %>% 
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  pivot_longer(cols = 4:6) %>%
  mutate(name = recode(name, 
                       weighted.minutes = 'Minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Minutes per participant')) %>%
  ggplot(aes(x = TUYEAR, y = value, group = TESEX, color = TESEX)) +
  geom_line() +
  geom_point() +
  facet_wrap(~name, nrow = 1, scales = 'free_y') +
  labs(title = 'Daily time spent on household children homeschooling',
       caption = "2003-2018 American Time Use Survey",
       x = 'Year',
       y = NULL,
       color = NULL)

# amount of historical respondents that participate in home schooling has been around ~10-20 annually
atussum_0320 %>% 
  select(TUYEAR, TESEX, t030203) %>% 
  group_by(TUYEAR, TESEX) %>% 
  summarize(n_participants = sum(t030203 > 0)) %>% 
  View



# household activities ----------------------------------------------------

# codes that are household activities
# TODO: REVIEW: may want to trim down
house.codes <- descriptions %>% 
  filter(description == 'Household Activities') %>%
  pull(activity)
specific.codes %>% mutate(Code = paste0("t", Code)) %>% inner_join(tibble(Code = house.codes)) %>% View

housework_by_age_sex_year <- atussum_0320 %>% 
  select(TUFNWGTP, TUCASEID, house.codes, TESEX, TUYEAR, TEAGE) %>% 
  filter(TEAGE > 20) %>% 
  mutate(TEAGE = cut(TEAGE, breaks = c(20, 30, 50, 65, 100))) %>% 
  get_min_per_part(groups = c('TESEX', 'TUYEAR', 'TEAGE'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  pivot_longer(cols = 5:7) %>%
  mutate(name = recode(name, 
                       weighted.minutes = 'Average minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Average minutes per participant'),
         TEAGE = recode(TEAGE,
                        `(20,30]` = 'Age 20-29',
                        `(30,50]` = '30-49',
                        `(50,65]` = '50-64',
                        `(65,100]` = '65+'),
         grouping = paste0(TESEX, '-', TEAGE))

housework_by_age_sex_year %>% 
  filter(name != 'Average minutes') %>% 
  ggplot(aes(x = TUYEAR, y = value, group = grouping, color = as.factor(TESEX))) +
  # geom_point(alpha = 0.15) +
  geom_smooth(method = lm,
              formula = y ~ x,
              se = TRUE,
              aes(linetype = TEAGE),
              alpha = 0.1) +
  ggplot2::scale_color_discrete() +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted")) +
  facet_wrap(~name, nrow = 1, scales = 'free_y') + 
  labs(title = '[DRAFT] Time spent in household activities per day',
       subtitle = 'Includes activities such as cleaning, laundry, cooking, etc.',
       # subtitle = 'STANDARD ERRORS ARE PROBABLY MISESTIMATED',
       caption = "2003-2018 American Time Use Survey",
       x = NULL,
       y = NULL,
       color = NULL,
       linetype = NULL) +
  guides(linetype = guide_legend(override.aes = list(size = 0.7, color = 'grey10'))) +
  theme(legend.position = 'bottom',
        legend.key.width = unit(1.3, "cm"))
# ggsave(file.path('analyses', 'supporting', 'plots', "housework_by_age_sex.png"),
#        height = 6, width = 10)

housework_by_age_sex_year_met <- atussum_0320 %>% 
  select(TUFNWGTP, TUCASEID, house.codes, TESEX, TUYEAR, TEAGE, metropolitan) %>% 
  filter(TEAGE > 20) %>% 
  mutate(TEAGE = cut(TEAGE, breaks = c(20, 30, 50, 65, 100))) %>% 
  get_min_per_part(groups = c('TESEX', 'TUYEAR', 'TEAGE', 'metropolitan'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  pivot_longer(cols = 6:8) %>%
  mutate(name = recode(name, 
                       weighted.minutes = 'Average minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Average minutes per participant'),
         TEAGE = recode(TEAGE,
                        `(20,30]` = 'Age 20-29',
                        `(30,50]` = '30-49',
                        `(50,65]` = '50-64',
                        `(65,100]` = '65+'),
         grouping = paste0(TESEX, '-', TEAGE))

housework_by_age_sex_year_met %>% 
  filter(name != 'Average minutes') %>% 
  na.omit() %>% 
  ggplot(aes(x = TUYEAR, y = value, group = grouping, color = as.factor(TESEX))) +
  # geom_line() +
  geom_smooth(method = lm,
              formula = y ~ x,
              se = TRUE,
              aes(linetype = TEAGE),
              alpha = 0.1) +
  ggplot2::scale_color_discrete() +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted")) +
  facet_grid(name~metropolitan, scales = 'free_y') + 
  labs(title = '[DRAFT] Time spent in household activities per day',
       subtitle = 'Includes activities such as cleaning, laundry, cooking, etc.',
       # subtitle = 'STANDARD ERRORS ARE PROBABLY MISESTIMATED',
       caption = "2003-2018 American Time Use Survey",
       x = NULL,
       y = NULL,
       color = NULL,
       linetype = NULL) +
  guides(linetype = guide_legend(override.aes = list(size = 0.7, color = 'grey10'))) +
  theme(legend.position = 'bottom',
        legend.key.width = unit(1.3, "cm"))
# ggsave(file.path('analyses', 'supporting', 'plots', "housework_by_age_sex_metropolitan.png"),
#        height = 8, width = 10)


# cooking -----------------------------------------------------------------

cook.codes <- c('t020201', 't020202', 't020203', 't020299')
grocery.codes <- c('t070101', 't180701')
specific.codes %>% mutate(Code = paste0("t", Code)) %>% inner_join(tibble(Code = c(cook.codes, grocery.codes))) %>% View

# cooking by sex and generation
atussum_0320 %>% 
  select(TUFNWGTP, TUCASEID, cook.codes, grocery.codes, TESEX, TUYEAR, TEAGE) %>% 
  filter(TEAGE > 20) %>% 
  mutate(TEAGE = cut(TEAGE, breaks = c(20, 40, 60, 100))) %>% 
  get_min_per_part(groups = c('TESEX', 'TUYEAR', 'TEAGE'), simplify = TRUE) %>%
  mutate(TESEX = recode(as.character(TESEX), '1' = 'Male', '2' = 'Female')) %>% 
  pivot_longer(cols = 5:7) %>%
  mutate(name = recode(name, 
                       weighted.minutes = 'Minutes',
                       participation.rate = 'Participation rate',
                       minutes.per.participant = 'Minutes per participant'),
         TEAGE = recode(TEAGE,
                        `(20,40]` = 'Age 20-40',
                        `(40,60]` = '40-60',
                        `(60,100]` = '60+'),
         grouping = paste0(TESEX, '-', TEAGE)) %>%
  ggplot(aes(x = TUYEAR, y = value, group = grouping, color = as.factor(TESEX))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm,
              formula = y ~ x,
              se = TRUE,
              aes(linetype = TEAGE),
              alpha = 0.2) +
  ggplot2::scale_color_discrete() +
  facet_wrap(~name, nrow = 1, scales = 'free_y') + 
  labs(title = 'Time spent in cooking and grocery activities per day',
       subtitle = 'STANDARD ERRORS ARE PROBABLY UNDERESTIMATED',
       caption = "2003-2018 American Time Use Survey",
       x = NULL,
       y = NULL,
       color = NULL,
       linetype = NULL) +
  theme(legend.position = 'bottom')


# cooking by age and sex
atussum_0320 %>% 
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
atussum_0320 %>% 
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
get_minutes(atussum_0320, 'TESEX', simplify = descriptions)
get_minutes(atussum_0320, 'TESEX', simplify = descriptions, )
get_participation(atussum_0320, 'TESEX', simplify = descriptions)
get_min_per_part(atussum_0320, 'TESEX', simplify = descriptions)

# specific activities
game.codes <- colnames(atussum_0320)[colnames(atussum_0320) %in% paste0('t', 130101:130199)]
grouping <- enframe(game.codes) %>%
  mutate(description = c(rep('yes', 30), rep('no', 7))) %>%
  select(activity = value, description)
get_min_per_part(atussum_0320, 'TESEX', game.codes, simplify = grouping)

# tv by age and sex with 95% confidence interval
get_minutes(df = atussum_0320, groups = c('TEAGE', 'TESEX'), 
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
