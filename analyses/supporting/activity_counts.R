library(tidyverse)
source('analyses/plots/ggplot_settings.R')

# read in ATUS data
atus_raw <- read_tsv("data/atus_30min.tsv")

# read in the matches
demographics_t1 <- read_csv(file = 'data/matched_time1_mahalanobis.csv')
demographics_t2 <- read_csv(file = 'data/matched_time2_mahalanobis.csv')

# remove duplicates -- probably should not remove duplicates when making density plots
# demographics_t2 <- distinct(demographics_t2_raw, across(-pair_id))

# create time1 and time2 dataframes for sequences
atus_t1 <- atus_raw[atus_raw$ID %in% demographics_t1$ID,]
atus_t2 <- atus_raw[atus_raw$ID %in% demographics_t2$ID,]

# list of activities to summarize
# TODO: this is aggregated; should we really be looking at specific activities
activities <- c("Caring For Household Member", "Household Activities")

# tally activities by ID
key_activities <- atus_t1 %>% 
  bind_rows(atus_t2) %>%
  filter(description %in% activities) %>% 
  group_by(ID) %>% 
  tally() %>% 
  mutate(minutes = n * 30) %>% 
  select(-n)

# plot densities of activities by year and sex
demographics_t1 %>% 
  bind_rows(demographics_t2) %>% 
  select(ID, year, sex) %>% 
  left_join(key_activities, by = 'ID') %>% 
  replace_na(list(minutes = 0)) %>% 
  group_by(sex, year) %>% 
  mutate(mean = mean(minutes),
         median = median(minutes),
         year = as.character(year)) %>% 
  ggplot(aes(x = minutes, color = year)) +
  geom_density() +
  geom_vline(aes(xintercept = mean, color = year), linetype = 'dashed') +
  scale_x_continuous(breaks = seq(0, 1500, by = 100)) +
  facet_wrap(~sex, ncol = 1) +
  labs(title = 'Minutes per day spent doing XYZ activities',
       x = "Minutes per day",
       y = NULL,
       color = NULL) +
  theme(legend.position = 'bottom')


# specific activities -----------------------------------------------------

# read in detailed activity data
atus_activities <- read_csv(file.path("inputs", "ATUS-2003-2018", "atusact_0318.dat"))

# example acitivities on childcare
# TODO: review; unsure if comprehensible or measuring what we think it is
'TRTOHH_LN' #Total time spent during activity providing secondary childcare for own household children < 13 (in minutes)
'TRTO_LN' #Total time spent during activity providing secondary childcare for own children < 13 (in minutes)
'TRTNOHH_LN' #Total time spent during activity providing secondary childcare for nonown household children < 13 (in minutes)
'TRTHH_LN' #Total time spent during activity providing secondary childcare for household children < 13 (in minutes)
'TRTHH' #Total time spent during diary day providing secondary childcare for household children < 13 (in minutes)

atus_activities %>% 
  select(ID = TUCASEID, TRTHH_LN) %>% 
  mutate(TRTHH_LN = ifelse(TRTHH_LN == -1, NA, TRTHH_LN)) %>% 
  right_join(demographics_t1 %>% 
               bind_rows(demographics_t2) %>% 
               select(ID, year, sex),
             by = 'ID') %>% 
  group_by(sex, year) %>% 
  mutate(mean = mean(TRTHH_LN, na.rm = TRUE),
         median = median(TRTHH_LN, na.rm = TRUE),
         year = as.character(year)) %>% 
  ggplot(aes(x = TRTHH_LN, color = year)) +
  geom_density() +
  geom_vline(aes(xintercept = mean, color = year), linetype = 'dashed') +
  scale_x_log10() +
  facet_wrap(~sex, ncol = 1) +
  labs(title = 'Minutes per day spent doing XYZ activities',
       x = "Minutes per day",
       y = NULL,
       color = NULL) +
  theme(legend.position = 'bottom')
