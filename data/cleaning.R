source('data/helpers.R')
library(lubridate)
options(mc.cores = parallel::detectCores())

# # remove unnecessary data from environment
# # memory management is important as datasets are large
# rm(atusrost_0318, atuswgts_0318)
# gc()


# cut down atus activity file ---------------------------------------------

# create dataframe of start and end time for each activity for each respondent
ATUS <- atusact_0318 %>%
  select(TUCASEID, TUSTARTTIM, TUSTOPTIME, TRCODEP) %>% 
  mutate(activity = paste0("t", TRCODEP)) %>% 
  fuzzyjoin::regex_left_join(y = curated.codes, by = 'activity') %>%
  select(TUCASEID, TUSTARTTIM, TUSTOPTIME, activity = activity.x, description)

baseline_time <- function(x_minutes){
  # function baselines time from 4am -> 12am
  ret <- x_minutes - (4*60)
  
  ret <- sapply(ret, function(x) {
    if (x < 0){
      (24*60) + x
    } else x
  })
  return(ret)
}

# convert the time to minutes where 0 = 4am
# split the data into individual dataframes for each respondent
split_ATUS <- ATUS %>% 
  mutate(start_time = (hour(TUSTARTTIM)*60) + minute(TUSTARTTIM),
         end_time = (hour(TUSTOPTIME)*60) + minute(TUSTOPTIME),
         start_time = baseline_time(start_time),
         end_time = baseline_time(end_time),
         # this cuts off things at 4am
         end_time = if_else(end_time < start_time, 1440, end_time)) %>% 
  select(TUCASEID, start_time, end_time, description) %>% 
  group_split(TUCASEID)

# throw out 14 respondents who's responses don't capture the whole day
whole_day <- parallel::mclapply(split_ATUS, FUN = function(tbl){
 max(tbl$end_time)
}) %>% unlist() == 1440
split_ATUS <- split_ATUS[whole_day]
rm(whole_day)

# for each respondent, expand the dataframe into increments of 5 minutes
# takes about 20min on 4-core i5 desktop
# resulting df is 200mm rows so make sure you have >30gb of memory
ATUS_1 <- parallel::mclapply(split_ATUS, FUN = function(tbl) {
  # pivot each table so there is a time column with each row representing
  #   a period of 1 minute
  stretched_tbl <-
    pmap_dfr(
      .l = list(tbl$TUCASEID, tbl$start_time, tbl$end_time, tbl$description),
      .f = function(ID, start, end, desc) {
        tibble(
          ID = ID,
          time = seq(from = start, to = (end - 1), by = 1),
          description = desc
        )
      }
    )
  
  # add NAs for missing times
  final_tbl <- stretched_tbl %>%
    right_join(y = tibble(time = seq(0, 1439, by = 1)), by = 'time') %>%
    ungroup()
  
  return(final_tbl)
}) %>% bind_rows()

Mode <- function(x) {
  # function calculates mode
  unique_x <- unique(na.omit(x))
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

# now collapse down to 30min chunks by taking the mode per each chunk
ATUS_30 <- ATUS_1 %>% 
  group_by(ID) %>% 
  mutate(period = floor(time / 30) + 1) %>% 
  group_by(ID, period) %>% 
  summarize(description = Mode(description),
            .groups = 'drop')



# pull demographics information -------------------------------------------

# from CPS, check if there is an elder in the household
# elder = parent or other relative who is >=1 year older than respondent
elder_in_HH <- atusrost_0318 %>% 
  group_by(TUCASEID) %>% 
  arrange(TUCASEID, TULINENO) %>%
  mutate(resp_age = first(TEAGE),
         is_elder = TERRP %in% c(24, 26) & TEAGE >= (resp_age + 1)) %>%
  summarize(elder_in_HH = any(is_elder),
            .groups = 'drop') %>% 
  rename(ID = TUCASEID)

# from CPS, check if person has partner (is married or is living with partner)
has_partner <- atuscps_0318 %>%
  group_by(TUCASEID) %>% 
  summarize(has_partner = any(PERRP %in% c(3, 13, 14)),
            .groups = 'drop') %>% 
  rename(ID = TUCASEID)

# from CPS data, get race, marriage status, education, metropolitan status, and state 
# TODO: figure out how to get NAICS code and match to essential_industries 
# data dictionary here: https://www2.census.gov/programs-surveys/cps/datasets/2021/basic/2021_Basic_CPS_Public_Use_Record_Layout_plus_IO_Code_list.txt
CPS_vars <- atuscps_0318 %>%
  filter(TULINENO == 1) %>%   # filter so only person responding to ATUS is included
  select(TUCASEID, PTDTRACE, PEEDUCA, GESTFIPS, GTMETSTA) %>%
  mutate(
    race = case_when(
      PTDTRACE == 1 ~ 'white',
      PTDTRACE == 2 ~ 'black',
      PTDTRACE == 4 ~ 'asian',
      TRUE ~ 'other'
    ),
    education = case_when(
      PEEDUCA < 38 ~ "Did not graduate from HS",
      PEEDUCA == 39 ~ "HS",
      PEEDUCA %in% 40:42 ~ "Some college",
      PEEDUCA == 43 ~ "Bachelors",
      PEEDUCA == 44 ~ "Masters",
      PEEDUCA %in% 44:45 ~ "Doctoral"
    ),
    metropolitan = case_when(
      GTMETSTA == 1 ~ 'metropolitan',
      GTMETSTA == 2 ~ 'non-metropolitan',
      GTMETSTA %in% c(-1, 3) ~ 'NA',
    )
  ) %>% 
  left_join(FIPS[, c('Name', 'FIPS')], by = c(GESTFIPS = 'FIPS')) %>%
  select(ID = TUCASEID, race, education, state = Name, metropolitan) %>% 
  distinct()

# from ATUS data, get weights, age, sex, children, income,  
atus_vars <- atussum_0318 %>% 
  select(TUCASEID, survey_weight = TUFNWGTP, age = TEAGE,
         sex = TESEX, age_youngest = TRYHHCHILD, n_child = TRCHILDNUM,
         labor_force_status = TELFS, partner_working = TESPEMPNOT) %>%
  mutate(age_youngest = ifelse(age_youngest == -1, NA, age_youngest),
         labor_force_status = case_when(
           labor_force_status == 1 ~ 'employed - at work',
           labor_force_status == 2 ~ 'employed - absent',
           labor_force_status == 3 ~ 'unemployed - on layoff',
           labor_force_status == 4 ~ 'unemployed - looking',
           labor_force_status == 5 ~ 'not in labor force',
           TRUE ~ 'NA'
         ),
         partner_working = case_when(
           partner_working == -1 ~ 'NA',
           partner_working == 1 ~ 'employed',
           partner_working == 2 ~ 'not employed'
         )) %>%
  left_join(distinct(atuscps_0318[, c('TUCASEID', 'HEFAMINC', 'HUFAMINC')]),
            by = 'TUCASEID') %>%
  left_join(select(income.levels, HUFAMINC, HH.income.new), by = 'HUFAMINC') %>%
  left_join(select(income.levels, HEFAMINC, HH.income.new), by = 'HEFAMINC') %>% 
  mutate(HH.income = pmax(HH.income.new.x, HH.income.new.y, na.rm = TRUE)) %>% 
  select(-c('HEFAMINC', 'HUFAMINC', 'HH.income.new.x', 'HH.income.new.y')) %>% 
  rename(ID = TUCASEID,
         fam_income = HH.income) %>% 
  distinct()

# final dataset of demographic variables
demographic_vars <- atus_vars %>% 
  left_join(elder_in_HH, by = 'ID') %>%
  left_join(has_partner, by = 'ID') %>% 
  left_join(CPS_vars, by = 'ID') %>% 
  semi_join(distinct(ATUS_30, ID), by = 'ID') %>% 
  left_join(atusresp_0318 %>% 
              select(ID = TUCASEID, day_of_week = TUDIARYDAY, holiday = TRHOLIDAY,
                     year = TUYEAR, TRTALONE, TRTALONE_WK, TESCHFT),
            by = 'ID')


# write out the final datasets --------------------------------------------
# write_tsv(ATUS_1, 'data/atus.tsv')
# write_tsv(ATUS_30, 'data/atus_30min.tsv')
# write_tsv(demographic_vars, 'data/demographic.tsv')
