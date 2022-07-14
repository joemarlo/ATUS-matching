
# source('data/helpers.R')
# library(lubridate)
# options(mc.cores = parallel::detectCores())

# # remove unnecessary data from environment
# # memory management is important as datasets are large
# rm(atusrost_0318, atuswgts_0318)
# gc()

# this script duplicates cleaning_atus30.R but also splits it each activity into if 
# the respondent was providing secondary childcare during that time or not

clean_atus <- function(atusact_0321){
  
  # create dataframe of start and end time for each activity for each respondent
  # if any secondary child care is provided during a given activity then denote it as SSC
  ATUS <- atusact_0321 %>%
    select(TUCASEID, TUSTARTTIM, TUSTOPTIME, TRCODEP, secondary_childcare = TRTHH_LN) %>% 
    mutate(secondary_childcare = if_else(
      secondary_childcare > 0,
      'SCC',
      'No SCC'),
      activity = paste0("t", TRCODEP)) %>% 
    fuzzyjoin::regex_left_join(y = curated.codes, by = 'activity') %>%
    mutate(description = paste0(description, ' : ', secondary_childcare)) %>% 
    select(TUCASEID, TUSTARTTIM, TUSTOPTIME, activity = activity.x, description)
  
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
  
  # throw out ~16 respondents who's responses don't capture the whole day
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
      purrr::pmap_dfr(
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
      dplyr::right_join(y = tibble(time = seq(0, 1439, by = 1)), by = 'time') %>%
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
   
  
  return(list(ATUS_1 = ATUS_1, ATUS_30 = ATUS_30))
}

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

