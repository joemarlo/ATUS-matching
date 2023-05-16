
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
    select(TUCASEID, TUSTARTTIM, TUSTOPTIME, TRCODEP, secondary_childcare = TRTHH_LN, location = TEWHERE) %>% 
    mutate(secondary_childcare = if_else(
      secondary_childcare > 0,
      'SCC',
      'No SCC'),
      activity = paste0("t", TRCODEP)) %>% 
    fuzzyjoin::regex_left_join(y = curated.codes, by = 'activity') %>%
    mutate(description = paste0(description, ' : ', secondary_childcare)) %>% 
    select(TUCASEID, TUSTARTTIM, TUSTOPTIME, activity = activity.x, description, location)
  
  # convert the time to minutes where 0 = 4am
  # split the data into individual dataframes for each respondent
  split_ATUS <- ATUS %>% 
    mutate(start_time = (lubridate::hour(TUSTARTTIM)*60) + lubridate::minute(TUSTARTTIM),
           end_time = (lubridate::hour(TUSTOPTIME)*60) + lubridate::minute(TUSTOPTIME),
           start_time = baseline_time(start_time),
           end_time = baseline_time(end_time),
           # this cuts off things at 4am
           end_time = dplyr::if_else(end_time < start_time, 1440, end_time)) %>% 
    select(TUCASEID, start_time, end_time, description, location) %>% 
    group_split(TUCASEID)
  rm(ATUS)
  
  # throw out ~16 respondents who's responses don't capture the whole day
  whole_day <- parallel::mclapply(split_ATUS, FUN = function(tbl){
    max(tbl$end_time)
  }) %>% unlist() == 1440
  split_ATUS <- split_ATUS[whole_day]
  rm(whole_day)
  
  Mode <- function(x) {
    # function calculates mode
    unique_x <- unique(na.omit(x))
    unique_x[which.max(tabulate(match(x, unique_x)))]
  }
 
  # for each respondent, expand the dataframe into increments of 5 minutes
  # takes about 20min on 8-core m2 mac
  # there may be a better way to handle this: crossing?
  ATUS_30 <- parallel::mclapply(split_ATUS, FUN = function(tbl) {
    # pivot each table so there is a time column with each row representing
    #   a period of 1 minute
    stretched_tbl <- purrr::pmap_dfr(
        .l = list(tbl$TUCASEID, tbl$start_time, tbl$end_time, tbl$description, tbl$location),
        .f = function(ID, start, end, desc, location) {
          tibble(
            ID = ID,
            time = seq(from = start, to = (end - 1), by = 1),
            description = desc,
            location = location
          )
        }
      )
    
    # add NAs for missing times
    tbl_with_times <- stretched_tbl %>%
      dplyr::right_join(y = tibble(time = seq(0, 1439, by = 1)), by = 'time') %>%
      ungroup()
    
    # collapse down to 30min chunks by taking the mode per each chunk
    tbl_with_modes <- tbl_with_times %>% 
      mutate(period = floor(time / 30) + 1) %>% 
      group_by(ID, period) %>% 
      summarize(
        description = Mode(description),
        location = Mode(location),
        .groups = 'drop'
      )
    
    return(tbl_with_modes)
  })
  ATUS_30 <- dplyr::bind_rows(ATUS_30)
  
  return(list(ATUS_1 = tibble(), ATUS_30 = ATUS_30))
}

clean_location <- function(ATUS_30){

  # location descriptions per survey
  location_codes <- tibble::tribble(
    ~location, ~loc_description,
    1, "Respondent's home or yard",
    2, "Respondent's workplace",
    3, "Someone else's home",
    4, "Restaurant or bar",
    5, "Place of worship",
    6, "Grocery store",
    7, "Other store/mall",
    8, "School",
    9, "Outdoors away from home",
    10, "Library",
    11, "Other place",
    12, "Car, truck, or motorcycle (driver)",
    13, "Car, truck, or motorcycle (passenger)",
    14, "Walking",
    15, "Bus",
    16, "Subway/train",
    17, "Bicycle",
    18, "Boat/ferry",
    19, "Taxi/limousine service",
    20, "Airplane",
    21, "Other mode of transportation",
    30, "Bank",
    31, "Gym/health club",
    32, "Post Office",
    89, "Unspecified place",
    99, "Unspecified mode of transportation"
  )
  
  # add codes
  ATUS_30 <- ATUS_30 |> 
    dplyr::left_join(location_codes, by = 'location') |> 
    dplyr::select(ID, period, description, location = loc_description)
  
  # replace NA for activities noted in survey
  ATUS_30 <- ATUS_30 |> 
    mutate(location = ifelse(description == 'Sleep : No SCC', '[sleep]', location),
           location = ifelse(description %in% c('Personal Care : No SCC', 'Personal Care : SCC'),
                             '[personal]', location),
           location = ifelse(is.na(location), '[did not respond]', location))
  
  return(ATUS_30)
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

