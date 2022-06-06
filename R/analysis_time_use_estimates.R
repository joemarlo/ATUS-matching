###
# This functions are no longer used
###


#' Estimate the mean minutes spent on a time-use activity
#'
#' Estimate the survey-weighted mean amount of time spent on a given activity or activities. 
#'
#' @param df the summary dataframe containing the time use data. Use atussum_0318 for multi-year or atussum_XX for single year estimates
#' @param groups grouping variables. E.g. 'TESEX' for sex
#' @param activities the t codes for time-use activities. If none are provided, then all t-codes are used
#' @param simplify if TRUE, then simplify output to the group. Can be a dataframe specifying a rollup of activities (see `descriptions` dataframe in 'data/helpers.R')
#' @include_SE include standard errors? Automatically sets `simplify = TRUE`. Calculation may take a while if there are multiple activities. Uses replicate weight method outlined here https://www.bls.gov/tus/atususersguide.pdf
#'
#' @return a tidy dataframe listing the activity, any grouping variables, and the survey-weighted mean minutes spent in that activity
#' @export
#'
#' @examples
#' get_minutes(atussum_0318)
#' get_minutes(atussum_0318, groups = 'TESEX') # group by sex
#' game.codes <- colnames(atussum_0318)[colnames(atussum_0318) %in% paste0('t', 130101:130199)]
#' get_minutes(atussum_0318, groups = 'TESEX', activities = game.codes)
#' get_minutes(atussum_0318, groups = 'TESEX', simplify = descriptions) # see 'data/helpers.R' for descriptions dataframe
get_minutes <- function(df, groups = NULL, activities = NULL, simplify = NULL, include_SE = FALSE){
  
  if (!all("TUCASEID" %in% names(df) & any(c('TUFINLWGT', 'TUFNWGTP') %in% names(df)))) {
    stop("data must contain variables named TUCASEID and (TUFINLWGT or TUFNWGTP)")
  }
  
  # if no activities are explicitly provided then include all of them
  if (is.null(activities)){
    activities <- str_subset(names(df), '^t[0-9]')
    message('get_minutes(): No activities explicitly provided. Returning all activities in dataframe.')
  }
  
  if (isTRUE(simplify)){
    simplify <- tibble(activity = activities, 
                       description = 'All activites provided')
  }
  
  if (!is.null(simplify)) {
    if (!is.data.frame(simplify) | !(all(names(simplify) %in% c('activity', 'description')))) {
      stop(
        "if simplifying then simplify must be a data frame with columns 'activity' and 'description'"
      )
    }
  }
  
  # select the correct weighting variable based on the data provided
  weight.var <- c('TUFNWGTP', 'TUFINLWGT')[c('TUFNWGTP', 'TUFINLWGT') %in% names(df)]
  
  estimates <- df %>% 
    select(TUCASEID, all_of(weight.var), groups, activities) %>%
    pivot_longer(cols = -c('TUCASEID', all_of(weight.var), groups),
                 names_to = "activity",
                 values_to = 'time') %>%
    group_by_at(vars(activity, groups)) %>% 
    summarize(weighted.minutes = sum(.data[[weight.var]] * time) / sum(.data[[weight.var]]),
              .groups = 'drop') %>%
    ungroup() %>%
    {
      # if simplifying, then left join to get the descriptions and then
      #  sum the minutes
      if(!is.null(simplify)) {
        left_join(x = .,
                  y = simplify,
                  by = 'activity') %>% 
          select(activity = description, groups, weighted.minutes) %>% 
          group_by_at(vars(activity, groups)) %>%
          summarize(weighted.minutes = sum(weighted.minutes),
                    .groups = 'drop') %>% 
          ungroup()
      } else .
    }
  
  # calculate standard errors via replicate weights method
  if (isTRUE(include_SE)){
    # calculate the original statistic
    y0 <- get_minutes(
      df = df,
      groups = groups,
      activities = activities,
      simplify = TRUE,
      include_SE = FALSE
    )
    
    # repeat the statistic calculation using each weight
    statistics <- apply(atuswgts_0318[, -1], MARGIN = 2, FUN = function(wgt) {
      df[, 'TUFNWGTP'] <- wgt
      minutes <- get_minutes(
        df = df,
        groups = groups,
        activities = activities,
        simplify = TRUE,
        include_SE = FALSE
      )
      return(minutes$weighted.minutes)
    }
    )
    
    # calculate the standard error. See https://www.bls.gov/tus/atususersguide.pdf
    y0$SE <- sqrt((4 / 160) * rowSums((statistics - y0$weighted.minutes)^2))
    
    return(y0)
  }
  
  return(estimates)
}

#' Estimate the participation rate for a time-use activity
#'
#' Estimate the survey-weighted participation rate spent on a given activity or activities. 
#'
#' @param df the summary dataframe containing the time use data. Use atussum_0318 for multi-year or atussum_XX for single year estimates
#' @param groups grouping variables. E.g. 'TESEX' for sex
#' @param activities the t codes for time-use activities. If none are provided, then all t-codes are used
#' @param simplify if TRUE, then simplify output to the group. Can be a dataframe specifying a rollup of activities (see `descriptions` dataframe in 'data/helpers.R')
#'
#' @return a tidy dataframe listing the activity, any grouping variables, and the survey-weighted participation rate for that activity
#' @export
#'
#' @examples
#' get_participation(atussum_0318)
#' get_participation(atussum_0318, groups = 'TESEX') # group by sex
#' game.codes <- colnames(atussum_0318)[colnames(atussum_0318) %in% paste0('t', 130101:130199)]
#' get_participation(atussum_0318, groups = 'TESEX', activities = game.codes)
#' get_participation(atussum_0318, groups = 'TESEX', simplify = descriptions) # see 'data/helpers.R' for descriptions dataframe
get_participation <- function(df, groups = NULL, activities = NULL, simplify = NULL) {
  
  if (!all("TUCASEID" %in% names(df) & any(c('TUFINLWGT', 'TUFNWGTP') %in% names(df)))) {
    stop("data must contain variables named TUCASEID and (TUFINLWGT or TUFNWGTP)")
  }
  
  # if no activities are explicitly provided then include all of them
  if (is.null(activities)){
    activities <- str_subset(names(df), '^t[0-9]')
    message('get_participation(): No activities explicitly provided. Returning all activities in dataframe.')
  }
  
  if (isTRUE(simplify)){
    simplify <- tibble(activity = activities, 
                       description = 'All activites provided')
  }
  
  if (!is.null(simplify)) {
    if (!is.data.frame(simplify) | !(all(names(simplify) %in% c('activity', 'description')))) {
      stop(
        "if simplifying then simplify must be a data frame with columns 'activity' and 'description'"
      )
    }
  }
  
  weight.var <- c('TUFNWGTP', 'TUFINLWGT')[c('TUFNWGTP', 'TUFINLWGT') %in% names(df)]
  
  df %>%
    select(TUCASEID, all_of(weight.var), groups, activities) %>%
    pivot_longer(
      cols = -c('TUCASEID', all_of(weight.var), groups),
      names_to = "activity",
      values_to = 'time'
    ) %>%
    {
      # if simplifying, then left join to get the descriptions and then
      #  sum the minutes
      if (!is.null(simplify)) {
        left_join(x = .,
                  y = simplify,
                  by = 'activity') %>%
          select(TUCASEID, all_of(weight.var), groups, activity = description, groups, time)
      } else .
    } %>%
    group_by_at(vars(activity, groups)) %>%
    group_modify(~ {
      # sum the distinct weights that have time > 0
      num <- .x %>%
        filter(time > 0) %>%
        select(TUCASEID, all_of(weight.var)) %>%
        distinct() %>%
        pull(weight.var) %>%
        sum()
      
      # sum all distinct weights
      denom <- select(.x, TUCASEID, all_of(weight.var)) %>%
        distinct() %>%
        pull(weight.var) %>%
        sum()
      
      return(tibble(participation.rate = num / denom))
    }) %>%
    ungroup()
}

#' Estimate the mean minutes spent on a time-use activity, only for the participants
#'
#' Estimate the survey-weighted mean amount of time spent on a given activity or activities. This differs from `get_minutes()` as it returns the mean amount of minutes for individuals that participated in the activity. `get_minutes()` returns the mean minutes for the entire survey population.
#'
#' @param df the summary dataframe containing the time use data. Use atussum_0318 for multi-year or atussum_XX for single year estimates
#' @param groups grouping variables. E.g. 'TESEX' for sex
#' @param activities the t codes for time-use activities. If none are provided, then all t-codes are used
#' @param simplify if TRUE, then simplify output to the group. Can be a dataframe specifying a rollup of activities (see `descriptions` dataframe in 'data/helpers.R')
#'
#' @return a tidy dataframe listing the activity, any grouping variables, the survey-weighted mean minutes, participation rate, and mean minutes per participant for that activity
#' @export
#'
#' @examples
#' get_min_per_part(atussum_0318)
#' get_min_per_part(atussum_0318, groups = 'TESEX') # group by sex
#' game.codes <- colnames(atussum_0318)[colnames(atussum_0318) %in% paste0('t', 130101:130199)]
#' get_min_per_part(atussum_0318, groups = 'TESEX', activities = game.codes)
#' get_min_per_part(atussum_0318, groups = 'TESEX', simplify = descriptions) # see 'data/helpers.R' for descriptions dataframe
get_min_per_part <- function(df, groups = NULL, activities = NULL, simplify = NULL) {
  
  min.df <- get_minutes(df = df, groups = groups, activities = activities, simplify = simplify)
  part.df <- get_participation(df = df, groups = groups, activities = activities, simplify = simplify)
  
  if (nrow(min.df) != nrow(part.df)) {
    stop("Issue in matching minutes and participation data.frames. Number of rows does not match.")
  }
  
  rslts <- full_join(min.df, part.df, by = c('activity', all_of(groups))) %>% 
    mutate(minutes.per.participant = weighted.minutes / participation.rate) %>% 
    select(activity, groups, weighted.minutes, participation.rate, minutes.per.participant)
  
  # fix NaNs; NaNs produced when participation.rate is 0
  rslts$minutes.per.participant[is.nan(rslts$minutes.per.participant)] <- 0
  
  return(rslts)
}
