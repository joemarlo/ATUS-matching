library(tidyverse)

`%notin%` <- negate(`%in%`)
scale_01 <- function(x) (x - min(x))/diff(range(x))

#' Plot the standardized difference in means
#' 
#' Plot the standardized difference in means from the output of arm::balance()
#'
#' @param .matches output from arm::balance()
#'
#' @return ggplot object
plot_diff_means <- function(.matches){
  
  if (isFALSE(is(.matches, 'balance'))) stop(".matches must be class 'balance'")
  
  # plot it
  p <- tibble(unmatched = balance_with$diff.means.raw[, 'diff.std'],
              matched = balance_with$diff.means.matched[, 'diff.std'],
              covs = setdiff(balance_with$covnames, 'treat')) %>% 
    pivot_longer(-covs) %>% 
    ggplot(aes(x = value, y = covs, group = name, color = name)) +
    geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey70') +
    geom_point() +
    labs(title = "Standardized difference in means",
         x = NULL,
         y = NULL,
         color = NULL) +
    theme(legend.position = 'bottom')
  
  return(p)
}

#' Calculate balance statistics from matchit model
#'
#' Calculate the standardized difference in means between treatment and control from a matchit model
#'
#' @param rawdata a dataframe of the original data
#' @param .matches the object returned from MatchIt::matchit()
#' @param .propensity_model a propensity model (only glm() is tested)
#'
#' @return a dataframe
calculate_balance <- function(rawdata, .matches, .propensity_model){
  
  if (isFALSE(is(.matches, 'matchit'))) stop(".matches must be class 'matchit'")
  if (isFALSE('treatment' %in% colnames(rawdata))) stop("rawdata must have column 'treatment'")
  
  # create df of the treated data and the matched control data
  match_indices <- as.numeric(.matches$match.matrix)
  control_df <- distinct(rawdata[match_indices,]) #TODO should this be distinct???
  treated_df <- rawdata[rawdata$treatment,]
  matched <- bind_rows(control_df, treated_df)
  
  # calculate balance stats
  balance_stats <- arm::balance(
    rawdata = rawdata,
    matched = matched,
    pscore.fit = .propensity_model
  )
  
  return(balance_stats)
}


#' Calculate the penalized total distance of elements in a matrix
#' 
#' Returns the summed value of given elements in a matrix. Penalized if the same row is used twice; useful in optimization process to minimize likelihood row_indices is chosen.
#' 
#' If row_indices=1:5 then it would subset mat[1,1], mat[2,2], etc.
#' If row_indices=1,3,3,4,3 would subset mat[1,1], mat[3,2], mat[3,3], mat[4,4], mat[3,5]
#'
#' @param row_indices a vector of numeric indices denoting the rows. Position in vector denotes the column
#' @param mat the matrix. Typically a distance matrix
#' @param penalized_value replacement value in matrix once row is used in a previous step
#'
#' @return a numeric representing the sum of the elements
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(25), ncol = 5)
#' get_distance(c(1,3,2,5,4), mat)
#' get_distance(c(1,3,2,5,5), mat)
get_distance <- function(row_indices, mat, penalized_value = 1e10){

  col_indices <- 1:ncol(mat)
  if (length(row_indices) != length(col_indices)) stop('row_indices should == ncol(mat)')
  
  # save the elements in the given row_indices and replace those rows with penalized_value
  values <- c()
  for (i in col_indices){
    row_index <- row_indices[i]
    values[i] <- mat[row_index, i]
    mat[row_index,] <- penalized_value
  }
  
  # sum to get the total distance
  total_distance <- sum(values)
  
  return(total_distance)
}

#' Calculate the minimum possible total distance
#' 
#' Returns the matching row for each column that greedily minimizes the total distance across all rows.
#'
#' @param mat the matrix. Typically a distance matrix
#' @param with_replacement logical. Can a row index be matched to multiple columns?
#' @param penalized_value replacement value in matrix once row is used in a previous step
#'
#' @return a vector denoting the row indexes 
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(25), ncol = 5)
#' minimal_distance(mat)
minimal_distance <- function(mat, with_replacement = FALSE, penalized_value = 1e10){
  
  if (!isTRUE(is.matrix(mat))) stop('mat must be a matrix')
  
  n_cols <- ncol(mat)
  n_rows <- nrow(mat)
  
  # return the row indices that denote the minimal value per column
  if (isTRUE(with_replacement)){
    best_matches <- apply(mat, 2, which.min)
    return(best_matches)
  }

  # grid search for optimum
  grid_results <- NMOF::gridSearch(get_distance,
                                   mat = mat,
                                   penalized_value = penalized_value,
                                   lower = rep(1, n_cols),
                                   upper = rep(n_rows, n_cols),
                                   n = n_rows)$minlevels
  
  # grid search
  # faster but only works n_rows >= n_cols
  # create grid of possible values with no duplicates
  # full_grid <- expand.grid(rep(list(1:n_rows), n_cols))
  # indices <- apply(full_grid, 1, function(row) sum(1:n_rows %in% row) == n_cols)
  # no_dups <- full_grid[indices,]
  # check <- nrow(no_dups) == (factorial(n_rows) / factorial(n_rows-n_cols))
  # if (!isTRUE(check)) stop('Internal error: nrow of grid search grid does not match expected nrow')
  # distances <- apply(no_dups, 1, function(row) get_distance(row, mat))
  # grid_results <- as.numeric(no_dups[which.min(distances),])
  
  # if there are more columns then rows (e.g. k_t2 > k_t1) then replace 
    # the duplicates with the furthest distance with NAs
  # this means the medoids that are duplicate matches -- but are not the best match -- will not be matched
  if (n_cols > n_rows){
    warning("ncol(mat) > nrow(mat); replacing duplicate row index with furthest distance with NA")
    tab <- table(grid_results)
    dup_values <- as.numeric(names(tab[tab > 1]))
    for (dup_value in dup_values) {
      min_value <- which.min(mat[dup_value, which(grid_results == dup_value)])
      grid_results[grid_results == dup_value][-min_value] <- NA
    }
  }

  return(grid_results)
}

#' Swap cluster labels given a mapping 
#'
#' Given two vectors denoting cluster labels, change the labels in the second vector to match the mapping provided label_mapping. Maintains the same order of the cluster vector.
#'
#' @param clusters_one a numeric vector of denoting cluster labels
#' @param clusters_two a numeric vector of denoting cluster labels
#' @param label_mapping a vector of length unique(clusters_one) that denotes which labels should be swapped based on index. Should be the output from minimal_distance().
#'
#' @return a factor representing `clusters_two` with swapped labels and levels that match union(`clusters_one`, `clusters_two`)
#' @export
#'
#' @examples
#' clus1 <- sample(1:5, 100, replace = T)
#' clus2 <- sample(1:5, 100, replace = T)
#' clus1_labels <- sort(unique(clus1))
#' clus2_labels <- sort(unique(clus2))
#' mapping <- sample(clus2_labels, length(clus1_labels), replace = FALSE) # defines the clusters from clus1 that match to clus2
#' swap_labels(clus1, clus2, mapping)
swap_labels <- function(clusters_one, clusters_two, label_mapping){
  
  if (!(is.numeric(clusters_one) & is.numeric(clusters_two))) stop('clusters_one and clusters_two must be numeric vectors')
  if (any(c(is.na(clusters_one), is.na(clusters_two)))) stop('clusters_one and/or clusters_two contain NAs')

  cluster_1_labels <- sort(unique(clusters_one))
  cluster_2_labels <- sort(unique(clusters_two))
  if (length(cluster_1_labels) != length(label_mapping)){
    stop('Length of unique(clusters_one) should match length of label_mapping')
  }
  
  # the mapping explicitly states how t1 -> t2 but directionality doesn't matter 
  label_mapping_df <- data.frame(t1 = seq_along(label_mapping),
                                 t2 = label_mapping)
  
  # join to get the new labels (this keeps the order unlike base::merge)
  clusters_two_new <- left_join(x = data.frame(t2 = clusters_two), 
                                y = label_mapping_df,
                                by = 't2')
  colnames(clusters_two_new) <- c('t2_old', 't2_new')
  
  # the NAs represent clusters in t2 that are not matched to clusters in t1
  # we'll make that explicit by changing the label
  if (any(is.na(clusters_two_new$t2_new))){
    distinct_NA_pairs <- distinct(clusters_two_new) %>% filter(is.na(t2_new))
    distinct_NA_pairs$t2_new_NA <- paste0("unmatched ", seq_along(distinct_NA_pairs$t2_new))
    clusters_two_new <- left_join(x = clusters_two_new,
                                  y = distinct_NA_pairs,
                                  by = c('t2_old', 't2_new'))
    clusters_two_new <- ifelse(is.na(clusters_two_new$t2_new),
                               clusters_two_new$t2_new_NA,
                               clusters_two_new$t2_new)
  } else {
    clusters_two_new <- clusters_two_new$t2_new
  }

  # convert to a factor so ordering matches clusters_one
  # this is important later when viewing the transition matrix
  clusters_two_new_f <- factor(clusters_two_new,
                               levels = union(cluster_1_labels, sort(unique(clusters_two_new))))
  
  # final checks
  if (!isTRUE(length(clusters_two) == length(clusters_two_new_f))){
    stop("Internal error: length of new cluster vector does not match original")
  }
  
  return(clusters_two_new_f)
}


# time use estimates ------------------------------------------------------

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

#' Format minutes as hour:minute
#'
#' @param min a numeric representing a count of minutes
#'
#' @return
#' @export
#'
#' @examples
#' format_hour_minute(230)
format_hour_minute <- function(min) {
  H <- floor(min / 60)
  M <- min %% 60
  M <- ifelse(nchar(M) == 1, paste0('0', M), as.character(M))
  formatted <- paste0(H, ":", M)
  return(formatted)
}



# global vars -------------------------------------------------------------
options(scipen = 999)


# experimental ------------------------------------------------------------

# # swap labels of clusters based on matching
# clus1 <- sample(1:5, 100, replace = T)
# clus2 <- sample(1:5, 100, replace = T)
# clus1_labels <- sort(unique(clus1))
# clus2_labels <- sort(unique(clus2))
# mapping <- sample(clus2_labels, length(clus1_labels), replace = FALSE) # defines the clusters from clus1 that match to clus2
# 
# # merge to get the new labels
# clusters_t2_numeric_relabeled <- tibble(clus1 = clus1_labels, 
#                                         clus2 = mapping) %>% 
#   left_join(x = tibble(clus2 = clus2),
#             y = .,
#             by = 'clus2') %>% 
#   rename(clus2_original = clus2,
#          clus2_new = clus1)
# 
# # replace NAs with new labels
# # NAs represent the clusters that were not matched
# clusters_t2_numeric_relabeled <- clusters_t2_numeric_relabeled %>% 
#   distinct() %>% 
#   filter(is.na(clus2_new)) %>% 
#   arrange(clus2_original) %>% 
#   mutate(clus2_new = row_number() + sum(!is.na(mapping))) %>% 
#   left_join(x = clusters_t2_numeric_relabeled,
#             y = .,
#             by = 'clus2_original') %>% 
#   mutate(clus2_new = pmax(clus2_new.x, clus2_new.y, na.rm = TRUE)) %>% 
#   pull(clus2_new)
# 
# # test
# sum(table(clus2, clusters_t2_numeric_relabeled) > 0) == n_distinct(clus2)
# sum(table(clus2, clusters_t2_numeric_relabeled)) == length(clus2)
# 
# 
# 
# # TODO
# mdist_weighted <- function (data.x, data.y = NULL, vc = NULL, weights){
#   # hacked from StatMatch::mahalanobis.dist and WMDB::wmahalanobis
#   
#   xx <- as.matrix(data.x)
#   if (is.null(data.y)) 
#     yy <- as.matrix(data.x)
#   else yy <- as.matrix(data.y)
#   if (is.null(vc)) {
#     if (is.null(data.y)) 
#       vc <- var(xx)
#     else vc <- var(rbind(xx, yy))
#   }
#   ny <- nrow(yy)
#   md <- matrix(0, nrow(xx), ny)
#   for (i in 1:ny) {
#     md[, i] <- mahalanobis(xx, yy[i, ], cov = vc)
#   }
#   if (is.null(data.y)) 
#     dimnames(md) <- list(rownames(data.x), rownames(data.x))
#   else dimnames(md) <- list(rownames(data.x), rownames(data.y))
#   sqrt(md)
# }
