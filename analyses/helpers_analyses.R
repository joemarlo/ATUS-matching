library(tidyverse)

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
#' Returns the matching row for each column that greedily minimizes the total distance across all rows. Rows are only selected once -- akin to sampling without replacement.
#'
#' @param mat the matrix. Typically a distance matrix
#' @param with_replacement logical. Can a row index be matched to multiple columns?
#' @param penalized_value replacement value in matrix once row is used in a previous step
#'
#' @return a denoting vector denoting the row indexes 
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(25), ncol = 5)
#' minimal_distance(mat)
minimal_distance <- function(mat, with_replacement = FALSE, penalized_value = 1e10){
  
  if (!is.matrix(mat)) stop('mat must be a matrix')
  
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
  
  # TODO: could replace with input vector of row_indices w/o duplicates
  # only works n_rows >= n_cols
  # full_grid <- expand.grid(rep(list(1:n_rows), n_cols))
  # indices <- apply(full_grid, 1, function(row) sum(1:n_rows %in% row) == n_cols)
  # no_dups <- full_grid[indices,]
  # distances <- apply(no_dups, 1, function(row) get_distance(row, mat))
  # grid_results <- as.numeric(no_dups[which.min(distances),])
  # nrow(no_dups) == (factorial(n_rows) / factorial(n_rows-n_cols))
  
  # if there are more columns then rows (e.g. k_t2 > k_t1) then replace 
  # the duplicates with the furthest distance with NAs
  if ((n_cols - 1) == n_rows){
    warning("ncol(mat) > nrow(mat); replacing duplicate row index with NA")
    tab <- table(grid_results)
    dup_values <- as.numeric(names(tab[tab > 1]))
    min_value <- which.min(mat[dup_values, which(grid_results == dup_values)])
    grid_results[grid_results == dup_values][-min_value] <- NA
  } else if ((n_cols - 1) > n_rows) {
    #TODO: remove this restriction?
    stop('(ncol(mat)-1) > nrow(mat); cannot currently handle matrices where there are two or more columns than the number of rows')
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
#' @return
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
  
  # TODO: check for overlap?
  
  cluster_1_labels <- sort(unique(clusters_one))
  cluster_2_labels <- sort(unique(clusters_two))
  if (length(cluster_1_labels) != length(label_mapping)) stop('Length of unique(clusters_one) should match length of label_mapping')
  
  # TODO: what if there are NAs in label_mapping?
  
  # merge to get the new labels
  clusters_two_relabeled <- tibble(clus1 = cluster_1_labels, 
                                   clus2 = label_mapping) %>% 
    left_join(x = tibble(clus2 = clusters_two),
              y = .,
              by = 'clus2') %>% 
    rename(clus2_original = clus2,
           clus2_new = clus1)
  
  # replace NAs with new labels
  # NAs represent the clusters that were not matched
  clusters_two_relabeled <- clusters_two_relabeled %>% 
    distinct() %>% 
    filter(is.na(clus2_new)) %>% 
    arrange(clus2_original) %>% 
    mutate(clus2_new = row_number() + sum(!is.na(label_mapping))) %>% 
    left_join(x = clusters_two_relabeled,
              y = .,
              by = 'clus2_original') %>% 
    mutate(clus2_new = pmax(clus2_new.x, clus2_new.y, na.rm = TRUE)) %>% 
    pull(clus2_new)
  
  return(clusters_two_relabeled)
  
  ## old explicit method
  # label_mapping_appended <- paste0(label_mapping, "_new")
  # 
  # # replace the labels with matches
  # for(i in seq_along(cluster_1_labels)){
  #   label_old <- cluster_1_labels[i]
  #   label_new <- label_mapping_appended[i]
  #   clusters_two[clusters_two == label_old] <- label_new
  # }
  # 
  # # for labels without a match, set label as the largest number
  # labels_missing <- setdiff(cluster_2_labels, label_mapping)
  # for(label_missing in seq_along(labels_missing)){
  #   label_missing <- labels_missing[i]
  #   label_new <- paste0(length(cluster_1_labels) + i, "_new")
  #   clusters_two[clusters_two == label_missing] <- label_new
  # }
  # 
  # return(clusters_two)
}



# global vars -------------------------------------------------------------
options(scipen = 999)


# experimental ------------------------------------------------------------

# swap labels of clusters based on matching
clus1 <- sample(1:5, 100, replace = T)
clus2 <- sample(1:5, 100, replace = T)
clus1_labels <- sort(unique(clus1))
clus2_labels <- sort(unique(clus2))
mapping <- sample(clus2_labels, length(clus1_labels), replace = FALSE) # defines the clusters from clus1 that match to clus2

# merge to get the new labels
clusters_t2_numeric_relabeled <- tibble(clus1 = clus1_labels, 
                                        clus2 = mapping) %>% 
  left_join(x = tibble(clus2 = clus2),
            y = .,
            by = 'clus2') %>% 
  rename(clus2_original = clus2,
         clus2_new = clus1)

# replace NAs with new labels
# NAs represent the clusters that were not matched
clusters_t2_numeric_relabeled <- clusters_t2_numeric_relabeled %>% 
  distinct() %>% 
  filter(is.na(clus2_new)) %>% 
  arrange(clus2_original) %>% 
  mutate(clus2_new = row_number() + sum(!is.na(mapping))) %>% 
  left_join(x = clusters_t2_numeric_relabeled,
            y = .,
            by = 'clus2_original') %>% 
  mutate(clus2_new = pmax(clus2_new.x, clus2_new.y, na.rm = TRUE)) %>% 
  pull(clus2_new)

# test
sum(table(clus2, clusters_t2_numeric_relabeled) > 0) == n_distinct(clus2)
sum(table(clus2, clusters_t2_numeric_relabeled)) == length(clus2)



# TODO
mdist_weighted <- function (data.x, data.y = NULL, vc = NULL, weights){
  # hacked from StatMatch::mahalanobis.dist and WMDB::wmahalanobis
  
  xx <- as.matrix(data.x)
  if (is.null(data.y)) 
    yy <- as.matrix(data.x)
  else yy <- as.matrix(data.y)
  if (is.null(vc)) {
    if (is.null(data.y)) 
      vc <- var(xx)
    else vc <- var(rbind(xx, yy))
  }
  ny <- nrow(yy)
  md <- matrix(0, nrow(xx), ny)
  for (i in 1:ny) {
    md[, i] <- mahalanobis(xx, yy[i, ], cov = vc)
  }
  if (is.null(data.y)) 
    dimnames(md) <- list(rownames(data.x), rownames(data.x))
  else dimnames(md) <- list(rownames(data.x), rownames(data.y))
  sqrt(md)
}
