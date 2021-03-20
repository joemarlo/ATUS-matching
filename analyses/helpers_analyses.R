
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
  control_df <- distinct(rawdata[match_indices,])
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
