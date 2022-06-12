
#' Match two years of survey respondents based on their demographics 
#'
#' Note: code must be manually updated if matching or blocking variables change
#'
#' @param demographics 
#' @param year1 
#' @param year2 
#' @param matching_vars 
#' @param method 
#' @param include_plots 
#'
#' @return
#' @export
#'
#' @examples
#' # See analysis/matching.R for an example
matching_mahalanobis <- function(demographics, year1, year2, matching_vars, method = c('1-to-many', 'many-to-1'), include_plots = FALSE){
  
  method <- match.arg(method)
  
  # TODO: double check
  if (method == 'many-to-1'){
    # time1 is many-to-one
    demographics$treatment <- demographics$year == year1 # flip time here to switch between many-to-1 and 1-to-many; ITT should use time2
  } else {
    demographics$treatment <- demographics$year == year2
  }
  
  # remove unnecessary columns for m distance
  demographics_trimmed <- demographics[, matching_vars]
  
  # split into treat and control
  demographics_treatment <- demographics_trimmed[demographics$treatment,]
  demographics_control <- demographics_trimmed[!demographics$treatment,]
  
  # dummy code categoricals
  demographics_treatment <- fastDummies::dummy_cols(
    demographics_treatment, 
    remove_selected_columns = TRUE,
    remove_first_dummy = TRUE)
  demographics_control <- fastDummies::dummy_cols(
    demographics_control, 
    remove_selected_columns = TRUE,
    remove_first_dummy = TRUE)
  
  # clean up column names
  col_names <- colnames(demographics_treatment)
  col_names <- stringr::str_replace_all(col_names, '[.]|[-]', " ")
  col_names <- stringr::str_squish(col_names)
  col_names <- stringr::str_replace_all(col_names, "[ ]", "_")
  colnames(demographics_treatment) <- col_names
  colnames(demographics_control) <- col_names
  
  # remove partner_working_NA column b/c it is co-linear
  demographics_treatment <- dplyr::select(demographics_treatment, -partner_working_NA)
  demographics_control <- dplyr::select(demographics_control, -partner_working_NA)
  
  
  # non weighted method -----------------------------------------------------
  
  # calculate mahalanobis distance
  demographics_mdistance <- StatMatch::mahalanobis.dist(
    data.x = demographics_treatment,
    data.y = demographics_control,
    vc = cov(demographics_treatment)
  )
  
  # get match by choosing row with smallest distance
  # this matches all of the treatments to some of the controls
  #   akin to matching with replacement
  match_indices <- apply(demographics_mdistance, 1, which.min)
  # length(match_indices) == nrow(demographics_treatment)
  
  # write out distances 
  # pair_distance <- apply(demographics_mdistance, 1, min)
  # tibble(pair_id = seq_along(pair_distance),
  #        distance = pair_distance) %>% 
  #   write_csv(path = file.path('data', 'pair_distance.csv'))
  
  
  # stratified matching -----------------------------------------------------
  
  # strata: age
  ages <- sort(unique(demographics_treatment$age))
  age_window <- 5
  age_window_half <- (age_window - 1) / 2
  
  # get logical indicating if column (t2) is in the age range of the row (t1)
  age_matches <- sapply(demographics_treatment$age, function(age){
    age_range <- seq(age - age_window_half, age + age_window_half, by = 1)
    t2_in_age_range <- demographics_control$age %in% age_range
    return(t2_in_age_range)
  })
  
  # strata: sex
  sex_matches <- sapply(demographics_treatment$sex_male, function(sex){
    is_match <- demographics_control$sex_male == sex
    return(is_match)
  })
  
  # strata: race
  race_matches <- sapply(demographics[demographics$treatment,]$race, 
                         USE.NAMES = FALSE, function(race){
                           is_match <- demographics[!demographics$treatment,]$race == race
                           return(is_match)
                         })
  
  # strata: labor_force_status
  # labor_force_matches <- sapply(demographics[demographics$treatment,]$labor_force_status, 
  #                        USE.NAMES = FALSE, function(labor_force_status){
  #   is_match <- demographics[!demographics$treatment,]$labor_force_status == labor_force_status
  #   return(is_match)
  # })
  
  # strata: quarter
  # quarter_matches <- sapply(demographics_treatment$quarter, function(quarter) {
  #   is_match <- demographics_control$quarter == quarter
  #   return(is_match)
  # })
  
  # get the index of the best match within the age range
  index_of_best_match <- c()
  distance_of_best_match <- c()
  potential_match_pop <- c()
  for (i in 1:nrow(demographics_mdistance)){
    
    # create vector of distances for this t1 observations
    t1 <- demographics_mdistance[i,]
    
    # for observations where observations do not match strata, replace distance with
    # unrealistically high number so it is not chosen but the index is kept
    t2_matches_age <- age_matches[,i]
    t2_matches_sex <- sex_matches[,i]
    t2_matches_race <- race_matches[,i]
    # t2_matches_labor_force <- labor_force_matches[,i]
    # t2_matches_quarter <- quarter_matches[, i]
    t2_matches_all <- t2_matches_age & t2_matches_sex & t2_matches_race #& t2_matches_quarter #& t2_matches_labor_force
    t1[!t2_matches_all] <- 1e10
    potential_match_pop[i] <- sum(t2_matches_all)
    
    # store the best match
    index_of_best_match[i] <- which.min(t1)
    distance_of_best_match[i] <- min(t1)
  }
  # rm(t1, t2_matches_age, t2_matches_sex, t2_matches_race, t2_matches_all, 
  #    i, age_matches, sex_matches, race_matches, labor_force_matches, quarter_matches)
  
  # how often is there no potential matchs
  # sum(potential_match_pop == 0)
  
  # replace these non matches with NAs
  # do not replace with NA and then create final dataframes because pair_ids will be wrong
  # index_of_best_match[potential_match_pop == 0] <- NA
  
  # how often does the stratifying have the same result as not stratifying?
  # mean(match_indices == index_of_best_match, na.rm = TRUE)
  
  # check the stratifying worked
  # max_age_diff <- range(demographics_treatment$age - demographics_control[index_of_best_match,]$age, na.rm = TRUE)
  # (diff(max_age_diff)+1) <= age_window
  
  # identify bad matches
  bad_matches <- which(potential_match_pop == 0)
  bad_matches <- tibble(ID = demographics[demographics$treatment,]$ID[bad_matches]) #%>% 
    # write_csv(file.path('data', 'thrown_out_observations.csv'))
    # write_csv(file.path(file_path, 'data', 'IDs_with_no_match.csv'))
  
  # write out distances 
  pair_distance <- tibble(pair_id = seq_along(distance_of_best_match),
         distance = distance_of_best_match) %>%
    filter(pair_id %notin% bad_matches) #%>%
    # write_csv(file.path(file_path, 'data', 'pair_distance.csv'))
  
  # overwrite match_indices if using stratifying
  match_indices <- index_of_best_match
  
  
  # create dataframe of the matches -----------------------------------------
  
  # create treatment and control dfs that have matching orders
  demographics_treated <- demographics[demographics$treatment,]
  demographics_control <- demographics[!demographics$treatment,][match_indices,]
  
  # add pair id so its easy to identify the matched pairs
  demographics_treated$pair_id <- 1:nrow(demographics_treated)
  demographics_control$pair_id <- 1:nrow(demographics_control)
  
  # remove the bad matches caused by too small stratum
  demographics_treated <- demographics_treated[demographics_treated$pair_id %notin% bad_matches,]
  demographics_control <- demographics_control[demographics_control$pair_id %notin% bad_matches,]
  # rm(bad_matches)
  
  # combine the data
  final_matches <- bind_rows(demographics_treated, demographics_control)
  
  # move id columns to first column
  final_matches <- dplyr::select(final_matches, 'treatment', 'pair_id', 'ID', everything())
  
  # final_matches %>% arrange(pair_id) %>% View
  # final_matches %>% arrange_at(c(blocking_vars, 'pair_id')) %>% View
  
  
  # assess balance and overlap ----------------------------------------------
  
  plots <- NULL
  if (isTRUE(include_plots)){
    
    # plot distributions by variable
    p_counts <- final_matches %>% 
      dplyr::select(-c('treatment', 'pair_id', 'ID')) %>% 
      distinct() %>% 
      mutate(across(everything(), as.character)) %>% 
      pivot_longer(cols = -year) %>% 
      mutate(value = factor(
        value, 
        levels = c(0:99, 
                   'asian', 'black', 'white', 'other',
                   'married', 'not married',
                   'Midwest', 'Northeast', 'South', 'West',
                   'TRUE', 'FALSE',
                   'employed', 'not employed',
                   'not in labor force', 'unemployed - looking', 'unemployed - on layoff',
                   'employed - absent', 'employed - at work',
                   unique(final_matches$sex),
                   # unique(final_matches$labor_force_status),
                   levels(final_matches$education),
                   unique(final_matches$metropolitan),
                   sort(unique(final_matches$fam_income))[-1])
      )
      ) %>% 
      ggplot(aes(x = value, group = year, fill = year)) +
      geom_bar(aes(y = ..prop..), position = 'dodge', stat = 'count') +
      scale_y_continuous(labels = scales::percent_format(1)) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(title = 'Counts of key groups within matched data',
           subtitle = 'Methodology: mahalanobis, blocking on sex, race, age +/- 2 years', #, labor_force_status',
           caption = 'Only includes distinct observations (i.e. removes duplicates due to matching with replacement)',
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = 'bottom')
    # ggsave(file.path(file_path, "plots", 'matching', "counts_matched_mahalanobis.png"), height = 12, width = 9)
    
    
    # privileged variables ----------------------------------------------------
    
    # how many of the pairs perfectly match on each variable
    match_summary <- final_matches %>% 
      group_by(pair_id) %>% 
      summarize(across(all_of(matching_vars), 
                       ~ first(.x) == last(.x))) %>%
      dplyr::select(-pair_id) %>% 
      mutate(n_matches = rowSums(.))
    p_matches_privileged <- match_summary %>% 
      dplyr::select(all_of(blocking_vars)) %>% 
      mutate(n_matches = rowSums(.)) %>% 
      ggplot(aes(x = n_matches)) +
      geom_bar(aes(y = ..prop..)) +
      scale_x_continuous(breaks = 1:length(blocking_vars)) +
      scale_y_continuous(labels = scales::percent_format(1),
                         breaks = seq(0, 1, 0.1)) +
      geom_hline(yintercept = 1, linetype = 'dashed') +
      labs(title = 'Proportion of matches that match perfectly on: ',
           subtitle = paste0(
             paste0(blocking_vars, collapse = ', '),
             '\nMethodology: mahalanobis, blocking on sex, race, age +/- 2 years, quarter' #, labor_force_status'
           ),
           x = 'Number of matches across all privileged variables',
           y = 'Proportion of all pairs')
    # ggsave(file.path(file_path, "plots", 'matching', "privileged_vars_all_mahalanobis.png"), height = 5, width = 9)
    p_matches_perfect <- match_summary %>%
      summarize(across(all_of(matching_vars), mean)) %>% 
      pivot_longer(everything()) %>% 
      mutate(Privileged = name %in% blocking_vars,
             isNumeric = name %in% vars_numeric,
             isNumeric = if_else(isNumeric, 'Numeric variables', 'Categorical variables')) %>% 
      ggplot(aes(x = reorder(name, -Privileged), y = value, fill = Privileged)) +
      geom_col() +
      geom_hline(yintercept = 1, linetype = 'dashed', color = 'darkgreen') +
      geom_hline(yintercept = 0.9, linetype = 'dashed', color = 'red') +
      scale_y_continuous(labels = scales::percent_format(1),
                         breaks = seq(0, 1, 0.1)) +
      facet_grid(~isNumeric, scales = 'free_x') +
      labs(title = 'How many pairs matched perfectly for each variable?',
           subtitle = paste0("Yellow variables are not explicitly privileged but are highlighted for emphasis",
                             '\nMethodology: mahalanobis, blocking on sex, race, age +/- 2 years, quarter'), #, labor_force_status'),
           x = NULL,
           y = 'Proportion of all pairs') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = 'none')
    # ggsave(file.path(file_path, "plots", 'matching', "perfect_matches_mahalanobis.png"), height = 5, width = 9)
    
    # difference within matched pairs for numeric vars
    p_matches_numeric <- final_matches %>% 
      group_by(pair_id) %>% 
      summarize(across(all_of(vars_numeric), 
                       ~ first(.x) - last(.x))) %>% 
      pivot_longer(-pair_id) %>% 
      ggplot(aes(x = value)) +
      geom_boxplot() +
      scale_y_continuous(labels = NULL) +
      facet_wrap(~name, scales = 'free') +
      labs(title = 'Difference within matched pairs for numeric variables',
           subtitle = 'Methodology: mahalanobis, blocking on sex, race, age +/- 2 years, quarter', #, labor_force_status',
           x = NULL,
           y = NULL)
    # ggsave(file.path(file_path, "plots", 'matching', "numeric_differences_mahalanobis.png"), height = 5, width = 9)
    
    plots <- list(
      counts = p_counts,
      matches_privileged = p_matches_privileged,
      matches_perfect = p_matches_perfect,
      matches_numeric = p_matches_numeric
    )
  }
  
  
  out <- list(
    demographics_treated = demographics_treated,
    demographics_control = demographics_control,
    final_matches = final_matches,
    pair_distance = pair_distance,
    bad_matches = bad_matches,
    plots = plots
  )
  
  return(out)
}


#' Prepare the demographics for matching
#' 
#' Filter the demographics data so it only includes: the two years of data, data May or later, no weekends, no holidays, no NAs
#' 
#' Clean: factorize education recode sex
#' 
#' Add: state, age_youngest, partner_working, child_in_HH
#'
#' @param atusresp_0320 
#' @param demographics 
#' @param state_regions 
#' @param year1 
#' @param year2 
#' @param matching_vars 
#'
#' @return
#' @export
#'
#' @examples
matching_prep_demographics <- function(atusresp_0320, demographics, state_regions, year1, year2, matching_vars){
  
  # throw out observations who interview was before May 10th
  post_may_respondents <- atusresp_0320 %>% 
    mutate(dairy_date = lubridate::ymd(TUDIARYDATE),
           dairy_month = lubridate::month(dairy_date)) %>% 
    filter(TUYEAR %in% c(year1, year2),
           dairy_month >= 5) %>% 
    select(ID = TUCASEID, year = TUYEAR)
  
  # filter demographics to only include this observations
  demographics <- inner_join(demographics, 
                             post_may_respondents, 
                             by = c('ID', 'year'))
  rm(post_may_respondents)

  # filter to just time1 and time2, just weekdays and non-holidays
  boolean <- demographics$year %in% c(year1, year2) &
    demographics$day_of_week %in% 2:6 &
    demographics$holiday == 0
  demographics <- demographics[boolean,]
  rm(boolean)
  
  # factorize education
  demographics$education <- factor(
    demographics$education,
    levels = c(
      'Did not graduate from HS',
      'HS',
      'Some college',
      'Bachelors',
      'Masters',
      'Doctoral'
    )
  )
  
  # recode sex
  demographics$sex <- recode(demographics$sex, `1` = 'male', `2` = 'female')
  
  # add geographic region
  colnames(state_regions) <- tolower(colnames(state_regions))
  demographics <- left_join(demographics, y = state_regions, by = 'state')
  rm(state_regions)
  
  # replace age_youngest NAs with 0s (these are NA b/c they don't have a child)
  demographics$age_youngest[is.na(demographics$age_youngest)] <- 0
  
  # replace partner_working NA with 'NA' (b/c matching)
  demographics$partner_working[is.na(demographics$partner_working)] <- 'NA'
  
  # replace metropolitan NA with 'NA' (b/c matching)
  # demographics$metropolitan[is.na(demographics$metropolitan)] <- 'NA'
  
  # add indicator if child in household
  demographics$child_in_HH <- demographics$n_child > 0
  
  # remove these observations?
  # sum(is.na(demographics$fam_income))
  
  
  # demographic var selection -----------------------------------------------
  
  # matching: follow this but try a few different (SES is primary goal)
  # hard match: gender, race, urban/rural, region, partnership, essential worker status, labor force status
  # soft match: age, income, number of children, education (+/- 1); is there  relative within X miles, is spouse working
  # covariate: is there an elder in the household; **health**, neighborhood attributes (eg crime), length of commute (or public transport?)
  
  # matching_vars <- c('age', 'sex', 'race', 'fam_income', 'has_partner', 
  #                    'education', 'child_in_HH', 'n_child', 'age_youngest', 'region', 
  #                    'partner_working', 'elder_in_HH', 'metropolitan')#, 'labor_force_status')
  demographics <- demographics[, c('ID', 'year', matching_vars)]
  
  # NAs by column
  # skimr::skim(demographics)
  
  # remove NAs b/c education, region, and/or metropolitan unknown for 5%
  demographics <- na.omit(demographics)
  
  # create list of numeric vars
  vars_numeric <- demographics %>% 
    dplyr::select(all_of(matching_vars)) %>% 
    dplyr::select(where(is.numeric)) %>% 
    colnames()
  
  return(list(demographics = demographics, vars_numeric = vars_numeric))
}
