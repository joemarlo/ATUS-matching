
# set batch mode to TRUE as this tells the child scripts where to save files
in_batch_mode <- TRUE

# set start years to backtest
years <- 2004:2019

# set lag between time1 and time2
lag_years <- 1

# set clustering method
cluster_algo <- 'pam' # hclust # pam

# create subfolder
date_time <- paste0("batch_", gsub(" ", "_", gsub("-|:", "", Sys.time())))
path_to_batch <- file.path('analyses', 'backtest', date_time)
dir.create(path_to_batch)

# run the child scripts
for (time1 in years){
  
  # remove global variables created in previous loop
  rm(list = setdiff(ls(), c("years", "lag_years", "time1", 'path_to_batch', 'in_batch_mode', 'cluster_algo', 'date_time')))
  
  try({
    # set time2 based on lag
    time2 <- time1 + lag_years
  
    # create subfolders
    file_path <- file.path(path_to_batch, paste0(time1, '_', time2))
    dir.create(file_path)
    dir.create(file.path(file_path, "plots"))
    dir.create(file.path(file_path, "plots", "matching"))
    dir.create(file.path(file_path, "plots", "clustering"))
    dir.create(file.path(file_path, "plots", "cluster_comparison"))
    dir.create(file.path(file_path, "data"))
  
    # run the scripts
    source(file.path('analyses', 'matching', 'matching_mahalanobis.R')) # note: this calls demographics.R
    # source(file.path('analyses', 'matching', 'matching_none.R')) # this skips the matching process
    source(file.path('analyses', 'clustering.R'))
    source(file.path('analyses', 'clustering_unmatched.R'))
  })
}

# are any years missing?
created_dirs <- list.dirs(path_to_batch, recursive = FALSE, full.names = FALSE)
if (length(years) != length(created_dirs)) warning("At least one year did not successfully run")

# create the summary stats
dir.create(file.path(path_to_batch, 'plots'))
if (cluster_algo == 'hclust') source(file.path('analyses', 'backtest', 'backtest_summary_statistics.R'))

# create readme with any relevant notes
notes <- paste0('This batch was run at ', Sys.time(), ' with the following specifications:',
                '  \n\nYears: ', paste0(years, collapse = ', '),
                '  \nTime 1 to time 2 lag: ', lag_years,
                '  \nk values: float across t1 and t2',
                '  \nk search range: ', paste0(k_seq, collapse = ', '),
                '  \nDistance method: TRATE', #levenshtein
                '  \nClustering algorithm: PAM, ward.D2', #agnes, PAM, hclust
                # '  \nNo matching',
                '  \nMatching method:',
                '  \n\t-Mahalanobis distance on ', paste0(matching_vars, collapse = ', '),
                '  \n\t-Stratified on sex, race, +/- 2 age', # labor force status
                '  \n\t-Matching performed from time1 to time2 (many-to-1)',
                '  \nActivities are segmented to include/not include secondary child care')
file_connection <- file(file.path(path_to_batch, "README.md"))
writeLines(notes, file_connection)
close(file_connection)

# bash to copy elbow plots to main /plots directory
# for f in * ; do cp $f/plots/clustering/cluster_validity_pam.png plots/$f.png ; done
