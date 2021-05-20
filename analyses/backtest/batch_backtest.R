
# set years to backtest
years <- 2003:2016

# set lag between time1 and time2
lag <- 2

# run the scripts
for (time1 in years){
  
  # remove global variables created in previous loop
  rm(list = setdiff(ls(), c("years", "lag", "time1")))
  
  try({
    # set time2 based on lag
    time2 <- time1 + lag
  
    # create subfolders
    time_file_path <- file.path('analyses', 'backtest', paste0(time1, '_', time2))
    dir.create(time_file_path)
    dir.create(file.path(time_file_path, "plots"))
    dir.create(file.path(time_file_path, "data"))
  
    # run the scripts
    source(file.path('analyses', 'matching', 'matching_mahalanobis.R')) #TODO: this calls demographics.R; should separate
    source(file.path('analyses', 'clustering.R'))
  })
}
