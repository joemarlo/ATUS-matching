source('analyses/demographics.R')
source('analyses/helpers_analyses.R')

# this script bypasses the matching process 
# goal is to understand if there are emergent clusters when matching is not applied

# if not running in batch mode, then create a file path to the analyses subfolder
# for use when saving plots and dataframes
if (!isTRUE(get0('in_batch_mode'))) file_path <- "analyses"

# add treatment column and dummy pair_id column
demographics$treatment <- demographics$year == time1
demographics$pair_id <- NA

# create treatment and control dfs to replicate matching workflow
demographics_treated <- demographics[demographics$treatment,]
demographics_control <- demographics[!demographics$treatment,]

# write out
write_csv(demographics_treated, file = file.path(file_path, 'data', 'matched_time1_mahalanobis.csv'))
write_csv(demographics_control, file = file.path(file_path, 'data', 'matched_time2_mahalanobis.csv'))
