###
# This script performs the sequence analysis and related clustering
###

# no matching for this clustering


library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(TraMineR)
library(fastcluster)
# library(sequenchr)
set.seed(44)

# functions
purrr:::walk(list.files('R', full.names = TRUE), source)

# read in ATUS data
atus_raw <- readr::read_tsv(file.path("data", "atus_30min_SSC.tsv"))
atus_raw <- atus_raw |> dplyr::select(-location)

# read in the demographics data
demographics <- readr::read_delim(file = file.path("data", "demographic.tsv"),
                                  delim = "\t",
                                  escape_double = FALSE,
                                  trim_ws = TRUE,
                                  col_types = readr::cols(metropolitan = readr::col_character()))

# sensitivity: filter to just couples
demographics <- demographics |> filter(has_partner)


# population --------------------------------------------------------------

# years to backtest
years1 <- 2004:2020
years2 <- years1 + 1

# run backtest of clustering
clustering_results <- local({
  
  # read in data with diary date
  atusresp_0320 <- readr::read_csv(file.path("inputs", "ATUS-2003-2021", "atusresp_0321.dat"))
  
  # regions for matching
  state_regions <- readr::read_csv(file.path("inputs", "state_regions.csv"))
  
  # cluster the data for each year
  clustering_result <- purrr::map2(years1, years2, function(year1, year2){
    
    cli::cli_alert('On year {year1}:{year2}')
    
    # prep the data
    atus_wide <- sequence_prep_atus(year1, year2, atusresp_0320, state_regions, demographics)
    
    # cluster the sequences
    sequences_clustered <- cluster_sequences(
      atus_wide = atus_wide, 
      year1 = year1,
      year2 = year2,
      method = 'LCS', 
      k_range = c(2, 10), 
      cluster_algo = 'hclust', 
      cluster_method = 'ward.D2', 
      include_plots = TRUE
    )
    
    # store the years
    sequences_clustered$years$year1 <- year1
    sequences_clustered$years$year2 <- year2
    
    return(sequences_clustered)
  })
  
  return(clustering_result)
})
names(clustering_results) <- glue::glue('{years1}:{years2}')

# save the results
saveRDS(clustering_results, 'outputs/SA/backtest_2021.rds')
# saveRDS(clustering_results, 'outputs/SA/backtest_2020_couples.rds') # sensitivity
# clustering_results <- readRDS('outputs/SA/backtest.rds')
# clustering_results <- readRDS('outputs/SA/backtest_2021.rds')


# post-hoc analysis -------------------------------------------------------

# sequences_clustered <- clustering_results$`2018:2019`
sequences_clustered <- clustering_results$`2019:2020`
# sequences_clustered <- clustering_results$`2020:2021`
clusters <- sequences_clustered$clusters
years <- sequences_clustered$clusters$year %>% unique() %>% sort()
year1 <- sequences_clustered$years$year1
year2 <- sequences_clustered$years$year2


# set labels for SeqI x axis: formatted as time
labels_x <- as.character(seq(2, 24, by = 2))
labels_x <- c(labels_x[2:12], labels_x[1:2])
labels_x[1] <- "4am"
breaks_x <- seq(0, 48, by = 4)

# establish color mapping
unique_states <- sort(unique(atus_raw$description))
color_mapping <- viridis::viridis_pal()(length(unique_states))
names(color_mapping) <- unique_states

# create second color mapping with extraneous tokens greyed out
color_mapping_grey <- c('grey20', '#d9d955', '#db324b', 'grey55', '#ffffff')
names(color_mapping_grey) <- c('Sleep', 'Work w/o SCC', 'Work with SCC', 'Other activities', 'filler')

# seqI plot sorted amount of work
yellow_labels <- c('Work : No SCC', 'Work : SCC')
seqI_groups <- atus_raw %>% 
  right_join(clusters, by = 'ID') %>% 
  group_by(ID) %>% 
  mutate(entropy = sum(description %in% yellow_labels)) %>% 
  ungroup() %>% 
  mutate(description = case_when(
    description == 'Sleep : No SCC' ~ 'Sleep',
    description == 'Work : No SCC' ~ 'Work w/o SCC',
    description == 'Work : SCC' ~ 'Work with SCC',
    TRUE ~ 'Other activities'))

# basic seqD
seqI_groups %>% 
  group_by(cluster, year) |> 
  mutate(n = n_distinct(ID)) |> 
  group_by(year) |> 
  mutate(percent = n / sum(unique(n)), # hack
         cluster = stringr::str_extract(cluster, "(.*?)\\|"),
         label = glue::glue('{cluster}  n={n}  |  pct={scales::percent(percent)}')) |> 
  ungroup() |> 
  ggplot(ggplot2::aes(x = period, fill = description)) +
  geom_bar(width = 1) +
  # geom_tile() +
  scale_fill_manual(values = color_mapping_grey[names(color_mapping_grey) != 'filler']) +
  scale_x_continuous(breaks = breaks_x, labels = labels_x) +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  facet_wrap(year~label, scales = 'free_y') +
  labs(title = "State distributions of the clusters",
       subtitle = 'Activities simplified into the four categories shown',
       x = NULL,
       y = 'Frequency',
       fill = NULL) +
  theme(legend.position = 'bottom')
# ggsave(file.path('outputs', 'SA', glue::glue("seqd_{year2}.png")), height = 6, width = 9)
# ggsave(file.path('outputs', 'SA', glue::glue("seqd_{year2}_couples.png")), height = 6, width = 9)

# 2020 seqD by gender -- resampled so each cluster has 1000 individuals 
# but maintains between sex split within cluster
resampled_seq <- local({
  
  # create filler rows for white-space on plot
  n_filler_rows <- 50
  cluster_names <- seqI_groups %>% filter(year == year2) %>% pull(cluster) %>% unique()
  filler_rows <- tibble(
    sex = 'filler',
    year = year2,
    cluster = rep(cluster_names, n_filler_rows),
    ID = as.numeric(paste0(999, 1:(n_filler_rows * length(cluster_names)))),
    entropy = 1,
  )
  
  # filler sequence data
  filler_seq <- crossing(
    period =  unique(seqI_groups$period),
    ID = filler_rows$ID,
    description = 'filler'
  )
  
  # resample
  resampled <- seqI_groups %>%
    filter(year == year2) %>%
    distinct(ID, cluster, year) %>%
    left_join(select(demographics, ID, survey_weight, sex), by = 'ID') %>%
    left_join(seqI_groups %>% distinct(ID, entropy), by = 'ID') %>%
    mutate(sex = recode(sex, `1` = 'male', `2` = 'female')) %>%
    group_by(year, cluster) %>%
    slice_sample(n = 1000, weight_by = survey_weight, replace = TRUE) %>% 
    bind_rows(filler_rows) %>%
    mutate(sex = factor(sex, levels = c('female', 'filler', 'male'))) %>% 
    group_by(year, cluster) %>%
    arrange(sex, entropy) %>% 
    mutate(ID_resampled = row_number()) %>%
    ungroup()
  
  # resampled %>% arrange(cluster, sex, ID_resampled) %>% View()

  # join to get activities
  resampled_seq <- resampled %>%
    left_join(seqI_groups %>% 
                select(ID, period, description, entropy) %>% 
                bind_rows(filler_seq),
              by = 'ID') %>%
    mutate(cluster = stringr::str_sub(cluster, 1, 9))

  return(resampled_seq)
})

# plot it
resampled_seq %>%
  ggplot(ggplot2::aes(x = period, y = ID_resampled, fill = description)) +
  geom_tile() +
  scale_fill_manual(values = color_mapping_grey) +
  scale_x_continuous(breaks = breaks_x, labels = labels_x) +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  facet_wrap(~cluster, scales = 'free_y') +
  labs(title = glue::glue("{year2} sequences split by cluster and sex"),
       subtitle = 'Each cluster resampled with n = 1,000',
       x = NULL, 
       y = "Respondent", 
       fill = NULL) +
  theme(legend.position = 'bottom')
# ggsave(file.path('outputs', 'SA', glue::glue("seqi_{year2}.png")), height = 6, width = 9)
# ggsave(file.path('outputs', 'SA', glue::glue("seqi_{year2}_couples.png")), height = 6, width = 9)

# how many people are in each cluster:quarter
# clusters %>% 
#   left_join(atusresp_0321 %>% select(TUCASEID, TUDIARYDATE),
#             by = c('ID' = 'TUCASEID')) %>% 
#   mutate(diary_date = lubridate::ymd(TUDIARYDATE),
#          diary_month = lubridate::month(diary_date),
#          quarter = ceiling(diary_month / 3),
#          quarter = as.Date(paste0(year, '-', quarter * 3, '-01')),
#          quarter = quarter + months(1) - 1,
#          year = lubridate::year(diary_date)) %>% 
#   filter(year == 2021) %>% 
#   group_by(cluster, quarter) %>% 
#   tally() %>% 
#   ggplot(aes(x = cluster, y = n)) +
#   geom_col() +
#   facet_wrap(~quarter)

# percent female in each cluster
clusters |> 
  left_join(demographics |> select(ID, sex), by = 'ID') |> 
  group_by(cluster, year) |> 
  summarize(percent_female = mean(sex == 2))
