###
# This script performs the sequence analysis and related clustering
# work-in-progress
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

# read in the demographics data
demographics <- readr::read_delim(file = file.path("data", "demographic.tsv"),
                                  delim = "\t",
                                  escape_double = FALSE,
                                  trim_ws = TRUE,
                                  col_types = readr::cols(metropolitan = readr::col_character()))


# population --------------------------------------------------------------

year1 <- 2019
year2 <- 2020

# convert atus data to wide for traminer
atus_wide <- local({
  
  # read in data with diary date
  atusresp_0320 <- readr::read_csv(file.path("inputs", "ATUS-2003-2020", "atusresp_0320.dat"))
  
  # regions for matching
  state_regions <- readr::read_csv("inputs/state_regions.csv")
  
  # NOTE: this is only for plots; must adjust code in matching_prep_demographics() and matching_mahalanobis()
  # if you want to change the matching and blocking vars
  matching_vars <- c('age', 'sex', 'race', 'fam_income', 'has_partner', 
                     'education', 'child_in_HH', 'n_child', 'age_youngest', 'region', 
                     'partner_working', 'elder_in_HH', 'metropolitan')#, 'labor_force_status')
  # blocking_vars <- c('sex', 'race', 'metropolitan', 'region', 'has_partner', 'labor_force_status') #'essential_worker # add child_in_HH?
  blocking_vars <- c('sex', 'race') #, 'has_partner') #'essential_worker
  
  # clean the demographics data
  demographics_prepped <- matching_prep_demographics(atusresp_0320, demographics, state_regions, year1, year2, matching_vars)
  
  # filter the ATUS data to just the this population
  year1_IDs <- demographics_prepped$demographics %>% 
    filter(year == year1) %>% 
    pull(ID)
  year2_IDs <- demographics_prepped$demographics %>% 
    filter(year == year2) %>% 
    pull(ID)
  
  atus_t1 <- atus_raw %>% 
    filter(ID %in% year1_IDs) %>% 
    pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
    arrange(ID)
  
  atus_t2 <- atus_raw %>% 
    filter(ID %in% year2_IDs) %>% 
    pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
    arrange(ID)
  
  return(list(t1 = atus_t1, t2 = atus_t2))
})

# cluster the sequences
sequences_clustered <- cluster_sequences(
  atus_wide, 
  method = 'LCS', 
  k_range = c(2, 10), 
  cluster_algo = 'hclust', 
  cluster_method = 'ward.D2', 
  include_plots = TRUE
)


# post-hoc ----------------------------------------------------------------

clusters <- tibble(
  ID = c(atus_wide$t1$ID, atus_wide$t2$ID),
  cluster = c(sequences_clustered$clusters$t1, sequences_clustered$clusters$t2),
  year = c(rep(year1, length(sequences_clustered$clusters$t1)),
           rep(year2, length(sequences_clustered$clusters$t2)))
)



# establish color mapping
unique_states <- sort(unique(atus_raw$description))
color_mapping <- viridis::viridis_pal()(length(unique_states))
names(color_mapping) <- unique_states

# create second color mapping with extraneous tokens greyed out
color_mapping_grey <- c('grey20', '#d9d955', '#db324b', 'grey55')
names(color_mapping_grey) <- c('Sleep', 'Work w/o SCC', 'Work with SCC', 'Other activities')

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
  ggplot(ggplot2::aes(x = period, fill = description)) +
  geom_bar(width = 1) +
  # geom_tile() +
  scale_fill_manual(values = color_mapping_grey) +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  facet_wrap(year~cluster, scales = 'free_y') +
  labs(title = "All sequences sorted by entropy",
       x = 'Period',
       y = 'Sequence',
       fill = NULL) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 5))
# ggsave(file.path('outputs', 'plots', "seqd_2020.png"), height = 6, width = 9)

# basic seqI
seqI_groups %>% 
  ggplot(ggplot2::aes(x = period, y = reorder(ID, entropy), fill = description)) +
  geom_tile() +
  scale_fill_manual(values = color_mapping_grey) +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  facet_wrap(~cluster, scales = 'free_y') +
  labs(title = "All sequences sorted by entropy",
       x = 'Period',
       y = 'Sequence',
       fill = NULL) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 5))

# 2020 seqD by gender -- resampled so each cluster has 1000 individuals 
# but maintains between sex split within cluster
resampled_seq <- local({
  
  # # create filler rows for white-space on plot
  # n_filler_rows <- 50
  # cluster_names <- seqI_groups %>% filter(year == year2) %>% pull(cluster) %>% unique()
  # filler_rows <- tibble(
  #   sex = 'filler',
  #   cluster = rep(cluster_names, n_filler_rows)
  # )
  # 
  # # resample
  # resampled <- seqI_groups %>%
  #   filter(year == year2) %>%
  #   distinct(ID, cluster, year) %>%
  #   left_join(select(demographics, ID, survey_weight, sex), by = 'ID') %>%
  #   mutate(sex = recode(sex, `1` = 'male', `2` = 'female')) %>%
  #   group_by(year, cluster) %>%
  #   slice_sample(n = 1000, weight_by = survey_weight, replace = TRUE) %>%
  #   bind_rows(filler_rows) %>%
  #   mutate(ID_resampled = row_number()) %>%
  #   ungroup()
  # 
  # # join to get activities
  # resampled_seq <- resampled %>%
  #   left_join(select(seqI_groups, ID, period, description, entropy), by = 'ID') %>%
  #   mutate(cluster = stringr::str_sub(cluster, 1, 9),
  #          description = if_else(is.na(description), 'filler', description))
  # group_by(cluster, sex, ID_resampled) %>%
  # arrange(entropy) %>%
  # mutate(ID_resampled = row_number())

  return(resampled_seq)
})

# plot it
resampled_seq %>% 
  ggplot(ggplot2::aes(x = period, y = reorder(ID_resampled, entropy), fill = description)) +
  geom_tile() +
  scale_fill_manual(values = color_mapping_grey) +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  facet_wrap(~cluster, scales = 'free_y') +
  labs(title = "2020 sequences split by cluster and sex",
       subtitle = 'Each cluster resampled with n = 1,000',
       x = NULL, 
       y = "Respondent", 
       fill = NULL) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 5))
