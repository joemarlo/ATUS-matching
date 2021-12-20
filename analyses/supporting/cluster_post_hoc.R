library(dplyr)
library(ggplot2)
library(TraMineR)
source('analyses/plots/ggplot_settings.R')
set.seed(44)

# read in ATUS data
# atus_raw <- read_tsv(file.path("data", "atus_30min.tsv"))
atus_raw <- readr::read_tsv(file.path("data", "atus_30min_SSC.tsv")) # tokens with supplemental child care

# read in cluster memberships (unmatched)
clusters <- readr::read_csv("outputs/final_clusters/default/2019_2020/data/cluster_pairs.csv")
dirs <- paste0(2004:2019, "_", 2005:2020)
clusters_all <- purrr::map_dfr(dirs, function(year){
  dir <- paste0("outputs/final_clusters/default/", year, "/data/cluster_pairs.csv")
  clusters <- readr::read_csv(dir)
  clusters$year <- year
  return(clusters)
})

# read in activity file
atusact_0320 <- readr::read_csv("inputs/ATUS-2003-2020/atusact_0320.dat")

# read in the demographics data
demographics <- readr::read_delim(
  file = file.path("data", "demographic.tsv"),
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE,
  col_types = readr::cols(metropolitan = readr::col_character())
)

# set labels for SeqI x axis: formatted as time
labels_x <- as.character(seq(2, 24, by = 2))
labels_x <- c(labels_x[2:12], labels_x[1:2])
labels_x[1] <- "4am"
breaks_x <- seq(0, 48, by = 4)


# traminer ----------------------------------------------------------------

# create time1 and time2 dataframes for sequences
atus_t1 <- atus_raw[atus_raw$ID %in% clusters[clusters$time == 't1',]$ID,]
atus_t2 <- atus_raw[atus_raw$ID %in% clusters[clusters$time == 't2',]$ID,]

# widen the sequence data
atus_t1 <- atus_t1 %>% 
  tidyr::pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  arrange(ID)
atus_t2 <- atus_t2 %>% 
  tidyr::pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  arrange(ID)

# define alphabet as all unique states
alphabet <- atus_t1[,-1] %>% unlist() %>% unique() %>% sort()

# create state sequence object
atus_seq_t1 <- seqdef(
  data = atus_t1[, -1], 
  alphabet = alphabet, 
  id = atus_t1$ID,
  xtstep = 1)
atus_seq_t2 <- seqdef(
  data = atus_t2[, -1], 
  alphabet = alphabet, 
  id = atus_t2$ID,
  xtstep = 1)


# seqI by work ------------------------------------------------------------

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
  mutate(entropy = sum(description %in% yellow_labels),
         year = case_when(time == 't1' ~ '2019',
                          time == 't2' ~ '2020',
                          TRUE ~ 'NA')) %>% 
  ungroup() %>% 
  mutate(description = case_when(
    description == 'Sleep : No SCC' ~ 'Sleep',
    description == 'Work : No SCC' ~ 'Work w/o SCC',
    description == 'Work : SCC' ~ 'Work with SCC',
    TRUE ~ 'Other activities'))
seqI_groups %>% 
  ggplot(aes(x = period, 
             y = stats::reorder(ID, entropy), 
             fill = description)) + 
  geom_tile() + 
  # scale_fill_manual(values = color_mapping) +
  scale_fill_manual(values = color_mapping_grey) +
  scale_x_continuous(breaks = breaks_x, labels = labels_x) +
  scale_y_discrete(labels = NULL, breaks = NULL) + 
  facet_wrap(year~cluster, scales = "free_y", ncol = 3) + 
  labs(title = "All sequences by cluster sorted by count of work activities", 
       x = NULL, 
       y = "Sequence", 
       fill = NULL) +
  theme(axis.text.x = element_text(size = 7))
# ggsave(file.path('outputs', 'plots', "seqi_grey_1920.png"), height = 6, width = 9)

# same seqI plot but split by sex
year_ <- 2020
seqI_groups_year <- seqI_groups %>% 
  filter(year == year_) %>% 
  left_join(select(demographics, ID, sex), by = 'ID') %>%
  group_by(cluster, sex) %>% 
  mutate(n = n_distinct(ID)) %>% 
  ungroup() %>%
  mutate(sex = if_else(sex == 1, 'Male', 'Female'),
         cluster = paste0(stringr::str_sub(cluster, 1, 9),
                          ' | n = ',
                          n)
         )
seqI_groups_year %>% 
  ggplot(aes(x = period, 
             y = stats::reorder(ID, entropy), 
             fill = description)) + 
  geom_tile() + 
  scale_fill_manual(values = color_mapping_grey) +
  scale_x_continuous(breaks = breaks_x, labels = labels_x) +
  scale_y_discrete(labels = NULL, breaks = NULL) + 
  facet_wrap(sex~cluster, scales = "free_y", ncol = 3) + 
  labs(title = "All sequences by cluster sorted by count of work activities", 
       subtitle = paste0(year_, ' only'),
       x = NULL, 
       y = "Sequence", 
       fill = NULL) +
  theme(axis.text.x = element_text(size = 7))


# take one at resampling
resampled <- seqI_groups %>% 
  distinct(ID, cluster, year) %>% 
  left_join(select(demographics, ID, survey_weight), by = 'ID') %>% 
  group_by(year, cluster) %>% 
  slice_sample(n = 1000, weight_by = survey_weight, replace = TRUE) %>% 
  ungroup() %>% 
  transmute(ID, ID_resampled = row_number())
resampled %>% 
  left_join(seqI_groups, by = 'ID') %>% 
  # filter(year == year_) %>%
  mutate(cluster = stringr::str_sub(cluster, 1, 9)) %>% 
  ggplot(aes(x = period, 
             y = stats::reorder(ID_resampled, entropy), 
             fill = description)) + 
  geom_tile() + 
  # scale_fill_manual(values = color_mapping) +
  scale_fill_manual(values = color_mapping_grey) +
  scale_x_continuous(breaks = breaks_x, labels = labels_x) +
  scale_y_discrete(labels = NULL, breaks = NULL) + 
  facet_wrap(year~cluster, scales = "free_y", ncol = 3) + 
  labs(title = "TBD title", 
       x = NULL, 
       y = "Respondent", 
       fill = NULL,
       caption = 'Each cluster resampled with n = 1,000') +
  theme(axis.text.x = element_text(size = 7))
# ggsave(file.path('outputs', 'plots', "seqi_grey_1920_resampled.png"), height = 6, width = 9)

# split 2020 by sex
resampled %>% 
  left_join(seqI_groups, by = 'ID') %>% 
  filter(year == 2020) %>%
  mutate(cluster = stringr::str_sub(cluster, 1, 9)) %>% 
  left_join(dplyr::select(demographics, ID, sex), by = 'ID') %>% 
  mutate(sex = if_else(sex == 1, 'Male', 'Female')) %>% 
  ggplot(aes(x = period, 
             y = stats::reorder(ID_resampled, entropy), 
             fill = description)) + 
  geom_tile() + 
  # scale_fill_manual(values = color_mapping) +
  scale_fill_manual(values = color_mapping_grey) +
  scale_x_continuous(breaks = breaks_x, labels = labels_x) +
  scale_y_discrete(labels = NULL, breaks = NULL) + 
  facet_wrap(sex~cluster, scales = "free_y", ncol = 3) + 
  labs(title = "TBD title: 2020", 
       x = NULL, 
       y = "Respondent", 
       fill = NULL,
       caption = 'Each cluster resampled with n = 1,000') +
  theme(axis.text.x = element_text(size = 7))
# ggsave(file.path('outputs', 'plots', "seqi_grey_1920_resampled_by_sex.png"), height = 6, width = 9)


# take one.five: resample with n = proportions between clusters
resampled_prop <- seqI_groups %>% 
  distinct(ID, cluster, year, entropy) %>% 
  mutate(cluster = stringr::str_sub(cluster, 1, 9)) %>% 
  filter(year == year_) %>%
  left_join(select(demographics, ID, survey_weight), by = 'ID') %>% 
  group_by(cluster) %>%
  mutate(n = n()) %>% 
  group_modify(~ {
    .n <- .x$n[1] * 5
    slice_sample(.x, n = .n, weight_by = survey_weight, replace = TRUE)
  }) %>% 
  arrange(desc(entropy)) %>% 
  mutate(ID_resampled = row_number()) %>% 
  ungroup() %>% 
  select(ID, ID_resampled)
resampled_prop %>% 
  left_join(seqI_groups, by = 'ID') %>% 
  mutate(cluster = stringr::str_sub(cluster, 1, 9)) %>% 
  ggplot(aes(x = period, 
             y = -ID_resampled, 
             fill = description)) + 
  geom_tile() + 
  # scale_fill_manual(values = color_mapping) +
  scale_fill_manual(values = color_mapping_grey) +
  scale_x_continuous(breaks = breaks_x, labels = labels_x) +
  scale_y_discrete(labels = NULL, breaks = NULL) + 
  facet_wrap(~cluster, ncol = 3) + 
  labs(title = "TBD title", 
       x = NULL, 
       y = "Respondent", 
       fill = NULL,
       caption = 'Each cluster resampled with n = 1,000') +
  theme(axis.text.x = element_text(size = 7))


# take two at resampling: 
# resample within each group but maintain group-to-group proportions
# this maintains within cluster gender split
resampled_prop <- seqI_groups_year %>%
  distinct(ID, cluster, sex, n, entropy) %>% 
  mutate(cluster = stringr::str_sub(cluster, 1, 9)) %>% 
  left_join(select(demographics, ID, survey_weight), by = 'ID') %>% 
  group_by(cluster, sex) %>% 
  group_modify(~ {
    .n <- .x$n[1] * 5
    slice_sample(.x, n = .n, weight_by = survey_weight, replace = TRUE)
    }) %>% 
  group_by(cluster, sex) %>% 
  arrange(desc(entropy)) %>% 
  mutate(ID_resampled = row_number()) %>% 
  ungroup() %>% 
  arrange(cluster, sex, entropy) %>% 
  select(ID, ID_resampled)
resampled_prop %>% 
  left_join(seqI_groups_year, by = 'ID') %>%
  mutate(cluster = stringr::str_sub(cluster, 1, 9)) %>% 
  ggplot(aes(x = period, 
             y = -ID_resampled, 
             fill = description)) + 
  geom_tile() + 
  # scale_fill_manual(values = color_mapping) +
  scale_fill_manual(values = color_mapping_grey) +
  scale_x_continuous(breaks = breaks_x, labels = labels_x) +
  scale_y_discrete(labels = NULL, breaks = NULL) + 
  facet_grid(sex~cluster) + 
  labs(title = "TBD title", 
       x = NULL, 
       y = "Respondent", 
       fill = NULL,
       caption = 'Each cluster resampled') +
  theme(axis.text.x = element_text(size = 7),
        legend.position = 'bottom')

# this more closely honors survey weights and does not maintain within cluster gender split 
resampled_prop_sex <- seqI_groups_year %>%
  distinct(ID, cluster, sex, entropy) %>% 
  mutate(cluster = stringr::str_sub(cluster, 1, 9)) %>% 
  left_join(select(demographics, ID, survey_weight), by = 'ID') %>% 
  group_by(cluster) %>% 
  mutate(n = n()) %>% 
  group_modify(~ {
    .n <- .x$n[1] * 5
    slice_sample(.x, n = .n, weight_by = survey_weight, replace = TRUE)
  }) %>% 
  group_by(cluster, sex) %>% 
  arrange(desc(entropy)) %>% 
  mutate(ID_resampled = row_number()) %>% 
  ungroup() %>% 
  arrange(cluster, sex, entropy) %>% 
  select(ID, ID_resampled)
resampled_prop_sex %>% 
  left_join(seqI_groups_year, by = 'ID') %>%
  mutate(cluster = stringr::str_sub(cluster, 1, 9)) %>% 
  ggplot(aes(x = period, 
             y = -ID_resampled, 
             fill = description)) + 
  geom_tile() + 
  # scale_fill_manual(values = color_mapping) +
  scale_fill_manual(values = color_mapping_grey) +
  scale_x_continuous(breaks = breaks_x, labels = labels_x) +
  scale_y_discrete(labels = NULL, breaks = NULL) + 
  facet_grid(sex~cluster) + 
  labs(title = "TBD title", 
       x = NULL, 
       y = "Respondent", 
       fill = NULL,
       caption = 'Each cluster resampled') +
  theme(axis.text.x = element_text(size = 7),
        legend.position = 'bottom')


# applying weights to get proportion of population
clusters %>% 
  transmute(ID, 
            cluster = stringr::str_sub(cluster, 1, 9),
            year = case_when(time == 't1' ~ '2019',
                             time == 't2' ~ '2020',
                             TRUE ~ 'NA')) %>% 
  left_join(select(demographics, ID, survey_weight), by = 'ID') %>% 
  group_by(cluster, year) %>% 
  summarize(cluster_weight = sum(survey_weight),
            .groups = 'drop') %>% 
  group_by(year) %>% 
  mutate(prop = cluster_weight / sum(cluster_weight)) %>% 
  ggplot(aes(x = cluster, y = prop)) +
  geom_col() + 
  facet_wrap(~year) +
  geom_text(aes(label = round(prop, 2)), 
            nudge_y = -0.03, color = 'white', fontface = 'bold') +
  labs(title = 'Re-weighted cluster proportions by year',
       x = NULL,
       y = NULL)
  


# turbulence --------------------------------------------------------------

# turbulence by cluster
turbulence_t1 <- TraMineR::seqST(atus_seq_t1)
turbulence_t2 <- TraMineR::seqST(atus_seq_t2)
turbulence_t1 <- tibble(ID = as.double(row.names(turbulence_t1)),
                        turbulence = turbulence_t1[,1])
turbulence_t2 <- tibble(ID = as.double(row.names(turbulence_t2)),
                        turbulence = turbulence_t2[,1])
turbulence_t <- left_join(clusters, 
                          bind_rows(turbulence_t1, turbulence_t2), 
                          by = 'ID')
turbulence_t %>%
  mutate(year = case_when(time == 't1' ~ '2019',
                          time == 't2' ~ '2020',
                          TRUE ~ 'NA'),
         cluster = stringr::str_extract(cluster, "Cluster .")) %>% 
  left_join(select(demographics, ID, sex), by = 'ID') %>% 
  mutate(sex = if_else(sex == 1, 'Male', 'Female')) %>% 
  group_by(cluster, year, sex) %>%
  mutate(group_mean = mean(turbulence)) %>% 
  ggplot(aes(x = turbulence, color = cluster)) +
  geom_density(size = 1.5, alpha = 0.85) +
  geom_vline(aes(xintercept = group_mean, color = cluster),
             linetype = 'dashed') +
  ggplot2::scale_color_discrete() +
  facet_wrap(sex~year, ncol = 1) +
  labs(title = 'Turbulence by cluster',
       subtitle = 'Unweighted',
       x = 'Turbulence',
       y = NULL,
       color = NULL) +
  theme(legend.position = 'bottom')
# t.test(filter(turbulence_t2, cluster == 'Cluster 1') %>% pull(turbulence),
#        filter(turbulence_t2, cluster == 'Cluster 3') %>% pull(turbulence))

## turbulence for all years
# remove duplicate years
clusters_all_distinct <- clusters_all %>%
  mutate(year_new = if_else(time == 't1', 
                            stringr::str_extract(year, '....'), 
                            stringr::str_extract(year, '....$'))) %>% 
  select(ID, cluster, year = year_new) %>% 
  group_by(ID) %>% 
  filter(row_number() == 1)

# calculate turbulence for each year
turbulence_all <- clusters_all_distinct %>% 
  group_by(year, cluster) %>% 
  group_split() %>% 
  purrr::map_dfr(function(tbl){
    atus_t <- atus_raw %>% 
      filter(ID %in% tbl$ID) %>% 
      tidyr::pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
      arrange(ID)
    atus_seq <- seqdef(
      data = atus_t[, -1], 
      alphabet = alphabet, 
      id = atus_t$ID,
      xtstep = 1)
    turbulence <- TraMineR::seqST(atus_seq)
    turbulence <- tibble(ID = as.double(row.names(turbulence)),
                         turbulence = turbulence[,1],
                         year = tbl$year[1],
                         cluster = tbl$cluster[1])
    return(turbulence)
  })

turbulence_all %>% 
  group_by(cluster, year) %>%
  mutate(group_mean = mean(turbulence),
         cluster = stringr::str_extract(cluster, "Cluster *([^\\s]+)")) %>% 
  ggplot(aes(x = turbulence, color = cluster)) +
  geom_density(size = 1.5, alpha = 0.85) +
  geom_vline(aes(xintercept = group_mean, color = cluster),
             linetype = 'dashed') +
  ggplot2::scale_color_discrete() +
  facet_wrap(~year) +
  labs(title = 'Turbulence by cluster',
       subtitle = 'Unweighted',
       x = 'Turbulence',
       y = NULL,
       color = NULL) +
  theme(legend.position = 'bottom')
  

# Is it people who are caring by younger generation and older generation?
# People that perhaps work part time or more flexibly
# how scattered is the day? Is it a confetti day? 


# where -------------------------------------------------------------------

# df to lookup plain text description from TEWHERE code
locations <- tibble(TEWHERE = c(1:21, 30:32, 89, 99), 
                    location = c(
                      "Respondent's home or yard",
                      "Respondent's workplace",
                      "Someone else's home",
                      "Restaurant or bar",
                      "Place of worship",
                      "Grocery store",
                      "Other store/mall",
                      "School",
                      "Outdoors away from home",
                      "Library",
                      "Other place",
                      "Car, truck, or motorcycle (driver)",
                      "Car, truck, or motorcycle (passenger)",
                      "Walking",
                      "Bus",
                      "Subway/train",
                      "Bicycle",
                      "Boat/ferry",
                      "Taxi/limousine service",
                      "Airplane",
                      "Other mode of transportation",
                      "Bank",
                      "Gym/health club",
                      "Post Office",
                      "Unspecified place",
                      "Unspecified mode of transportation"
                    ))

# plot the proportion of locations by cluster
clusters %>%
  mutate(year = case_when(time == 't1' ~ '2019',
                          time == 't2' ~ '2020',
                          TRUE ~ 'NA')) %>% 
  left_join(select(atusact_0320, ID = TUCASEID, TEWHERE), 
            by = 'ID') %>% 
  left_join(locations, by = 'TEWHERE') %>% 
  group_by(cluster, year, location) %>%
  tally() %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(x = prop, y = reorder(location, -prop))) +
  geom_col() +
  facet_wrap(year~cluster, ncol = 3) +
  labs(title = 'Location of activities by cluster',
       subtitle = "Can interpret as the proportion of time spent in X location for the mean respondent in that cluster", 
       x = 'Proportion of all states',
       y = NULL) +
  theme(axis.text.y = element_text(size = 7))
