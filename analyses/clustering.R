library(tidyverse)
library(TraMineR)
library(fastcluster)
library(sequenchr)
source('analyses/plots/ggplot_settings.R')
set.seed(44)

# read in ATUS data
atus_raw <- read_tsv("data/atus_30min.tsv")

# read in the demographics data
demographics <- read_delim(file = "data/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE)

# read in the matches
demographics_t1 <- read_csv(file = 'data/matched_treatment_mahalanobis.csv')
demographics_t2_raw <- read_csv(file = 'data/matched_control_mahalanobis.csv')
demographics_t2 <- distinct(demographics_t2_raw, across(-pair_id))

# create treated and control for sequences
atus_t1 <- atus_raw[atus_raw$ID %in% demographics_t1$ID,]
atus_t2 <- atus_raw[atus_raw$ID %in% demographics_t2$ID,]

# widen the sequence data
atus_t1 <- atus_t1 %>% 
  pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  arrange(ID)
atus_t2 <- atus_t2 %>% 
  pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  arrange(ID)


# traminer ----------------------------------------------------------------

# define alphabet as all unique states
alphabet <- atus_t1[,-1] %>% unlist() %>% unique() %>% sort()
labels <- c("Care_HH", "Care_NHH", "Cons_Pur", "Eat_drink", "Edu", 
            "HH_activ", "Other", "Prsl_care", "Care_svcs", "Rel_spirit", 
            "Sleep", "Leisure", "Recreation", "Volunteer", "Work")

# create state sequence object
atus_seq_t1 <- seqdef(
  data = atus_t1[, -1], 
  alphabet = alphabet, 
  id = atus_t1$ID,
  labels = labels,
  xtstep = 1)
atus_seq_t2 <- seqdef(
  data = atus_t2[, -1], 
  alphabet = alphabet, 
  id = atus_t2$ID,
  labels = labels,
  xtstep = 1)

# sequenchr::launch_sequenchr(atus_seq_t1)


# clustering --------------------------------------------------------------

# calculate transition rates
# TRATE_t1 <- seqtrate(atus_seq_t1)

# calculate substitution cost from time1
TRATE_cost <- seqsubm(atus_seq_t1, method = 'TRATE') #, time.varying = TRUE

# compute distances
dist_t1 <- seqdist(atus_seq_t1, method = "OM", indel = 1, sm = TRATE_cost)
dist_t2 <- seqdist(atus_seq_t2, method = "OM", indel = 1, sm = TRATE_cost)
# TODO: can/should TRATE vary by time of day???
# TODO: do levenshtein difference to see

# cluster the data
cluster_model_t1 <- fastcluster::hclust(as.dist(dist_t1), method = "ward.D2")
cluster_model_t2 <- fastcluster::hclust(as.dist(dist_t2), method = "ward.D2")
# plot(cluster_model_t1)
# plot(cluster_model_t2)
# TODO: do wards or PAM (similar to kmeans; also called agnes)

# optimize k
# TODO: do this separately between t1 and t2
k_range <- c(2, 10)
stats_t1 <- sequenchr::cluster_stats(dist_t1, cluster_model_t1, k_range[1], k_range[2])
stats_t2 <- sequenchr::cluster_stats(dist_t2, cluster_model_t2, k_range[1], k_range[2])
bind_rows(
  stats_t1 %>% mutate(time = 't1') %>% pivot_longer(c("ch_norm", "silhouette_norm")),
  stats_t2 %>% mutate(time = 't2') %>% pivot_longer(c("ch_norm", "silhouette_norm"))
) %>% 
  mutate(name = recode(name, 
                       'ch_norm' = "Calinski and Harabasz index",
                       'silhouette_norm' = 'Silhouette width')) %>%
  ggplot(aes(x = k, y = value, color = time, linetype = name)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = k_range[1]:k_range[2]) +
  labs(title = "Cluster validity statistics",
       subtitle = 'Maximum values == optimal number of clusters',
       x = 'n clusters',
       y = 'Normalized index',
       color = NULL,
       linetype = NULL) +
  ggplot2::theme(legend.position = 'bottom')

# set the cluster labels
k_t1 <- 5
k_t2 <- 5
sequenchr::plot_dendrogram(cluster_model_t1, k_t1, 50) + labs(subtitle = "Time 1")
sequenchr::plot_dendrogram(cluster_model_t2, k_t2, 50) + labs(subtitle = "Time 2")
clusters_t1 <- sequenchr::label_clusters(cluster_model_t1, k_t1)
clusters_t2 <- sequenchr::label_clusters(cluster_model_t2, k_t2)


# demographics by cluster -------------------------------------------------

# create dataframe denoting ID, cluster assignment, and time period
cluster_assignments <- tibble(
  ID = c(demographics_t1$ID, demographics_t2$ID),
  cluster = c(as.character(clusters_t1), as.character(clusters_t2)),
  time = c(rep('t1', length(clusters_t1)), rep('t2', length(clusters_t2))),
)

# age by cluster and time
cluster_assignments %>% 
  left_join(demographics[, c('ID', 'age')], by = 'ID') %>% 
  mutate(cluster = str_extract(cluster, "Cluster \\d")) %>% 
  ggplot(aes(x = age)) +
  geom_bar(aes(y = ..prop..)) +
  # geom_histogram() +
  facet_grid(time~cluster)


# transitions between clusters --------------------------------------------

# relative cluster sizes
table(clusters_t1) / sum(table(clusters_t1))
table(clusters_t2) / sum(table(clusters_t2))

# create dataframe indicating the cluster assignment and pair_id
pair_ids <- tibble(
  ID = c(demographics_t1$ID, demographics_t2_raw$ID),
  pair_id = c(demographics_t1$pair_id, demographics_t2_raw$pair_id),
)
cluster_pairs <- left_join(pair_ids, cluster_assignments, by = 'ID')

# how consistent are the clusters across treatment and control?
cluster_pairs_wide <- cluster_pairs %>% 
  mutate(cluster = str_extract(cluster, "Cluster \\d")) %>%
  pivot_wider(id_cols = pair_id, values_from = cluster, names_from = time)
transition_rate <- cluster_pairs_wide %>% 
  group_by(t1) %>% 
  summarize(cluster_transition = list(table(t2))) %>% 
  unnest_longer(col = cluster_transition) %>%
  select(t1, t2 = cluster_transition_id, n = cluster_transition) %>%
  mutate(n = as.numeric(n),
         rate = n / sum(n)) %>%
  group_by(t1) %>%
  mutate(rate_group = n / sum(n)) %>%
  ungroup()

transition_table <- table(t1 = cluster_pairs_wide$t1, t2 = cluster_pairs_wide$t2)


# how often do matches stay in the same cluster?
mean(cluster_pairs_wide$t1 == cluster_pairs_wide$t2)

# transition matrix
transition_rate %>% 
  ggplot(aes(x = t1, y = t2, fill = rate_group, label = round(rate_group, 2))) +
  geom_tile() +
  geom_text(color = 'grey70') +
  labs(title = "Transition matrix between clusters",
       x = "\nCluster in time 1",
       y = "Cluster in time 2",
       fill = "Transition rate") +
  theme(axis.text.x = element_text(angle = -20, hjust = 0))

# transition rate by cluster
ggplot(transition_rate, aes(x = cluster_from, y = rate_group, fill = cluster_to)) +
  geom_col() +
  scale_x_continuous(breaks = 1:k_t1) +
  labs(title = "Transition from cluster X to cluster Y",
       x = "From cluster X",
       y = "Transition rate from cluster X",
       fill = "To cluster Y")

