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
demographics_treated <- read_csv(file = 'data/matched_treatment_mahalanobis.csv')
demographics_control_raw <- read_csv(file = 'data/matched_control_mahalanobis.csv')
demographics_control <- distinct(demographics_control_raw, across(-pair_id))

# create treated and control for sequences
atus_treated <- atus_raw[atus_raw$ID %in% demographics_treated$ID,]
atus_control <- atus_raw[atus_raw$ID %in% demographics_control$ID,]

# widen the sequence data
atus_treated <- atus_treated %>% 
  pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  arrange(ID)
atus_control <- atus_control %>% 
  pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  arrange(ID)


# traminer ----------------------------------------------------------------

# define alphabet as all unique states
alphabet <- atus_treated[,-1] %>% unlist() %>% unique() %>% sort()
labels <- c("Care_HH", "Care_NHH", "Cons_Pur", "Eat_drink", "Edu", 
            "HH_activ", "Other", "Prsl_care", "Care_svcs", "Rel_spirit", 
            "Sleep", "Leisure", "Recreation", "Volunteer", "Work")

# create state sequence object
atus_seq_treated <- seqdef(
  data = atus_treated[, -1], 
  alphabet = alphabet, 
  id = atus_treated$ID,
  labels = labels,
  xtstep = 1)
atus_seq_control <- seqdef(
  data = atus_control[, -1], 
  alphabet = alphabet, 
  id = atus_control$ID,
  labels = labels,
  xtstep = 1)

# sequenchr::launch_sequenchr(atus_seq_treated)


# manual clustering -------------------------------------------------------

# compute optimal matching distances
dist_treated <- seqdist(atus_seq_treated, method = "OM", sm = "TRATE")
dist_control <- seqdist(atus_seq_control, method = "OM", sm = "TRATE")
# TODO: does the TRATE vary by time of day???
# TODO: do levenshtein difference to see


# cluster the data
cluster_model_treated <- fastcluster::hclust(as.dist(dist_treated), method = "ward.D2")
cluster_model_control <- fastcluster::hclust(as.dist(dist_control), method = "ward.D2")
# plot(cluster_model_treated)
# plot(cluster_model_control)
# TODO: do wards or PAM (similar to kmeans; also called agnes)

# optimize k
# TODO: do this separately
k_range <- c(2, 10)
stats_treated <- sequenchr::cluster_stats(dist_treated, cluster_model_treated, k_range[1], k_range[2])
stats_control <- sequenchr::cluster_stats(dist_control, cluster_model_control, k_range[1], k_range[2])
bind_rows(
  stats_treated %>% mutate(treatment = 'Treatment') %>% pivot_longer(c("ch_norm", "silhouette_norm")),
  stats_control %>% mutate(treatment = 'Control') %>% pivot_longer(c("ch_norm", "silhouette_norm"))
) %>% 
  mutate(name = recode(name, 
                       'ch_norm' = "Calinski and Harabasz index",
                       'silhouette_norm' = 'Silhouette width')) %>%
  ggplot(aes(x = k, y = value, color = treatment, linetype = name)) +
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

k <- 6
clusters_treated <- stats::cutree(cluster_model_treated, k = k)
clusters_control <- stats::cutree(cluster_model_control, k = k)


# demographics by cluster -------------------------------------------------



# transitions between clusters --------------------------------------------

# create dataframe indicating the cluster assignment and pair_id
cluster_assignments <- tibble(
  ID = c(demographics_treated$ID, demographics_control$ID),
  cluster = c(clusters_treated, clusters_control),
  treatment = c(rep(TRUE, length(clusters_treated)), rep(FALSE, length(clusters_control))),
)
pair_ids <- tibble(
  ID = c(demographics_treated$ID, demographics_control_raw$ID),
  pair_id = c(demographics_treated$pair_id, demographics_control_raw$pair_id),
)
cluster_assignments <- left_join(pair_ids, cluster_assignments, by = 'ID')

# how consistent are the clusters across treatment and control?
transition_rate <- cluster_assignments %>% 
  pivot_wider(id_cols = pair_id, values_from = cluster, names_from = treatment) %>%
  rename(cluster_from = `TRUE`, cluster_to = `FALSE`) %>% 
  group_by(cluster_from) %>% 
  summarize(cluster_transition = list(table(cluster_to))) %>% 
  unnest_longer(col = cluster_transition) %>%
  select(cluster_from, cluster_to = cluster_transition_id, n = cluster_transition) %>%
  mutate(n = as.numeric(n),
         rate = n / sum(n)) %>%
  group_by(cluster_from) %>%
  mutate(rate_group = n / sum(n)) %>%
  ungroup()

# transition matrix
transition_rate %>% 
  ggplot(aes(x = cluster_from, y = cluster_to, fill = rate, label = round(rate, 2))) +
  geom_tile() +
  geom_text(color = 'grey70') +
  scale_x_continuous(breaks = 1:k) +
  labs(title = "Transition matrix between clusters",
       x = "From cluster",
       y = "To cluster",
       fill = "Transition rate")

# transition rate by cluster
ggplot(transition_rate, aes(x = cluster_from, y = rate_group, fill = cluster_to)) +
  geom_col() +
  scale_x_continuous(breaks = 1:k) +
  labs(title = "Transition from cluster X to cluster Y",
       x = "From cluster X",
       y = "Transition rate from cluster X",
       fill = "To cluster Y")

