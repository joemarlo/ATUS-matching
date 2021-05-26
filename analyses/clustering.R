library(tidyverse)
library(TraMineR)
library(fastcluster)
library(sequenchr)
source('analyses/plots/ggplot_settings.R')
source('analyses/helpers_analyses.R')
set.seed(44)

# read in ATUS data
atus_raw <- read_tsv("data/atus_30min.tsv")

# read in the demographics data
demographics <- read_delim(file = "data/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           col_types = cols(metropolitan = col_character()))

# read in the matches
# demographics_t1 <- read_csv(file = 'data/matched_time1_mahalanobis.csv')
# demographics_t2_raw <- read_csv(file = 'data/matched_time2_mahalanobis.csv')

# for batch script only
demographics_t1 <- read_csv(file = file.path(time_file_path, 'data', 'matched_time1_mahalanobis.csv'))
demographics_t2_raw <- read_csv(file = file.path(time_file_path, 'data', 'matched_time2_mahalanobis.csv'))

# remove duplicates for clustering
demographics_t2 <- distinct(demographics_t2_raw, across(-pair_id))

# pull years denoting t1 and t2
# time1 <- demographics_t1$year[[1]]
# time2 <- demographics_t2$year[[1]]

# create time1 and time2 dataframes for sequences
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
# TODO: do levenshtein difference as well

# compute distances
dist_t1 <- seqdist(atus_seq_t1, method = "OM", indel = 1, sm = TRATE_cost)
dist_t2 <- seqdist(atus_seq_t2, method = "OM", indel = 1, sm = TRATE_cost)
# TODO: can/should TRATE vary by time of day???


# cluster the data
cluster_model_t1 <- fastcluster::hclust(as.dist(dist_t1), method = "ward.D2")
cluster_model_t2 <- fastcluster::hclust(as.dist(dist_t2), method = "ward.D2")
# plot(cluster_model_t1)
# plot(cluster_model_t2)
# TODO: do wards or PAM (similar to kmeans; also called agnes)

# optimize k
k_range <- c(3, 10)
k_seq <- k_range[1]:k_range[2]
# TODO: verify Hubert C index code via 1976 paper
hubert_c_t1 <- sapply(k_seq, function(k) clusterSim::index.C(dist_t1, stats::cutree(cluster_model_t1, k)))
hubert_c_t2 <- sapply(k_seq, function(k) clusterSim::index.C(dist_t2, stats::cutree(cluster_model_t2, k)))
stats_t1 <- sequenchr::cluster_stats(dist_t1, cluster_model_t1, k_range[1], k_range[2])
stats_t2 <- sequenchr::cluster_stats(dist_t2, cluster_model_t2, k_range[1], k_range[2])

# plot all the metrics
validity_stats <- bind_rows(
  tibble(k = k_seq, name = 'Hubert C', value = scale_01(hubert_c_t1)) %>% mutate(time = 't1'),
  tibble(k = k_seq, name = 'Hubert C', value = scale_01(hubert_c_t2)) %>% mutate(time = 't2'),
  stats_t1 %>% select(k, ch_norm, silhouette_norm) %>% mutate(time = 't1') %>% pivot_longer(c("ch_norm", "silhouette_norm")),
  stats_t2 %>% select(k, ch_norm, silhouette_norm) %>% mutate(time = 't2') %>% pivot_longer(c("ch_norm", "silhouette_norm"))
)
mean_metric <- validity_stats %>%
  pivot_wider() %>% 
  group_by(time, k) %>% 
  summarize(mean_metrics = mean(c(1-`Hubert C`, ch_norm, silhouette_norm)),
            .groups = 'drop')
mean_metric %>% 
  rename(value = mean_metrics) %>% 
  mutate(name = "Aggregate mean") %>%  
  bind_rows(validity_stats) %>% 
  mutate(name = recode(name, 
                       'ch_norm' = "Calinski and Harabasz index",
                       'silhouette_norm' = 'Silhouette width'),
         name = factor(name, levels = c('Calinski and Harabasz index', 'Hubert C',
                                        'Silhouette width', 'Aggregate mean'))) %>%
  ggplot(aes(x = k, y = value, color = name, linetype = time)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = k_range[1]:k_range[2]) +
  facet_wrap(~name, nrow = 1) +
  guides(color = FALSE) +
  labs(title = "Normalized cluster validity statistics",
       subtitle = paste0("Best = max(CH), min(Hubert C), max(Silhouette), max(aggregate)\n", time1, "/", time2),
       x = 'n clusters',
       y = 'Normalized index',
       color = NULL,
       linetype = NULL) +
  theme(legend.position = 'bottom')
ggsave(file.path(time_file_path, "plots", 'clustering', "cluster_validity.png"), height = 6, width = 9)

# which k is optimal based on the mean metrics?
optimal_k <- mean_metric %>%  
  group_by(time) %>% 
  summarize(optimal_k = k[which.max(mean_metrics)],
            .groups = 'drop')

# set the cluster labels
k_t1 <- optimal_k$optimal_k[optimal_k$time == 't1']
k_t2 <- optimal_k$optimal_k[optimal_k$time == 't2']
sequenchr::plot_dendrogram(cluster_model_t1, k_t1, 50) + labs(subtitle = paste0("Time 1: ", time1))
ggsave(file.path(time_file_path, "plots", 'clustering', "dendrogram_time1.png"), height = 6, width = 9)
sequenchr::plot_dendrogram(cluster_model_t2, k_t2, 50) + labs(subtitle = paste0("Time 2: ", time2))
ggsave(file.path(time_file_path, "plots", 'clustering', "dendrogram_time2.png"), height = 6, width = 9)
clusters_t1 <- sequenchr::label_clusters(cluster_model_t1, k_t1)
clusters_t2 <- sequenchr::label_clusters(cluster_model_t2, k_t2)

# calculate n and proportion per cluster
n_t1 <- as.numeric(table(clusters_t1))
n_t2 <- as.numeric(table(clusters_t2))
p_t1 <- n_t1 / sum(n_t1)
p_t2 <- n_t2 / sum(n_t2)


# cluster descriptions ----------------------------------------------------

# tidy the data
atus_tidy_t1 <- sequenchr::tidy_sequence_data(atus_seq_t1)
atus_tidy_t2 <- sequenchr::tidy_sequence_data(atus_seq_t2)

# establish color mapping
color_mapping <- viridis::viridis_pal()(length(alphabet(atus_seq_t1)))
names(color_mapping) <- TraMineR::alphabet(atus_seq_t1)

# plot seqI
sequenchr::plot_sequence_index(atus_tidy_t1, color_mapping, clusters_t1) + labs(subtitle = paste0("Time 1: ", time1))
ggsave(file.path(time_file_path, "plots", 'clustering', "seqI_time1.png"), height = 9, width = 9)
sequenchr::plot_sequence_index(atus_tidy_t2, color_mapping, clusters_t2) + labs(subtitle = paste0("Time 2: ", time2))
ggsave(file.path(time_file_path, "plots", 'clustering', "seqI_time2.png"), height = 9, width = 9)

# plot seqD
sequenchr::plot_state(atus_tidy_t1, color_mapping, clusters_t1) + labs(subtitle = paste0("Time 1: ", time1))
ggsave(file.path(time_file_path, "plots", 'clustering', "seqD_time1.png"), height = 9, width = 9)
sequenchr::plot_state(atus_tidy_t2, color_mapping, clusters_t2) + labs(subtitle = paste0("Time 2: ", time2))
ggsave(file.path(time_file_path, "plots", 'clustering', "seqD_time2.png"), height = 9, width = 9)

# plot modals
sequenchr::plot_modal(atus_tidy_t1, color_mapping, clusters_t1) + labs(subtitle = paste0("Time 1: ", time1))
ggsave(file.path(time_file_path, "plots", 'clustering', "modals_time1.png"), height = 9, width = 9)
sequenchr::plot_modal(atus_tidy_t2, color_mapping, clusters_t2) + labs(subtitle = paste0("Time 2: ", time2))
ggsave(file.path(time_file_path, "plots", 'clustering', "modals_time2.png"), height = 9, width = 9)


# matching clusters across times ------------------------------------------

# convert cluster labels to numeric for medoid calculation
clusters_t1_numeric <- as.numeric(str_extract(as.character(clusters_t1), "\\d"))
clusters_t2_numeric <- as.numeric(str_extract(as.character(clusters_t2), "\\d"))

# get medoids (note: medoid is in the dataset)
medoids_t1 <- GDAtools::medoids(dist_t1, clusters_t1_numeric)
# as_tibble(atus_seq_t1)[medoids_t1,]
medoids_t2 <- GDAtools::medoids(dist_t2, clusters_t2_numeric)

# distance between medoids
medoids_all <- rbind(atus_seq_t1[medoids_t1,], atus_seq_t2[medoids_t2,])
medoids_dist <- seqdist(medoids_all, method = "OM", indel = 1, sm = TRATE_cost)
medoids_dist <- medoids_dist[(k_t1+1):nrow(medoids_dist), 1:k_t1]

# optimal matches -- without replacement
medoids_matched <- minimal_distance(medoids_dist, with_replacement = FALSE)

# write out medoid matching
tibble(t1 = seq_along(medoids_matched),
       t2 = medoids_matched) %>% 
  write_csv(path = file.path(time_file_path, 'data', 'medoid_matching.csv'))

# plot and save distance matrix
medoids_dist %>% 
  as.data.frame() %>% 
  setNames(paste0('Cluster ', 1:k_t1, '\n p=', round(p_t1, 2))) %>%
  mutate(label = paste0("Cluster ", 1:k_t2, '\n p=', round(p_t2, 2))) %>% 
  pivot_longer(cols = -label) %>% 
  ggplot(aes(x = label, y = name, fill = value, label = round(value, 2))) +
  geom_tile() +
  shadowtext::geom_shadowtext(colour = 'grey90', bg.colour = 'grey35') +
  # geom_text(color = 'grey70', family = 'Impact') +
  labs(title = 'Distance between medoids in time 1 and time 2',
       subtitle = paste0('Clusters in time 1 are matched to clusters ', paste0(medoids_matched, collapse = ', '), ' respectively',
                         '\n', time1, "/", time2),
       caption = 'p = proportion of observations\n',
       x = '\nCluster in time 2',
       y = 'Cluster in time 1',
       fill = 'TRATE distance')
ggsave(file.path(time_file_path, "plots", "cluster_comparison", "medoids_distance.png"), height = 7, width = 9)

## relabel time2 clusters to with the matched label
clusters_t2_relabeled <- swap_labels(clusters_t1_numeric, 
                                     clusters_t2_numeric, 
                                     label_mapping = medoids_matched)

# verify it worked
# table(new = clusters_t2_relabeled, old = clusters_t2_numeric)

# add label back
clusters_t2_relabeled <- paste0("Cluster ", clusters_t2_relabeled)

# add "n =" label back
# clusters_t2_relabeled <- tibble(old = clusters_t2_numeric_relabeled) %>% 
#   group_by(old) %>% 
#   add_tally() %>% 
#   ungroup() %>% 
#   mutate(new = paste0("Cluster ", old, "  |  n = ", n)) %>% 
#   pull(new)
# clusters_t2_relabeled <- factor(clusters_t2_relabeled, levels = sort(unique(clusters_t2_relabeled)))


# demographics by cluster -------------------------------------------------

# create dataframe denoting ID, cluster assignment, and time period
cluster_assignments <- tibble(
  ID = as.double(c(rownames(atus_seq_t1), rownames(atus_seq_t2))),
  cluster = c(as.character(clusters_t1), as.character(clusters_t2_relabeled)),
  time = c(rep('t1', length(clusters_t1)), rep('t2', length(clusters_t2_relabeled))),
)

# age by cluster and time
cluster_assignments %>%
  left_join(demographics[, c('ID', 'age')], by = 'ID') %>%
  mutate(cluster = str_remove_all(cluster, "  \\|.*")) %>%
  ggplot(aes(x = age)) +
  geom_bar() +
  facet_grid(time~cluster, scales = 'free_y') +
  labs(title = 'Histogram of age by cluster and time',
       subtitle = 'Cluster labels in t2 have been relabeled based on medoid matching',
       x = NULL,
       y = 'n')
ggsave(file.path(time_file_path, "plots", "cluster_comparison", "medoid_matching_age.png"), height = 7, width = 9)

# sex
cluster_assignments %>%
  left_join(demographics[, c('ID', 'sex')], by = 'ID') %>%
  mutate(cluster = str_remove_all(cluster, "  \\|.*"),
         sex = recode(sex, '1' = 'Male', '2' = 'Female')) %>% 
  ggplot(aes(x = sex)) +
  geom_bar() +
  facet_grid(time~cluster, scales = 'free_y') +
  labs(title = 'Count of sex by cluster and time',
       subtitle = paste0(time1, "/", time2),
       caption = 'Cluster labels in t2 have been relabeled based on medoid matching',
       x = NULL,
       y = 'n')
ggsave(file.path(time_file_path, "plots", "cluster_comparison", "medoid_matching_sex.png"), height = 7, width = 9)

# race
cluster_assignments %>%
  left_join(demographics[, c('ID', 'race')], by = 'ID') %>%
  mutate(cluster = str_remove_all(cluster, "  \\|.*")) %>% 
  ggplot(aes(x = race)) +
  geom_bar() +
  facet_grid(time~cluster, scales = 'free_y') +
  labs(title = 'Count of race by cluster and time',
       subtitle = paste0(time1, "/", time2),
       caption = 'Cluster labels in t2 have been relabeled based on medoid matching',
       x = NULL,
       y = 'n') +
  theme(axis.text.x = element_text(angle = -40, hjust = 0))
ggsave(file.path(time_file_path, "plots", "cluster_comparison", "medoid_matching_race.png"), height = 7, width = 9)

# metropolitan
cluster_assignments %>%
  left_join(demographics[, c('ID', 'metropolitan')], by = 'ID') %>%
  mutate(cluster = str_remove_all(cluster, "  \\|.*")) %>% 
  ggplot(aes(x = metropolitan)) +
  geom_bar() +
  facet_grid(time~cluster, scales = 'free_y') +
  labs(title = 'Count of metropolitan status by cluster and time',
       subtitle = paste0(time1, "/", time2),
       caption = 'Cluster labels in t2 have been relabeled based on medoid matching',
       x = NULL,
       y = 'n') +
  theme(axis.text.x = element_text(angle = -40, hjust = 0))
ggsave(file.path(time_file_path, "plots", 'cluster_comparison', "medoid_matching_metropolitan.png"), height = 7, width = 9)

# has_partner
cluster_assignments %>%
  left_join(demographics[, c('ID', 'has_partner')], by = 'ID') %>%
  mutate(cluster = str_remove_all(cluster, "  \\|.*")) %>% 
  ggplot(aes(x = has_partner)) +
  geom_bar() +
  facet_grid(time~cluster, scales = 'free_y') +
  labs(title = 'Count of has_partner status by cluster and time',
       subtitle = paste0(time1, "/", time2),
       caption = 'Cluster labels in t2 have been relabeled based on medoid matching',
       x = NULL,
       y = 'n')
ggsave(file.path(time_file_path, "plots", "cluster_comparison", "medoid_matching_has_partner.png"), height = 7, width = 9)


# transitions between clusters --------------------------------------------

# relative cluster sizes
table(clusters_t1) / sum(table(clusters_t1))
table(clusters_t2_relabeled) / sum(table(clusters_t2_relabeled))

# create dataframe indicating the cluster assignment and pair_id
pair_ids <- tibble(
  ID = c(demographics_t1$ID, demographics_t2_raw$ID),
  pair_id = c(demographics_t1$pair_id, demographics_t2_raw$pair_id),
)
cluster_pairs <- left_join(pair_ids, cluster_assignments, by = 'ID')

# write out: batch script only
write_csv(cluster_pairs, path = file.path(time_file_path, 'data', 'cluster_pairs.csv'))
# write_csv(cluster_pairs, path = file.path('data', 'cluster_pairs.csv'))

# how consistent are the clusters across time1 and time2?
cluster_pairs_wide <- cluster_pairs %>% 
  mutate(cluster = str_remove_all(cluster, "  \\|.*")) %>%
  pivot_wider(id_cols = pair_id, values_from = cluster, names_from = time)
transition_rate <- cluster_pairs_wide %>% 
  group_by(t1) %>% 
  summarize(cluster_transition = list(table(t2)),
            .groups = 'drop') %>% 
  unnest_longer(col = cluster_transition) %>%
  select(t1, t2 = cluster_transition_id, n = cluster_transition) %>%
  mutate(n = as.numeric(n),
         rate = n / sum(n)) %>%
  group_by(t1) %>%
  mutate(rate_group = n / sum(n)) %>%
  ungroup()

transition_table <- table(t1 = cluster_pairs_wide$t1, t2 = cluster_pairs_wide$t2)

# write out: batch script only
write_csv(as_tibble(transition_table), 
          path = file.path(time_file_path, 'data', 'transition_matrix.csv'))

# how often do matches stay in the same cluster?
mean(cluster_pairs_wide$t1 == cluster_pairs_wide$t2)

# transition matrix
transition_rate %>%
  group_by(t1) %>% 
  mutate(t1 = paste0(t1, '\n n=', sum(n))) %>% 
  group_by(t2) %>%
  mutate(t2 = paste0(t2, '\n n=', sum(n))) %>% 
  ggplot(aes(x = t2, y = t1, fill = rate_group, label = round(rate_group, 2))) +
  geom_tile() +
  shadowtext::geom_shadowtext(colour = 'grey90', bg.colour = 'grey35') +
  # geom_text(color = 'grey70') +
  labs(title = "Transition matrix between matched clusters. Rows sum to 1.",
       subtitle = paste0(time1, "/", time2),
       caption = paste0('Cluster labels in t2 have been relabeled based on medoid matching',
                        '\nn values in time 2 are the absorbing observations not the unique time 2 observations E.g. sum(t2_n) == length(t1)'),
       x = "\nCluster in time 2",
       y = "Cluster in time 1",
       fill = "Transition rate") +
  theme(axis.text.x = element_text(angle = -20, hjust = 0))
ggsave(file.path(time_file_path, "plots", "cluster_comparison", "transition_matrix.png"), height = 7, width = 9)

# transition rate by cluster
# ggplot(transition_rate, aes(x = t1, y = rate_group, fill = t2)) +
#   geom_col() +
#   labs(title = "Transition from cluster X to cluster Y",
#        x = "\nCluster in time 1",
#        y = "Transition rate from cluster in time 1",
#        fill = "To cluster in time 2")
