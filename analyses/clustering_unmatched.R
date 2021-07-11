# library(tidyverse)
# library(TraMineR)
# library(fastcluster)
# library(sequenchr)
# source('analyses/plots/ggplot_settings.R')
# source('analyses/helpers_analyses.R')
source('analyses/demographics.R')
set.seed(44)

# this script clusters the observations that were thrown out due to matching
# only works in batch mode


# prep the data -----------------------------------------------------------

# create dataframe of unmatched observations
unmatched_observations <- demographics %>% 
  filter(year == time2, 
         ID %notin% demographics_t2_raw$ID)

# write out
write_csv(unmatched_observations, 
          path = file.path(file_path, 'data', 't2_unmatched_observations.csv'))

# create dataframe of time use sequences
atus_unmatched <- atus_raw[atus_raw$ID %in% unmatched_observations$ID,]

# widen the sequence data
atus_unmatched <- atus_unmatched %>% 
  pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  arrange(ID)

# create state sequence object
atus_seq_unmatched <- seqdef(
  data = atus_unmatched[, -1], 
  alphabet = alphabet, 
  id = atus_unmatched$ID,
  labels = labels,
  xtstep = 1)


# cluster the data --------------------------------------------------------

# compute distances
dist_unmatched <- seqdist(atus_seq_unmatched, method = "OM", sm = TRATE_cost)

# cluster the data
cluster_model_unmatched <- fastcluster::hclust(as.dist(dist_unmatched), method = "ward.D2")

# optimize k
hubert_c_unmatched <- sapply(k_seq, function(k) clusterSim::index.C(dist_unmatched, stats::cutree(cluster_model_unmatched, k)))
stats_unmatched <- sequenchr::cluster_stats(dist_unmatched, cluster_model_unmatched, k_range[1], k_range[2])

# plot all the metrics
validity_stats <- bind_rows(
  tibble(k = k_seq, name = 'Hubert C', value = scale_01(hubert_c_unmatched)) %>% mutate(time = 'unmatched'),
  stats_unmatched %>% select(k, ch_norm, silhouette_norm) %>% mutate(time = 'unmatched') %>% pivot_longer(c("ch_norm", "silhouette_norm")),
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
ggsave(file.path(file_path, "plots", 'clustering', "cluster_validity_unmatched.png"), height = 6, width = 9)

# which k is optimal based on the mean metrics?
optimal_k <- mean_metric %>%  
  group_by(time) %>% 
  summarize(optimal_k = k[which.max(mean_metrics)],
            .groups = 'drop')

# set the cluster labels
k_unmatched <- optimal_k$optimal_k[optimal_k$time == 'unmatched']
sequenchr::plot_dendrogram(cluster_model_unmatched, k_unmatched, 50) + labs(subtitle =  paste0("Unmatched: ", time1, "/", time2))
ggsave(file.path(file_path, "plots", 'clustering', "dendrogram_unmatched.png"), height = 6, width = 9)
clusters_unmatched <- sequenchr::label_clusters(cluster_model_unmatched, k_unmatched)

# calculate n and proportion per cluster
n_unmatched <- as.numeric(table(clusters_unmatched))
p_unmatched <- n_unmatched / sum(n_unmatched)


# cluster descriptions ----------------------------------------------------

# tidy the data
atus_tidy_unmatched <- sequenchr::tidy_sequence_data(atus_seq_unmatched)

# plot seqI
sequenchr::plot_sequence_index(atus_tidy_unmatched, color_mapping, clusters_unmatched) + labs(subtitle = paste0("Unmatched: ", time1, "/", time2))
ggsave(file.path(file_path, "plots", 'clustering', "seqI_unmatched.png"), height = 9, width = 9)

# plot seqD
sequenchr::plot_state(atus_tidy_unmatched, color_mapping, clusters_unmatched) + labs(subtitle = paste0("Unmatched: ", time1, "/", time2))
ggsave(file.path(file_path, "plots", 'clustering', "seqD_unmatched.png"), height = 9, width = 9)

# plot modals
sequenchr::plot_modal(atus_tidy_unmatched, color_mapping, clusters_unmatched) + labs(subtitle =  paste0("Unmatched: ", time1, "/", time2))
ggsave(file.path(file_path, "plots", 'clustering', "modals_unmatched.png"), height = 9, width = 9)
