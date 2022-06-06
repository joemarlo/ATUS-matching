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

# read in data with diary date
atusresp_0320 <- readr::read_csv(file.path("inputs", "ATUS-2003-2020", "atusresp_0320.dat"))

# regions for matching
state_regions <- readr::read_csv("inputs/state_regions.csv")


# population --------------------------------------------------------------

year1 <- 2019
year2 <- 2020

# convert atus data to wide for traminer
atus_wide <- local({
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
  year1_IDs <- demographics %>% 
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


# traminer ----------------------------------------------------------------

# define alphabet as all unique states
alphabet <- atus_wide$t1[,-1] %>% unlist() %>% unique() %>% sort()
labels <- c("Care_HH", "Care_NHH", "Cons_Pur", "Eat_drink", "Edu", 
            "HH_activ", "Other", "Prsl_care", "Care_svcs", "Rel_spirit", 
            "Sleep", "Leisure", "Recreation", "Volunteer", "Work")

# create state sequence object
atus_seq_t1 <- seqdef(
  data = atus_wide$t1[, -1], 
  alphabet = alphabet, 
  id = atus_wide$t1$ID,
  # labels = labels,
  xtstep = 1)
atus_seq_t2 <- seqdef(
  data = atus_wide$t2[, -1], 
  alphabet = alphabet, 
  id = atus_wide$t2$ID,
  # labels = labels,
  xtstep = 1)

# sequenchr::launch_sequenchr(atus_seq_t1)

# tidy the data
atus_tidy_t1 <- sequenchr::tidy_sequence_data(atus_seq_t1)
atus_tidy_t2 <- sequenchr::tidy_sequence_data(atus_seq_t2)

# establish color mapping for plots
color_mapping <- viridis::viridis_pal()(length(alphabet(atus_seq_t1)))
names(color_mapping) <- TraMineR::alphabet(atus_seq_t1)


# clustering --------------------------------------------------------------

### TRATE method
## calculate transition rates
## TRATE_t1 <- seqtrate(atus_seq_t1)

# calculate substitution cost from year1
# TRATE_cost <- seqsubm(atus_seq_t1, method = 'TRATE') #, time.varying = TRUE
# # TRATE_cost_t2 <- TRATE_cost # default
# TRATE_cost_t2 <- seqsubm(atus_seq_t2, method = 'TRATE') # only use when clustering separately
# 
# # compute distances
# dist_t1 <- seqdist(atus_seq_t1, method = "OM", sm = TRATE_cost)
# dist_t2 <- seqdist(atus_seq_t2, method = "OM", sm = TRATE_cost_t2)

### LCS method
dist_t1 <- seqdist(atus_seq_t1, method = "LCS")
dist_t2 <- seqdist(atus_seq_t2, method = "LCS")


# cluster the data and create summary metrics
k_range <- c(2, 10)
cluster_algo <- 'hclust'

# cluster the data
cluster_model_t1 <- fastcluster::hclust(as.dist(dist_t1), method = "ward.D2")
cluster_model_t2 <- fastcluster::hclust(as.dist(dist_t2), method = "ward.D2")
# plot(cluster_model_t1)
# plot(cluster_model_t2)

# optimize k
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
  guides(color = 'none') +
  labs(title = "Normalized cluster validity statistics",
       subtitle = paste0("Best = max(CH), min(Hubert C), max(Silhouette), max(aggregate)\n", year1, "/", year2),
       x = 'n clusters',
       y = 'Normalized index',
       color = NULL,
       linetype = NULL) +
  theme(legend.position = 'bottom')
# ggsave(file.path(file_path, "plots", 'clustering', "cluster_validity.png"), height = 6, width = 9)

# which k is optimal based on the mean metrics?
optimal_k <- mean_metric %>%  
  group_by(time) %>% 
  summarize(optimal_k = k[which.max(mean_metrics)],
            .groups = 'drop')

# set the cluster labels
k_t1 <- optimal_k$optimal_k[optimal_k$time == 't1']
k_t2 <- optimal_k$optimal_k[optimal_k$time == 't2']
sequenchr::plot_dendrogram(cluster_model_t1, k_t1, 50) + labs(subtitle = paste0("Time 1: ", year1))
# ggsave(file.path(file_path, "plots", 'clustering', "dendrogram_year1.png"), height = 6, width = 9)
sequenchr::plot_dendrogram(cluster_model_t2, k_t2, 50) + labs(subtitle = paste0("Time 2: ", year2))
# ggsave(file.path(file_path, "plots", 'clustering', "dendrogram_year2.png"), height = 6, width = 9)
clusters_t1 <- sequenchr::label_clusters(cluster_model_t1, k_t1)
clusters_t2 <- sequenchr::label_clusters(cluster_model_t2, k_t2)

# calculate n and proportion per cluster
n_t1 <- as.numeric(table(clusters_t1))
n_t2 <- as.numeric(table(clusters_t2))
p_t1 <- n_t1 / sum(n_t1)
p_t2 <- n_t2 / sum(n_t2)


# cluster descriptions ----------------------------------------------------

# plot seqI
sequenchr::plot_sequence_index(atus_tidy_t1, color_mapping, clusters_t1) + labs(subtitle = paste0("Time 1: ", year1))
# ggsave(file.path(file_path, "plots", 'clustering', "seqI_year1.png"), height = 9, width = 9)
sequenchr::plot_sequence_index(atus_tidy_t2, color_mapping, clusters_t2) + labs(subtitle = paste0("Time 2: ", year2))
# ggsave(file.path(file_path, "plots", 'clustering', "seqI_year2.png"), height = 9, width = 9)

# plot seqD
sequenchr::plot_state(atus_tidy_t1, color_mapping, clusters_t1) + labs(subtitle = paste0("Time 1: ", year1))
# ggsave(file.path(file_path, "plots", 'clustering', "seqD_year1.png"), height = 9, width = 9)
sequenchr::plot_state(atus_tidy_t2, color_mapping, clusters_t2) + labs(subtitle = paste0("Time 2: ", year2))
# ggsave(file.path(file_path, "plots", 'clustering', "seqD_year2.png"), height = 9, width = 9)

# plot modals
sequenchr::plot_modal(atus_tidy_t1, color_mapping, clusters_t1) + labs(subtitle = paste0("Time 1: ", year1))
# ggsave(file.path(file_path, "plots", 'clustering', "modals_year1.png"), height = 9, width = 9)
sequenchr::plot_modal(atus_tidy_t2, color_mapping, clusters_t2) + labs(subtitle = paste0("Time 2: ", year2))
# ggsave(file.path(file_path, "plots", 'clustering', "modals_year2.png"), height = 9, width = 9)

# plotly::ggplotly()
