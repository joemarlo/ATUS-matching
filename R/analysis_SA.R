
#' Cluster the American Time Use survey sequences
#'
#' See sequence_analysis.R for example
#'
#' @param atus_wide 
#' @param year1 
#' @param year2 
#' @param method one of c('LCS', 'TRATE')
#' @param k_range range of k values to test to determine best k values
#' @param cluster_algo unused
#' @param cluster_method clustering method to be passed to fastcluster::hclust
#' @param include_plots TRUE/FALSE
#'
#' @return
#' @export
#'
#' @examples
cluster_sequences <- function(atus_wide, year1, year2, method = 'LCS', k_range = c(2, 10), cluster_algo = 'hclust', cluster_method = 'ward.D2', include_plots = FALSE){
  
  method <- match.arg(method, c('LCS', 'TRATE'))
  cluster_algo <- match.arg(cluster_algo, c('hclust'))
  cluster_method <- match.arg(cluster_method, c('ward.D2'))
  
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

  
  # clustering --------------------------------------------------------------
  
  if (method == 'TRATE'){
    # calculate substitution cost from year1
    TRATE_cost <- seqsubm(atus_seq_t1, method = 'TRATE') #, time.varying = TRUE
    # TRATE_cost_t2 <- TRATE_cost # default
    TRATE_cost_t2 <- seqsubm(atus_seq_t2, method = 'TRATE') # only use when clustering separately

    # compute distances
    dist_t1 <- seqdist(atus_seq_t1, method = "OM", sm = TRATE_cost)
    dist_t2 <- seqdist(atus_seq_t2, method = "OM", sm = TRATE_cost_t2)
  } else if (method == 'LCS'){
    # LCS method
    dist_t1 <- seqdist(atus_seq_t1, method = method)
    dist_t2 <- seqdist(atus_seq_t2, method = method)
  }
  
  # cluster the data
  cluster_model_t1 <- fastcluster::hclust(as.dist(dist_t1), method = cluster_method)
  cluster_model_t2 <- fastcluster::hclust(as.dist(dist_t2), method = cluster_method)
  # plot(cluster_model_t1)
  # plot(cluster_model_t2)
  
  # optimize k
  k_seq <- k_range[1]:k_range[2]
  
  # metrics
  # Hubert C index code via 1976 paper
  hubert_c_t1 <- sapply(k_seq, function(k) clusterSim::index.C(dist_t1, stats::cutree(cluster_model_t1, k)))
  hubert_c_t2 <- sapply(k_seq, function(k) clusterSim::index.C(dist_t2, stats::cutree(cluster_model_t2, k)))
  stats_t1 <- sequenchr::cluster_stats(dist_t1, cluster_model_t1, k_range[1], k_range[2])
  stats_t2 <- sequenchr::cluster_stats(dist_t2, cluster_model_t2, k_range[1], k_range[2])
  
  # put stats in same data format
  validity_stats <- bind_rows(
    tibble(k = k_seq, name = 'Hubert C', value = scale_01(hubert_c_t1)) %>% mutate(time = 't1'),
    tibble(k = k_seq, name = 'Hubert C', value = scale_01(hubert_c_t2)) %>% mutate(time = 't2'),
    stats_t1 %>% select(k, ch_norm, silhouette_norm) %>% mutate(time = 't1') %>% pivot_longer(c("ch_norm", "silhouette_norm")),
    stats_t2 %>% select(k, ch_norm, silhouette_norm) %>% mutate(time = 't2') %>% pivot_longer(c("ch_norm", "silhouette_norm"))
  )
  
  # calculate average normalized stat
  mean_metric <- validity_stats %>%
    pivot_wider() %>% 
    group_by(time, k) %>% 
    summarize(mean_metrics = mean(c(1-`Hubert C`, ch_norm, silhouette_norm)),
              .groups = 'drop')
  
  # which k is optimal based on the mean metrics?
  optimal_k <- mean_metric %>%  
    group_by(time) %>% 
    summarize(optimal_k = k[which.max(mean_metrics)],
              .groups = 'drop')
  
  # set the cluster labels
  k_t1 <- optimal_k$optimal_k[optimal_k$time == 't1']
  k_t2 <- optimal_k$optimal_k[optimal_k$time == 't2']
  clusters_t1 <- sequenchr::label_clusters(cluster_model_t1, k_t1)
  clusters_t2 <- sequenchr::label_clusters(cluster_model_t2, k_t2)
  
  # calculate n and proportion per cluster
  n_t1 <- as.numeric(table(clusters_t1))
  n_t2 <- as.numeric(table(clusters_t2))
  p_t1 <- n_t1 / sum(n_t1)
  p_t2 <- n_t2 / sum(n_t2)
  

  # plots -------------------------------------------------------------------

  plots <- NULL
  if (isTRUE(include_plots)){
    
    # plot all the metrics
    p_validity <- mean_metric %>% 
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
    
    # dendrograms
    p_dendrogram_t1 <- sequenchr::plot_dendrogram(cluster_model_t1, k_t1, 50) + labs(subtitle = paste0("Time 1: ", year1))
    # ggsave(file.path(file_path, "plots", 'clustering', "dendrogram_year1.png"), height = 6, width = 9)
    p_dendrogram_t2 <- sequenchr::plot_dendrogram(cluster_model_t2, k_t2, 50) + labs(subtitle = paste0("Time 2: ", year2))
    # ggsave(file.path(file_path, "plots", 'clustering', "dendrogram_year2.png"), height = 6, width = 9)
    
    
    # tidy the data
    atus_tidy_t1 <- sequenchr::tidy_sequence_data(atus_seq_t1)
    atus_tidy_t2 <- sequenchr::tidy_sequence_data(atus_seq_t2)
    
    # establish color mapping for plots
    color_mapping <- viridis::viridis_pal()(length(alphabet(atus_seq_t1)))
    names(color_mapping) <- TraMineR::alphabet(atus_seq_t1)
    
    # plot seqI
    p_seqi_t1 <- sequenchr::plot_sequence_index(atus_tidy_t1, color_mapping, clusters_t1) + labs(subtitle = paste0("Time 1: ", year1))
    # ggsave(file.path(file_path, "plots", 'clustering', "seqI_year1.png"), height = 9, width = 9)
    p_seqi_t2 <- sequenchr::plot_sequence_index(atus_tidy_t2, color_mapping, clusters_t2) + labs(subtitle = paste0("Time 2: ", year2))
    # ggsave(file.path(file_path, "plots", 'clustering', "seqI_year2.png"), height = 9, width = 9)
    
    # plot seqD
    p_seqd_t1 <- sequenchr::plot_state(atus_tidy_t1, color_mapping, clusters_t1) + labs(subtitle = paste0("Time 1: ", year1))
    # ggsave(file.path(file_path, "plots", 'clustering', "seqD_year1.png"), height = 9, width = 9)
    p_seqd_t2 <- sequenchr::plot_state(atus_tidy_t2, color_mapping, clusters_t2) + labs(subtitle = paste0("Time 2: ", year2))
    # ggsave(file.path("outputs", 'SA', method, "seqD_year2.png"), height = 9, width = 9)
    
    # plot modals
    p_modal_t1 <- sequenchr::plot_modal(atus_tidy_t1, color_mapping, clusters_t1) + labs(subtitle = paste0("Time 1: ", year1))
    # ggsave(file.path(file_path, "plots", 'clustering', "modals_year1.png"), height = 9, width = 9)
    p_modal_t2 <- sequenchr::plot_modal(atus_tidy_t2, color_mapping, clusters_t2) + labs(subtitle = paste0("Time 2: ", year2))
    # ggsave(file.path(file_path, "plots", 'clustering', "modals_year2.png"), height = 9, width = 9)
    
    # plotly::ggplotly()
    
    plots <- list(
      validity = p_validity,
      dendrogram = list(t1 = p_dendrogram_t1,
                        t2 = p_dendrogram_t2),
      seqi = list(t1 = p_seqi_t1,
                  t2 = p_seqi_t2),
      seqd = list(t1 = p_seqd_t1,
                  t2 = p_seqd_t2),
      modal = list(t1 = p_modal_t1,
                   t2 = p_modal_t2)
      
    )
  }
  
  # df denoting the clusters
  clusters <- tibble(
    ID = c(atus_wide$t1$ID, atus_wide$t2$ID),
    cluster = c(clusters_t1, clusters_t2),
    year = c(rep(year1, length(clusters_t1)),
             rep(year2, length(clusters_t2)))
  )
  
  # output structure
  out <- list(
    sequences = list(t1 = atus_tidy_t1,
                     t2 = atus_tidy_t2),
    clusters = clusters,
    k = list(t1 = k_t1,
             t2 = k_t2),
    plots = plots
  )
  
  return(out)
}



#' Prep the data for clustering
#' 
#' See sequence_analysis.R for example
#'
#' @param year1 
#' @param year2 
#' @param atusresp_0320 
#' @param state_regions 
#' @param demographics 
#'
#' @return
#' @export
sequence_prep_atus <- function(year1, year2, atusresp_0320, state_regions, demographics){
  
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
}
