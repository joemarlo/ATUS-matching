library(dplyr)
library(ggplot2)
library(TraMineR)
source('analyses/plots/ggplot_settings.R')

# read in ATUS data
# atus_raw <- read_tsv(file.path("data", "atus_30min.tsv"))
atus_raw <- readr::read_tsv(file.path("data", "atus_30min_SSC.tsv")) # tokens with supplemental child care

# read in cluster memberships
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

# seqI plot sorted amount of work
yellow_labels <- c('Work : No SCC', 'Work : SCC')
atus_raw %>% 
  right_join(clusters, by = 'ID') %>% 
  group_by(ID) %>% 
  mutate(entropy = sum(description %in% yellow_labels),
         year = case_when(time == 't1' ~ '2019',
                          time == 't2' ~ '2020',
                          TRUE ~ 'NA')) %>% 
  ungroup() %>% 
  ggplot(aes(x = period, 
             y = stats::reorder(ID, entropy), 
             fill = description)) + 
  geom_tile() + 
  scale_fill_manual(values = color_mapping) +
  scale_y_discrete(labels = NULL, breaks = NULL) + 
  facet_wrap(year~cluster, scales = "free_y", ncol = 3) + 
  labs(title = "All sequences by cluster sorted by count of work activities", 
       x = "Period", 
       y = "Sequence", 
       fill = NULL)


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
  group_by(cluster, year) %>%
  mutate(group_mean = mean(turbulence)) %>% 
  ggplot(aes(x = turbulence, color = cluster)) +
  geom_density(size = 1.5, alpha = 0.85) +
  geom_vline(aes(xintercept = group_mean, color = cluster),
             linetype = 'dashed') +
  ggplot2::scale_color_discrete() +
  facet_wrap(~year, ncol = 1) +
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
