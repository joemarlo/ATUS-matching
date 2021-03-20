library(tidyverse)
library(TraMineR)
library(fastcluster)
library(sequenchr)
set.seed(44)

# read in ATUS data
atus_raw <- read_tsv("data/atus_30min.tsv")

# read in the demographics data
demographics <- read_delim(file = "data/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE)


# filter to only include respondents who logged on weekdays and non holidays
# filter to only include 2007:2009 data
IDs <- demographics %>% 
  filter(day_of_week %in% 2:6,
         holiday == 0,
         year %in% 2007:2009) %>% 
  dplyr::select(ID, survey_weight)

# filter to just include weekends, pivot wider, and sample with weights
n_sample <- 2500
atus_sampled <- atus_raw %>% 
  pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  right_join(IDs, by = "ID") %>% 
  slice_sample(n = n_sample, weight_by = survey_weight) %>% 
  dplyr::select(-survey_weight) %>% 
  arrange(ID)

# create df of just the demographics for the sample
demographics_sample <- demographics %>% 
  filter(ID %in% atus_sampled$ID) %>% 
  arrange(ID) %>% 
  select(age, sex, age_youngest, n_child, HH_income, married, year)


# traminer ----------------------------------------------------------------

# define alphabet as all unique states
alphabet <- atus_sampled[,-1] %>% unlist() %>% unique() %>% sort()
labels <- c("Care_HH", "Care_NHH", "Cons_Pur", "Eat_drink", "Edu", 
            "HH_activ", "Other", "Prsl_care", "Care_svcs", "Rel_spirit", 
            "Sleep", "Leisure", "Recreation", "Volunteer", "Work")

# create state sequence object
atus_seq <- seqdef(data = atus_sampled[, -1], 
                   alphabet = alphabet, 
                   # states = mvad.scodes,
                   id = atus_sampled$ID,
                   labels = labels,
                   xtstep = 1)


# sequenchr ---------------------------------------------------------------

launch_sequenchr(atus_seq, demographics_sample)

# todo: look at separation metrics across 2007:2009 individually

# add capability to break by group before cluster??
# look for test for 1 vs many clusters under heierachcal (hennig from fpc)
#   https://link.springer.com/article/10.1007/s11222-015-9566-5
# add loading screen so users know whats going on


# manual clustering -------------------------------------------------------

# compute optimal matching distances
# dist_om_TRATE <- seqdist(atus_seq, method = "OM", indel = 1, sm = "TRATE")
# dist_om_DHD <- seqdist(atus_seq, method = "DHD")

# cluster the data
# clusters <- fastcluster::hclust(as.dist(dist_om_DHD), method = "ward.D2")

