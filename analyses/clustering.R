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

# read in the matches
demographics_treated <- read_csv(file = 'data/matched_treatment.csv')
demographics_control <- read_csv(file = 'data/matched_control.csv')

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


# manual clustering -------------------------------------------------------

# compute optimal matching distances
dist_treated <- seqdist(atus_seq_treated, method = "OM", indel = 1, sm = "TRATE")
dist_control <- seqdist(atus_seq_control, method = "OM", indel = 1, sm = "TRATE")

# cluster the data
clusters_treated <- fastcluster::hclust(as.dist(dist_treated), method = "ward.D2")
clusters_control <- fastcluster::hclust(as.dist(dist_control), method = "ward.D2")
