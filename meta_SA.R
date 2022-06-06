###
# This script performs the sequence analysis and related clustering
# work-in-progress
###


library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(TraMineR)
library(fastcluster)
library(sequenchr)
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


# no matching for this clustering



# population --------------------------------------------------------------

year1 <- 2019
year2 <- 2020

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

# calculate transition rates
# TRATE_t1 <- seqtrate(atus_seq_t1)

# calculate substitution cost from time1
TRATE_cost <- seqsubm(atus_seq_t1, method = 'TRATE') #, time.varying = TRUE
# TRATE_cost_t2 <- TRATE_cost # default
TRATE_cost_t2 <- seqsubm(atus_seq_t2, method = 'TRATE') # only use when clustering separately
# TODO: do levenshtein difference as well

# compute distances
dist_t1 <- seqdist(atus_seq_t1, method = "OM", sm = TRATE_cost)
dist_t2 <- seqdist(atus_seq_t2, method = "OM", sm = TRATE_cost_t2)



