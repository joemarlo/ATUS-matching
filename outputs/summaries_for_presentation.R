

library(dplyr)

# data filtering summary

# time series
atussum <- readr::read_csv(file.path("inputs","ATUS-2003-2021", 'atussum_0321.dat'))
nrow(atussum)

# matching


# sequence analysis


# word cloud --------------------------------------------------------------

# read in ATUS data
atus_raw <- readr::read_tsv(file.path("data", "atus_30min_SSC.tsv"))

atus_raw <- atus_raw %>% 
  mutate(activity = stringr::str_remove_all(description, " :.*$"))

activity_counts <- table(atus_raw$activity)

wordcloud::wordcloud(names(activity_counts), 
                     as.vector(activity_counts),
                     scale = c(5, 1.2),
                     min.freq = 0,
                     rot.per = 0.1,
                     fixed.asp = TRUE)
