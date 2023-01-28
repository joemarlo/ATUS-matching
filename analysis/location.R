### location data

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
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


# WFH over time
demographics |> 
  filter(labor_force_status %in% c("employed - absent", "employed - at work"),
         day_of_week %in% 2:6) |> 
  group_by(year) |> 
  summarize(WFH_rate = mean(is_WFH)) |> 
  ggplot(aes(x = year, y = WFH_rate)) + 
  geom_col()
