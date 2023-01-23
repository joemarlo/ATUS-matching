### location data

# TEWHERE



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

# get location information
location_codes <- tibble::tribble(
  ~code, ~description,
  1, "Respondent's home or yard",
  2, "Respondent's workplace",
  3, "Someone else's home",
  4, "Restaurant or bar",
  5, "Place of worship",
  6, "Grocery store",
  7, "Other store/mall",
  8, "School",
  9, "Outdoors away from home",
  10, "Library",
  11, "Other place",
  12, "Car, truck, or motorcycle (driver)",
  13, "Car, truck, or motorcycle (passenger)",
  14, "Walking",
  15, "Bus",
  16, "Subway/train",
  17, "Bicycle",
  18, "Boat/ferry",
  19, "Taxi/limousine service",
  20, "Airplane",
  21, "Other mode of transportation",
  30, "Bank",
  31, "Gym/health club",
  32, "Post Office",
  89, "Unspecified place",
  99, "Unspecified mode of transportation"
)

# join to get location descriptions
atus <- atus_raw |> 
  dplyr::left_join(location_codes, by = c('location' = 'code')) |> 
  dplyr::select(ID, period, description = description.x, location = description.y)
  
# how many NAs by activity
# TEWHERE is not collected for activities with activity codes of 
# 0101xx (sleep), 0102xx (sleep), 0104xx (private), 500105 (dnr), or 500106 (dnr)
atus |> 
  group_by(description) |> 
  summarize(n = mean(is.na(location))) |> 
  filter(n != 0) |> 
  ggplot(aes(x = reorder(description, n), y = n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -40, hjust = 0))

# replace NA for sleep
atus <- atus |> 
  mutate(location = ifelse(description == 'Sleep : No SCC', '[sleep]', location),
         location = ifelse(description %in% c('Personal Care : No SCC', 'Personal Care : SCC'),
                           '[personal]', location),
         location = ifelse(is.na(location), '[did not respond]', location))



# atus_act <- readr::read_csv("inputs/ATUS-2003-2021/atusact_0321.dat")
# distribution of locations
atus_act |> 
  dplyr::select(code = TEWHERE) |> 
  dplyr::left_join(location_codes) |> 
  count(description) |> 
  ggplot(aes(x = reorder(description, n), y = n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -60, hjust = 0))

# 
atus_act |> 
  filter(code == -1) |> 
  View()

  
