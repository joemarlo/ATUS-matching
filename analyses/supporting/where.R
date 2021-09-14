library(dplyr)
source('analyses/demographics.R')
source('data/helpers.R')
source('analyses/plots/ggplot_settings.R')

# read in date denoting latest participants by cluster
cluster_pairs <- readr::read_csv('analyses/data/cluster_pairs.csv')

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
cluster_pairs %>%
  filter(time == 't2') %>% 
  left_join(select(atusact_0320, ID = TUCASEID, TEWHERE), 
            by = 'ID') %>% 
  left_join(locations, by = 'TEWHERE') %>% 
  ggplot(aes(x = location)) +
  geom_bar(aes(y = ..prop.., group = cluster)) +
  # geom_bar() +
  facet_wrap(~cluster, ncol = 1) +
  theme(axis.text.x = element_text(angle = -40, hjust = 0))


