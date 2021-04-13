### script estimates the expected match rate if matching is performed randomly 
library(tidyverse)


# binary ------------------------------------------------------------------

categories <- c(TRUE, FALSE) 
sample_size <- 10000
splits <- seq(0.5, 1, by = 0.05)
sims <- map_dfr(splits, function(split){
  probs <- c(split, 1-split)
  pop1 <- sample(categories, size = sample_size, replace = TRUE, prob = probs)
  pop2 <- sample(categories, size = sample_size, replace = TRUE, prob = probs)
  sims <- map_dfr(1:2500, function(x){
    random_match_rate <- mean(sample(pop1) == sample(pop2))
    sim <- tibble(split = split, sim = x, random_match_rate = random_match_rate)
    return(sim)
  })
  return(sims)
})

# get expectation then apply loess to get more datapoints
(matching_expectation <- sims %>% group_by(split) %>% summarize(expectation = mean(random_match_rate)))
# write_csv(matching_expectation, "data/matching_expectation.csv")
# smoothing_model <- loess(mean ~ split, data = matching_expectation)

# plot it
sims %>% 
  ggplot(aes(x = split, group = split, y = random_match_rate)) +
  geom_boxplot() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(breaks = splits) +
  scale_y_continuous(breaks = splits) +
  labs(title = "Binary: Expected % of true matches if matching is random",
       subtitle = "Expectation is not perfectly linear with bias",
       x = "Bias (e.g. data consists of X% A vs. B observations)",
       y = "Simulated match rate") +
  theme_minimal()


# trinary -----------------------------------------------------------------

categories <- LETTERS[1:3]
sample_size <- 10000
splits <- seq(0.3, 1, by = 0.05)
sims <- map_dfr(splits, function(split){
  probs <- c(split, 1-(split*0.5), 1-(split*0.5))
  pop1 <- sample(categories, size = sample_size, replace = TRUE, prob = probs)
  pop2 <- sample(categories, size = sample_size, replace = TRUE, prob = probs)
  sims <- map_dfr(1:2500, function(x){
    random_match_rate <- mean(sample(pop1) == sample(pop2))
    sim <- tibble(split = split, sim = x, random_match_rate = random_match_rate)
    return(sim)
  })
  return(sims)
})

# plot it
sims %>% 
  ggplot(aes(x = split, group = split, y = random_match_rate)) +
  geom_boxplot() +
  scale_x_continuous(breaks = splits) +
  labs(title = "Trinary: Expected % of true matches if matching is random",
       subtitle = "Expectation is not perfectly linear with bias",
       x = "Bias (e.g. data consists of X% A vs. 1-(X%/2) B and C observations)",
       y = "Simulated match rate") +
  theme_minimal()
