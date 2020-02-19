### Chi-square
library(tidyverse)
library(furrr)
plan(multicore)

num_simulations <- 3000
probabilities <- c(33,33,33,1)
target_sample_sizes <- seq(100, 500, 50)

h1_results <- expand_grid("Sample Size" = target_sample_sizes)
# stats::rmutinom
# Function to create a sample given a sample size
create_sample_lists <- function(sample_size){
  sim_samples_matrix <- rmultinom(num_simulations, sample_size, probabilities)
  sample_lists <- split(sim_samples_matrix,
                        rep(1:ncol(sim_samples_matrix), each = nrow(sim_samples_matrix)))
  return(sample_lists)
}

# create samples
h1_results <- h1_results %>%
  mutate(`Simulated Sample` = future_map(`Sample Size`, create_sample_lists, .progress = TRUE))

# unnest the table
h1_results <- h1_results %>% unnest(cols = c(`Simulated Sample`))

# for each sample apply a function to create a chisqare object
h1_results <- h1_results %>% 
  mutate("Chi Square Results" = map(`Simulated Sample`, function(x) chisq.test(x)))

# extract p-value for each result
h1_results <- h1_results %>% 
  mutate("p-value" = map_dbl(`Chi Square Results`, function(x) x$p.value))

# calculate power
summary_table <- h1_results %>% 
  group_by(`Sample Size`) %>%
  summarise(`Type 2 Error rate at .05` = sum(`p-value` > .05)/n(),
            `Power at .05` = 1 - `Type 2 Error rate at .05`)
summary_table

# Histograms
h1_results %>% 
  ggplot(aes(x = `p-value`)) +
  geom_histogram() + 
  facet_wrap(~ `Sample Size`)


