## Hypothesis 2 and 3

# Hierarchical Model

library(simr)
library(tidyverse)

num_days <- 7
days <- factor(1:num_days)

num_participants <- 100
participants <- factor(1:num_participants)

# Create a long table of data
mydata <- expand.grid("Participant ID" = participants) %>% 
  as_tibble()

# Base rate for 

avoidance_means <- rnorm(num_participants)
avoidance_sds <- rnorm(num_participants)^2

approach_means <- rnorm(num_participants)
avoidance_sds <- rnorm(num_participants)^2


func_create_within_data <- function(participant_id){ rnorm(num_days,
                                 mean = avoidance_means[participant_id],
                                 sd = avoidance_sds[participant_id])}


# Participant's avoidance and approach scores
for(i in 1:num_participants){
  mydata <- mydata %>% 
    mutate(`Approach` = map(`Participant ID`, func_create_within_data))
}

#### Not Approach but time should be estimted...
# mydata %>% unnest(cols = c(Approach))

# Simulate the covariance
rmultinom(num_simulations, sample_size, probabilities)



# The model will be:
# engaged_vs_disengaged ~ approach_scores + avoidance_scores + (1|`Participant ID`/`Day`)
# engaged_vs_disengaged ~ RSA + (1|`Participant ID`/`Day`)



# Power Analysis with simr
# https://humburg.github.io/Power-Analysis/simr_power_analysis.html


