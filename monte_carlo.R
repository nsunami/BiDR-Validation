# Monte Carlo Simulation for Approach Avoidance 

runs <- 100000
approach <- rnorm(runs,mean=1,sd=10)
mc.integral <- sum(sims >= 3 & sims <= 6)/runs


# Parameters
# Approach scores
#   Distribution: normal
#   Mean: 

# Avoidance scores
#   Distribution: normal
#   Mean: 
#   SD:

# Behavior Type
#   Distrubiton: Binomial
#   Base rate: 50-50?



