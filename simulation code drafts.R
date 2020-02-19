probabilities <- list(c(33,33,33,1))








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

# Assumptions: people with +1SD approach scores are 


# Libraries that may help:
# MCMCPack
# SIMR
#



# Are we sampling the regression coefficients for the avoidance and approach?


# Probability distribution of the slope

# Use this linear regression example for a code?
#   https://bookdown.org/ccolonescu/RPoE4/simplelm.html#monte-carlo-simulation




set.seed(8372)


sample_size <- 300
# For one-unit change in avoidance score 
# one SD unit change in avoidance should 
# correspond with OR = 1.68 (small effect) --- Chen 2010
b_avoidance_odds <- NA
b_avoidance_logodds <- exp(b_avoidance_odds)

# Elliot Study 6 had the largest N 
#   From the paper:
    # The measures of approach temperament (M = 30.75, SD = 5.44, range = 10–42) and
    # avoidance temperament (M = 24.51, SD = 8.58, range57–42) each demonstrated
    # good internal consistency, a5.86 and .82, respectively. The two measures were
    # correlated at r = .03, ns.


## # # # # # # # # # # # # # # # # # # # # # # # # 
#  Instead Use Logistic Regression Model
# # # # # # # # # # # # # # # # # # # # # # # # # 
# wp.logistic method
library(WebPower)

wp.logistic(p0 = .50, p1 = .50,
            alpha = .05, power = .99,
            family = "normal")
### Error--- we need n

# 
mean_avoidance <- 30.75
sd_avoidance <- 5.44
mean_approach <- 24.51
sd_approach <- 8.58

ns <- seq(100, 500, 100)
avoidance_wp <- wp.logistic(n = ns,
                            p0 = .50, # on average, assume that we have an equal chance of engaged or disengaged
                            p1 = 0.6855346,
                            alpha = .05,
                            family = "normal",
                            parameter = c(0, 1))
avoidance_wp

# ,59

# .50 * OR?
#1:2 becomes 
# 2 * 2.18 = 4.36
# 1- 1/4.36
2.18/3.18

# 0.6855346 checks out with the calculations below


rtoodds <- function(r){
}


# One SD change in approach score corresponds with 
# 
#*** r=.21 as an average effect of social psych
library(compute.es)
res(r = .21, var.r = .15^2, n = 322)
# OR [ 95 %CI] = 2.18 [ 0.69 , 6.85 ] 
# p-value(OR) = 0.18 
# Log OR [ 95 %CI] = 0.78 [ -0.37 , 1.92 ] 
# var(lOR) = 0.34 
# p-value(Log OR) = 0.18 
### Every one-unit increase 

# 100% of responses --- 

# OR = hits/missess
# # 2.18 = hits/missess
# # since missess = alltrials - hits 
# # 2.18 = hits/ (100 - hits)
# # 2.18(100 - hits) = hits
# # 218 - 2.18*hits = hits
# # hits + 2.18*hits = 218
# # 3.18*hits = 218
# # hits = 218/3.18

proportion_hits <- 218/3.18
# [1] 68.55346



# RProE4 Method

N <- 40
x1 <- 10
x2 <- 20
b1 <- 100
b2 <- 10
mu <- 0
sig2e <- 2500
sde <- sqrt(sig2e)
yhat1 <- b1+b2*x1
yhat2 <- b1+b2*x2
curve(dnorm(x, mean=yhat1, sd=sde), 0, 500, col="blue")
curve(dnorm(x, yhat2, sde), 0,500, add=TRUE, col="red")
abline(v=yhat1, col="blue", lty=2)
abline(v=yhat2, col="red", lty=2)
legend("topright", legend=c("f(y|x=10)", 
                            "f(y|x=20)"), lty=1,
       col=c("blue", "red"))



### https://stats.stackexchange.com/questions/35940/simulation-of-logistic-regression-power-analysis-designed-experiments/36040#36040
mydat <- data.frame( v1 = rep( c(3,6,9), each=2 ),
                     v2 = rep( 0:1, 3 ), 
                     resp=c(0.0025, 0.00395, 0.003, 0.0042, 0.0035, 0.002) )

fit0 <- glm( resp ~ poly(v1, 2, raw=TRUE)*v2, data=mydat,
             weight=rep(100000,6), family=binomial)
b0 <- coef(fit0)
simfunc <- function( beta=b0, n=10000 ) {
  w <- sample(1:6, n, replace=TRUE, prob=c(3, rep(1,5)))
  mydat2 <- mydat[w, 1:2]
  eta <- with(mydat2,  cbind( 1, v1, 
                              v1^2, v2,
                              v1*v2,
                              v1^2*v2 ) %*% beta )
  p <- exp(eta)/(1+exp(eta))
  mydat2$resp <- rbinom(n, 1, p)
  
  fit1 <- glm( resp ~ poly(v1, 2)*v2, data=mydat2,
               family=binomial)
  fit2 <- update(fit1, .~ poly(v1,2) )
  anova(fit1,fit2, test='Chisq')[2,5]
}

out <- replicate(100, simfunc(b0, 10000))
mean( out <= 0.05 )
hist(out)
abline(v=0.05, col='lightgrey')


##
library(rms)

tmpfun <- function(n, beta0, beta1, beta2) {
  x <- runif(n, 0, 10)
  eta1 <- beta0 + beta1*x
  eta2 <- eta1 + beta2
  p1 <- exp(eta1)/(1+exp(eta1))
  p2 <- exp(eta2)/(1+exp(eta2))
  tmp <- runif(n)
  y <- (tmp < p1) + (tmp < p2)
  fit <- lrm(y~x)
  fit$stats[5] # get the p-value
}

pvalues <- replicate(1000, tmpfun(100, -1/2, 1/4, 1/4))
mean( pvalues < 0.05 )
hist(pvalues)



# This code takes too long..
sample_sizes <- 100:300
for(i in 1:length(sample_sizes)){
  pvalues <- replicate(1000, tmpfun(sample_sizes[i], -1/2, 1/4, 1/4))
  mean( pvalues < 0.05 )
}


library(tidyverse)
mytibble <- tibble()


# # # # # # # # # # # # # # # # # # # # # # # 
# Simulating
rbinom(1, 300, .50)
rbinom(1, 300, .69)

norm_var <- rnorm(300)
norm_var
hist(norm_var)

pnorm_norm <- pnorm(norm_var)
pnorm_norm
hist(pnorm_norm)


## https://stats.stackexchange.com/questions/46523/how-to-simulate-artificial-data-for-logistic-regression
#create data:
set.seed(666)
x1 = rnorm(1000)           # some continuous variables 
x2 = rnorm(1000)

# condition
# https://stats.stackexchange.com/questions/83172/generate-two-variables-with-precise-pre-specified-correlation
# https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables


# Correlation between approach and avoidance 
r <- 0.03 # from Elliot
covariance_matrix <- matrix(c(1, r, r, 1), nrow=2)
sample_size <- 300 
data <- mvrnorm(n=sample_size,
               mu=c(0, 0), # Mean of zero (standardized)
               Sigma=covariance_matrix,
               empirical=TRUE)

x1_approach <- data[,1]
x2_avoidance <- data[,2]
# the linear combination in bias expressed in
# log-odds ratio (raw coefficients)
z = 1 + 2*x1 + 3*x2        # linear combination with a bias (outcome) --- 
pr = 1/(1+exp(-z))         # pass through an inv-logit function
y = rbinom(sample_size, 1, pr)      # bernoulli response variable

#now feed it to glm:
df = data.frame(y=y,x1=x1,x2=x2)
glm( y~x1+x2,data=df,family="binomial")

library(tidyverse)
results_df <- tibble()
