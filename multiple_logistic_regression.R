# A logistic regression with two correlated predictors
library(tidyverse)
library(compute.es)
library(furrr) # gain was minimum --- dropping for now 10.28355 secs vs 10.98571 secs
plan(multicore)

social_mean_es<- res(r = .21, var.r = .15^2, n = 322)
social_mean_lor <- social_mean_es$l.lor

num_replications <- 1000
sample_sizes <- seq(100, 1000, 100)
# sample_size <- 300 
r <- 0.03 # from Elliot
covariance_matrix <- matrix(c(1, r, r, 1), nrow=2)

# initialize the ouput table
output_table <- expand.grid("Replication #" = 1:num_replications,
                           "Sample Size" = sample_sizes) %>% 
  as_tibble()

create_dataset <- function(input_sample_size,
                           cov_matrix = covariance_matrix,
                           bx1 = social_mean_lor,
                           bx2 = social_mean_lor){
  data <- MASS::mvrnorm(n=input_sample_size,
                        mu=c(0, 0), # Mean of zero (standardized)
                        Sigma=cov_matrix,
                        empirical=TRUE)
  x1 <- data[,1]
  x2 <- data[,2]
  # the linear combination in bias expressed in
  # log-odds ratio (raw coefficients)
  z = 1 + bx1*x1 - bx2*x2        # linear combination with a bias (outcome) --- 
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  y = rbinom(input_sample_size, 1, pr)      # bernoulli response variable
  #now feed it to glm:
  output_df <- tibble(y = y, x1 = x1, x2 = x2)
  return(output_df)
}

glm_fun <- function(df){glm(y ~ x1 + x2, data=df, family="binomial")}

# # Create simulated datasets
# time_now <- Sys.time()
# output_table <- output_table %>%
#   mutate(`Sample df` = future_map(`Sample Size`,
#                               create_dataset))
# (time_duration <- Sys.time() - time_now) # Time difference of 26.11223 secs

# Create simulated datasets (without future_map)
time_now <- Sys.time()
output_table <- output_table %>%
  mutate(`Sample df` = future_map(`Sample Size`,
                                  create_dataset))
(time_duration <- Sys.time() - time_now)

# run GLM for each of the datasets
output_table <- output_table %>%
  mutate(`GLM Object` = future_map(`Sample df`, glm_fun))

# Create summary of GLM objects
output_table <- output_table %>%
  mutate(`GLM Summary` = future_map(`GLM Object`, summary))

# Extract table of coefficients
output_table <- output_table %>%
  mutate(`Coefficients` = future_map(`GLM Summary`, function(x) as_tibble(pluck(x, "coefficients")))) 

# Extract P-values
output_table <- output_table %>%
  mutate(x1_pvalue = map_dbl(Coefficients, function(x) x$`Pr(>|z|)`[2]), # for x1
         x2_pvalue = map_dbl(Coefficients, function(x) x$`Pr(>|z|)`[3])) # for x2

# Compute Type I error Rate
summary_table <- output_table %>% group_by(`Sample Size`) %>%
  summarise(`x1 - Type 2 Error rate at .05` = sum(x1_pvalue > .05)/n(),
            `x1 - Power at .05` = 1 - `x1 - Type 2 Error rate at .05`)
summary_table

# plot p-values
output_table %>% 
  ggplot(aes(x = x1_pvalue)) +
  geom_histogram() + 
  facet_wrap(~ `Sample Size`)
# 
# 
# for(i in 1:length(sample_size)){
#   current_sample_size <- sample_size[i]
#   for(replication_number in 1:num_replications){
#     # create a matrix of two correlated variables
#     data <- MASS::mvrnorm(n=current_sample_size,
#                           mu=c(0, 0), # Mean of zero (standardized)
#                           Sigma=covariance_matrix,
#                           empirical=TRUE)
#     x1 <- data[,1]
#     x2 <- data[,2]
#     # the linear combination in bias expressed in
#     # log-odds ratio (raw coefficients)
#     z = 1 + social_mean_lor*x1 - social_mean_lor*x2        # linear combination with a bias (outcome) --- 
#     pr = 1/(1+exp(-z))         # pass through an inv-logit function
#     y = rbinom(current_sample_size, 1, pr)      # bernoulli response variable
#     #now feed it to glm:
#     df <- data.frame(y = y, x1 = x1, x2 = x2)
#     resulting_model <- glm(y ~ x1 + x2, data=df, family="binomial")
#     resulting_model_container[[i]] <- resulting_model
#     resulting_model_summary <- summary(resulting_model)
#     resulting_model_coeficients[[i]] <-  resulting_model_summary$coefficients
#     p_values_x1[[paste("N =", current_sample_size)]] <- resulting_model_summary$coefficients[2,4]
#     # results_df <- resulting_model_coeficients
#     
#     output_table <- output_table %>%
#       mutate(`glm obj` = case_when(
#         (`Replication #` == replication_number) & (`Sample Size` == current_sample_size) ~ list(resulting_model)
#       ))
#   }
#   
# }
