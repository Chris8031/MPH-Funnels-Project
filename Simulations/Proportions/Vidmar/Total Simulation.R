#################
# Preable
# Load Packages
library(FunnelPlotR)
library(tidyverse)
library(DescTools)
library(MASS)
library(ids)
library(furrr)
library(readxl)
set.seed(8)
###################################################
vidmar_simulation.fn <- function(iteration) {
  id <- ids::random_id(20, bytes = 2) # generate random IDs for hospitals
  # 300 chosen as the this approximately the max number of surguries for the 
  # AAA surgeries group. 
  denominators <- sample(1:300, size = 20)
  denominators
  # Generate randomised parameters for generating numerator
  numerator_parameter_dataset <- rnorm(10000, mean = 0.5, sd = 0.025)
  # 95% of the time, we'd like p to be between 0.45 and 0.55
  p <- sample(numerator_parameter_dataset, 1)
  p
  # Generate outlier mean parameter
  outlier_mean_parameter <- mean(p) + (3.5*sd(numerator_parameter_dataset))
  outlier_mean_parameter
  outlier_dataset <- rnorm(10000, mean = outlier_mean_parameter, sd = 0.01)
  outlier_dataset
  outlier_p <- sample(outlier_dataset, 1)
  outlier_p
  # generate observed counts using paramaters
  # One outlier encoded
  numerators <- 
    c(
      rbinom(n = 19, size = denominators[1:19], prob = p),
      rbinom(n = 1, size = denominators[20], prob = outlier_p)
    )
  numerators
  data <- data.frame(
    #  iteration = iteration,
    id,
    numerators,
    denominators,
    true_outlier = c(rep(FALSE, 19), rep(TRUE, 1))
  )

  # Vidmar's method adjusts confidence level to sample size. 
  # This is based on a Bayesian-influenced variation of 
  # Chauvenet's criterion (pg 349 Vidmar & Blagus 2013)
  # Function will be used in generating control limits
adjustment <- function(N){
    a <-(N-0.5)/N
    b <- a + (1-a)/2
    c <- PEIP::tinv(b,N-1)
    return(c)
  }
  
  # Rename to suit Vidmar's code
  df1 <-data %>%
    rename(
      denominators = denominator,
      numerators = numerator
    )
  glimpse(df1)
  # Name variables
  df2 <- df1 %>%
    mutate(sqrt_x = sqrt(numerators),
           sqrt_nx = sqrt(denominators-numerators),
           pi = numerators/denominators)
  
  # Fit linear model through origin
  lm_df2 <- lm(sqrt_x ~ sqrt_nx+0, data = df2)
  lm_df2
  df3 <- df2 %>%
    mutate(b = lm_df2$coefficients, # Add slope (b) from model to dataset
           res = sqrt_x - (sqrt_nx*b), # Add residuals 
           'n-1' = nrow(df2)-1,
           MSE = ((sum(res^2))/`n-1`))
  # Equations outlined in dissertation
  df3 %>%
    mutate(nx = sqrt_nx^2,
           SSQ = sum(nx),
           delta = adjustment(nrow(df3))*sqrt(MSE*(1+(nx/SSQ))),
           theta = b^2/(1+b^2),
           theta2 = sum(numerators)/sum(denominators),
           theta_x = theta*denominators,
           sqrt_theta_x = sqrt(theta_x),
           UCL_x = (sqrt_theta_x + delta)^2,
           LCL_x = sign(sqrt_theta_x - delta)*(sqrt_theta_x - delta)^2,
           UCL_p = UCL_x/denominators,
           LCL_p = LCL_x/denominators,
           outlier = (pi < LCL_p) | (pi > UCL_p)
    )
}
######################################################################
# Simulations for 100
future::plan(multisession)
vidmar_simulation_100 <- future_map_dfr(1:100,
                                       vidmar_simulation.fn,
                                       
                                       .progress = TRUE,
                                       
                                       .options = furrr_options(seed = 8)
)
plan(sequential)

vidmar_confusion_iter_100 <- vidmar_simulation_100 %>%
  group_by(iteration) %>%
  summarise(
    true_positive = sum(outlier == TRUE & true_outlier == TRUE),
    false_positive = sum(outlier == TRUE & true_outlier == FALSE),
    true_negative = sum(outlier == FALSE & true_outlier == FALSE),
    false_negative = sum(outlier == FALSE & true_outlier == TRUE),
    total_number_outliers_labelled = sum(outlier == TRUE),
    sensitivity = true_positive / (true_positive + false_negative),
    specificity = true_negative / (false_positive + true_negative),
    outlier_found = ifelse(true_positive > 0, TRUE, FALSE),
    number_of_false_outliers = false_positive,
    number_correct = sum(outlier == true_outlier),
    number_incorrect = sum(outlier != true_outlier),
    predictive_accuracy = number_correct/(number_correct + number_incorrect),
    test_error_rate = 1 - predictive_accuracy
  ) %>%
  ungroup()
vidmar_summary_100 <- vidmar_confusion_iter_100 %>%
  summarise(
    "Iterations" = n(),
    "Mean TP" = mean(true_positive),
    "Mean FP" = mean(false_positive),
    "Mean TN" = mean(true_negative),
    "Mean FN" = mean(false_negative),
    "Mean Outliers" = mean(total_number_outliers_labelled),
    "Sensitivity" = mean(sensitivity),
    "Specificity" = mean(specificity),
    "Predictive Accuracy" = mean(predictive_accuracy)
    
  )
vidmar_summary_100
###############################################################
# Simulations for 1000
future::plan(multisession)
vidmar_simulation_1000 <- future_map_dfr(1:1000,
                                        vidmar_simulation.fn,
                                        
                                        .progress = TRUE,
                                        
                                        .options = furrr_options(seed = 8)
)
plan(sequential)

vidmar_confusion_iter_1000 <- vidmar_simulation_1000 %>%
  group_by(iteration) %>%
  summarise(
    true_positive = sum(outlier == TRUE & true_outlier == TRUE),
    false_positive = sum(outlier == TRUE & true_outlier == FALSE),
    true_negative = sum(outlier == FALSE & true_outlier == FALSE),
    false_negative = sum(outlier == FALSE & true_outlier == TRUE),
    total_number_outliers_labelled = sum(outlier == TRUE),
    sensitivity = true_positive / (true_positive + false_negative),
    specificity = true_negative / (false_positive + true_negative),
    outlier_found = ifelse(true_positive > 0, TRUE, FALSE),
    number_of_false_outliers = false_positive,
    number_correct = sum(outlier == true_outlier),
    number_incorrect = sum(outlier != true_outlier),
    predictive_accuracy = number_correct/(number_correct + number_incorrect),
    test_error_rate = 1 - predictive_accuracy
  ) %>%
  ungroup()
vidmar_summary_1000 <- vidmar_confusion_iter_1000 %>%
  summarise(
    "Iterations" = n(),
    "Mean TP" = mean(true_positive),
    "Mean FP" = mean(false_positive),
    "Mean TN" = mean(true_negative),
    "Mean FN" = mean(false_negative),
    "Mean Outliers" = mean(total_number_outliers_labelled),
    "Sensitivity" = mean(sensitivity),
    "Specificity" = mean(specificity),
    "Predictive Accuracy" = mean(predictive_accuracy)
    
  )
vidmar_summary_1000
########################################################################
# Simulations for 10000
future::plan(multisession)
vidmar_simulation_10000 <- future_map_dfr(1:10000,
                                        vidmar_simulation.fn,
                                        
                                        .progress = TRUE,
                                        
                                        .options = furrr_options(seed = 8)
)
plan(sequential)

vidmar_confusion_iter_10000 <- vidmar_simulation_10000 %>%
  group_by(iteration) %>%
  summarise(
    true_positive = sum(outlier == TRUE & true_outlier == TRUE),
    false_positive = sum(outlier == TRUE & true_outlier == FALSE),
    true_negative = sum(outlier == FALSE & true_outlier == FALSE),
    false_negative = sum(outlier == FALSE & true_outlier == TRUE),
    total_number_outliers_labelled = sum(outlier == TRUE),
    sensitivity = true_positive / (true_positive + false_negative),
    specificity = true_negative / (false_positive + true_negative),
    outlier_found = ifelse(true_positive > 0, TRUE, FALSE),
    number_of_false_outliers = false_positive,
    number_correct = sum(outlier == true_outlier),
    number_incorrect = sum(outlier != true_outlier),
    predictive_accuracy = number_correct/(number_correct + number_incorrect),
    test_error_rate = 1 - predictive_accuracy
  ) %>%
  ungroup()
vidmar_summary_10000 <- vidmar_confusion_iter_10000 %>%
  summarise(
    "Iterations" = n(),
    "Mean TP" = mean(true_positive),
    "Mean FP" = mean(false_positive),
    "Mean TN" = mean(true_negative),
    "Mean FN" = mean(false_negative),
    "Mean Outliers" = mean(total_number_outliers_labelled),
    "Sensitivity" = mean(sensitivity),
    "Specificity" = mean(specificity),
    "Predictive Accuracy" = mean(predictive_accuracy)
    
  )
vidmar_summary_10000
##############################################################
# Simulation for Laney
laney_simulation.fn <- function(iteration){
  # Data generation
  id <- ids::random_id(20, bytes = 2) # generate random IDs for hospitals
  # 300 chosen as the this approximately the max number of surguries for the 
  # AAA surgeries group. 
  denominators <- sample(1:300, size = 20)
  denominators
  # Generate randomised parameters for generating numerator
  numerator_parameter_dataset <- rnorm(10000, mean = 0.5, sd = 0.025)
  # 95% of the time, we'd like p to be between 0.45 and 0.55
  p <- sample(numerator_parameter_dataset, 1)
  p
  # Generate outlier mean parameter
  outlier_mean_parameter <- mean(p) + (3.5*sd(numerator_parameter_dataset))
  outlier_mean_parameter
  outlier_dataset <- rnorm(10000, mean = outlier_mean_parameter, sd = 0.01)
  outlier_dataset
  outlier_p <- sample(outlier_dataset, 1)
  outlier_p
  # generate observed counts using paramaters
  # One outlier encoded
  numerators <- 
    c(
      rbinom(n = 19, size = denominators[1:19], prob = p),
      rbinom(n = 1, size = denominators[20], prob = outlier_p)
    )
  numerators
  data <- data.frame(
    iteration = iteration,
    id,
    numerators,
    denominators,
    true_outlier = c(rep(FALSE, 19), rep(TRUE, 1))
  )

# Function for calculating control limits multiplier
laneys_cl.fn <- function(x) {
  y <- x + ((1-x)/2)
  z <- qnorm(y)
  return(z)
}
# for 99cl 
cl <- laneys_cl.fn(0.997)
cl
glimpse(data)
laneys_df1 <- data
ldf2 <- laneys_df1 %>%
  rename(numerators = numerator,
         denominators = denominator) %>%
  mutate(
    pi = (numerators/denominators),
    pm = sum(numerators)/sum(denominators)
  ) %>%
  rename(xi = numerators,
         ni = denominators)
# Laney's approach based on Original paper
# Compute the standard deviation for each proportion
ldf3 <- ldf2 %>%
  mutate(spi = sqrt((pm*(1-pm))/ni))
# Create standardised z-scores
ldf4 <- ldf3 %>%
  mutate(z_i = (pi-pm)/spi)
head(ldf4)
# Compute the standard deviation of the z_i values
ldf5 <- ldf4 %>%
  mutate(z_bar = mean(z_i)) %>%
  mutate(z_diff = z_i - z_bar) %>%
  mutate(sum_z_diff_squared = sum(z_diff^2),
         N = nrow(df4)) %>%
  mutate(sigma_z_squared = sum_z_diff_squared/N-1) %>%
  mutate(sigma_z = sqrt(abs(sigma_z_squared))) %>%
  mutate(sz = sd(z_i))
glimpse(ldf5)
# Create sigma_piz
ldf6 <- ldf5 %>%
  mutate(sigma_piz = spi*sigma_z)
glimpse(ldf6)
# create control limits (= 3 used for now).
# However this value can be obtained from Spiegelhalter's MAM model as
# shown in Vidmar's excel sheet
ldf6 %>%
  mutate(ul99_laney = pm + cl*sigma_piz,
         ll99_laney = pm - cl*sigma_piz,
         outlier = (pi < ll99_laney) | (pi > ul99_laney))
}
############################################################################
# Run simulations
future::plan(multisession)
laney_simulation <- future_map_dfr(
  1:100,
  laney_simulation.fn,
  .progress = TRUE,
  .options = furrr_options(seed = 8)
)
plan(sequential)

laney_confusion_iter <- laney_simulation %>%
  group_by(iteration) %>%
  summarise(
    true_positive = sum(outlier == TRUE & true_outlier == TRUE),
    false_positive = sum(outlier == TRUE & true_outlier == FALSE),
    true_negative = sum(outlier == FALSE & true_outlier == FALSE),
    false_negative = sum(outlier == FALSE & true_outlier == TRUE),
    total_number_outliers_labelled = sum(outlier == TRUE),
    sensitivity = true_positive / (true_positive + false_negative),
    specificity = true_negative / (false_positive + true_negative),
    outlier_found = ifelse(true_positive > 0, TRUE, FALSE),
    number_of_false_outliers = false_positive,
    number_correct = sum(outlier == true_outlier),
    number_incorrect = sum(outlier != true_outlier),
    predictive_accuracy = number_correct/(number_correct + number_incorrect),
    test_error_rate = 1 - predictive_accuracy
  ) %>%
  ungroup()
view(laney_confusion_iter)
laney_summary <- laney_confusion_iter %>%
  summarise(
    "Control Limits" = "Laney",
    "Iterations" = n(),
    "Mean TP" = mean(true_positive),
    "Mean FP" = mean(false_positive),
    "Mean TN" = mean(true_negative),
    "Mean FN" = mean(false_negative),
    "Mean Outliers" = mean(total_number_outliers_labelled),
    "Sensitivity" = mean(sensitivity),
    "Specificity" = mean(specificity),
    "Predictive Accuracy" = mean(predictive_accuracy)
    
  )
laney_summary