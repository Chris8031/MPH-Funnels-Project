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
id <- ids::random_id(100, bytes = 2) # generate random IDs for hospitals
# 400 chosen as the this approximately the max number of surguries for the 
# AAA surgeries group. 
denominator <- sample(1:400, size = 100)
denominator
# Generate randomised parameters for generating numerator
numerator_parameter_dataset <- rnorm(10000, mean = 0.5, sd = 0.025)
p <- sample(numerator_parameter_dataset, 1)
p
outlier_mean <- 0.5 + 3.5*0.025
outlier_dataset <- rnorm(10000, mean = outlier_mean, sd = 0.01)
outlier_p <- sample(outlier_dataset, 1)
outlier_p
# generate observed counts
numerator <- 
  c(
    rbinom(n = 99, size = denominators[1:99], prob = p),
    rbinom(n = 1, size = denominator[100], prob = outlier_p)
  )
data <- data.frame(
  # iteration = iteration
  id,
  numerator,
  denominator,
  true_outlier = c(rep(FALSE, 99), rep(TRUE, 1))
)
# Vidmar's method adjusts confidence level to sample size. 
# This is based on a Bayesian-influenced variation of 
# Chauvenet's criterion (pg 349 Vidmar & Blagus 2013)
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
         pi = numerators/denominators) %>%
  filter(pi <= 1)
# Fit linear model through origin
lm_df2 <- lm(sqrt_x ~ sqrt_nx+0, data = df2)
lm_df2
df3 <- df2 %>%
  mutate(b = lm_df2$coefficients, # Add slope (b) from model to dataset
         res = sqrt_x - (sqrt_nx*b), # Add residuals 
         'n-1' = nrow(df2)-1,
         MSE = ((sum(res^2))/`n-1`))
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

# Plot Vidmar's chart
vidmar_plt1 <- ggplot(df4, aes(x = denominators, 
                               y = pi)) +
  geom_point() +
  geom_line(aes(y=theta2)) +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=UCL_p)) +
  geom_line(aes(y=LCL_p))
vidmar_plt1










