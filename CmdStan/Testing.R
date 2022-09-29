#################################################################################
# Preamble
library(brms)
library(bayesplot)
library(tidyverse)
library(cmdstanr)
set_cmdstan_path(path = "C:\\Users\\cch80\\miniconda3\\envs\\stan\\Library\\bin\\cmdstan")
cmdstan_path()
cmdstan_version(error_on_NA = TRUE)
###############################################################################
# Testing cmdstanr
# Compiling a model
file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstanr::cmdstan_model(file)
mod$print()
mod$exe_file()
# Running MCMC
# names correspond to the data block in the Stan program
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

fit <- mod$sample(
  data = data_list, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)
fit$summary()
##########################################

hospital <- c("a","b","c","d","e","f")
numerator <- c(25, 32, 34, 11, 21, 29)
denominator <- c(38, 48, 63, 33, 38, 51)
data <- data.frame(hospital, numerator, denominator)
head(data)

PR_model = 
  # brm is the workhorse function which estimates the specified model
  # Here we specify that we have proportion data with numerators and
  # denominators, and these are clustered within hospitals
  brms::brm(numerator | trials(denominator) ~ 1 +  hospital, data, 
      # The family function is essential to ensure that the right model is
      # applied to the dataset
      family = binomial(),
      # These parameters are for optimising performance, don't worry about them
      # for now
      normalize = FALSE, backend = "cmdstanr", cores = 4,
      # Suppress printing output progress
      silent = 2, refresh = 0)





