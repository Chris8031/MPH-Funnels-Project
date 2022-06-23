#########################################################
#Preamble
library(tidyverse)
library(DescTools)
library(checkmate)
# set critical values for control limits
a99 <-  qnorm(.999)  
a95 <- qnorm(.975)
# Simulate dataset for proportion indicator
set.seed(80)
denominators <- sample(1:20, size = 100, replace = T)
numerators <-  rbinom(n=100, size = denominators, p = 0.5)
hospital.fn <- function(length.out) {
  a <- rep(letters, length.out = length.out)
}
raw_data <- data.frame(numerators, denominators, hospital = hospital.fn(nrow(raw_data)))
df1 <- raw_data
head(df1)
glimpse(df1)
# First prep for double-square-root charts
df1 <- df1 %>%
  mutate(sqrt_x = sqrt(numerators),
         sqrt_nx = sqrt(denominators-numerators))
# fit regression model through the origin
lm_df1 <- lm(sqrt_x ~ sqrt_nx + 0, data=df1)
summary(lm_df1)
df2 <- df1 %>%
  mutate(slope = lm_df1$coefficients)
glimpse(df2)
df3 <-  df2 %>%
  mutate(sqrt_x_pred = predict(lm_df1))
head(df3)
##########################################
# Two methods to calculate MSE
# https://www.statology.org/how-to-calculate-mse-in-r/
  # Method 1
MSE1 <- mean((df3$sqrt_x - df3$sqrt_x_pred)^2)
MSE1
  # Method 2
summ <- summary(lm_df1)
MSE2 <- mean(summ$residuals^2)
MSE2
# Both methods should give the same score
assert_set_equal(MSE1, MSE2)
########################################################
# Calculate SSQ
df4 <- df3 %>%
mutate(MSE = MSE1) %>%
mutate(SSQ = sum(denominators-numerators))
head(df4)
df5 <- df4 %>%
  mutate(delta = (numerators + denominators)/SSQ) %>%
  mutate(delta = delta + 1) %>%
  mutate(delta = MSE*delta) %>%
  mutate(delta = sqrt(delta))
head(df5)
