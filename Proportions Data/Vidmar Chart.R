# Preamble
library(tidyverse)
library(DescTools)
# Generate Data with strong outliers/likely to have high overdispersion ratio
set.seed(3)
denominators <- sample(1:100, size = 100, replace = T)
denominators
# for numerators, make it a 30/40/30 split to ensure overdispersion
numerators <- c(rbinom(n=30, size = denominators[1:30], p = 0.2),
                rbinom(n=40, size = denominators[31:70], p = 0.5),
                rbinom(n=30, size = denominators[71:100], p = 0.8))
numerators               
hospital.fn <- function(length.out) {
  a <- rep(letters, length.out = length.out)
}
raw_data <- data.frame(numerators, denominators, hospital = hospital.fn(nrow(raw_data)))
View(raw_data)
# First prep for double-square-root charts
df <- raw_data
df1 <- df %>%
  mutate(sqrt_x = sqrt(numerators),
         sqrt_nx = sqrt(denominators-numerators))
lm_df1 <- lm(sqrt_x ~ sqrt_nx, data=df1)
df2 <- df1 %>%
  mutate(slope = lm_df1$coefficients[2])
df3 <-  df2 %>%
  mutate(sqrt_x_pred = slope*sqrt_nx)
head(df3)
df4 <- df3 %>%
  mutate(pred = predict(lm_df1))
head(df4)
# Two methods to calculate MSE
# https://www.statology.org/how-to-calculate-mse-in-r/
  # Method 1
MSE1 <- mean((df3$sqrt_x - df3$sqrt_x_pred)^2)
MSE1
  # Method 2
summ <- summary(lm_df1)
MSE2 <- mean(summ$residuals^2)
MSE2
MSE1 == MSE2
# As this is false, clearly I did something wrong in my calculations
# Will use MSE2 for now
df5 <- df4 %>%
  mutate(SSQ = sum(denominators-numerators)^2)
head(df5)
