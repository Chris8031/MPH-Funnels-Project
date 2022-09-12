#########################################################
#Preamble
library(tidyverse)
library(DescTools)
library(readxl)
library("PEIP")
Dataset <- read_excel("Vidmar's Dataset/Dataset.xlsx")
df1 <- Dataset %>%
  mutate(hospital_ID = row_number()) %>%
  rename(numerators = Numerator,
         denominators = Denominator)
# Vidmar's method adjusts confidence level to sample size. 
# This is based on a Bayesian-influenced variation of 
# Chauvenet's criterion (pg 349 Vidmar & Blagus 2013)
adjustment <- function(N){
  a <-(N-0.5)/N
  b <- a + (1-a)/2
  c <- PEIP::tinv(b,N-1)
  return(c)
}
# Name variables
df2 <- df1 %>%
  mutate(sqrt_x = sqrt(numerators),
         sqrt_nx = sqrt(denominators-numerators),
         pi = numerators/denominators)
glimpse(df2)
# Fit linear model through origin
lm_df2 <- lm(sqrt_x ~ sqrt_nx+0, data = df2)
lm_df2
df3 <- df2 %>%
  mutate(b = lm_df2$coefficients, # Add slope (b) from model to dataset
         res = sqrt_x - (sqrt_nx*b), # Add residuals 
         'n-1' = nrow(df2)-1,
         MSE = ((sum(res^2))/`n-1`))
glimpse(df3)
# Double checking MSE value
summ <- summary(lm_df2)
MSE2 <- mean(summ$residuals^2)
MSE2
# 0.1 difference amongst the two. Unclear as to why.
df4 <- df3 %>%
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
         LCL_p = LCL_x/denominators
         )
glimpse(df4)
# Plot Vidmar's chart
vidmar_plt1 <- ggplot(df4, aes(x = denominators, 
                              y = pi)) +
  geom_point() +
  geom_line(aes(y=theta2)) +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=UCL_p)) +
  geom_line(aes(y=LCL_p))
vidmar_plt1
# Create table for outliers
outliers_vidmar <- df4 %>%
  filter(pi >= UCL_p |
           pi <= LCL_p) 
view(outliers_vidmar)
