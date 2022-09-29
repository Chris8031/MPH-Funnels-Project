# Phase 2 Proportions
#####################################################
# Preable
# Load Packages
library(tidyverse)
library(DescTools)
library(MASS)
library(readxl)
library(xtable)
# Import Data
Du_et_al_2018_Data <- read_excel("Real world data application/Du et al Dataset/Du et al (2018) Data.xlsx", 
                                 +     sheet = "AAA Repairs")
hist(Du_et_al_2018_Data$Surgeries)
#################################################################
# Vidmar approach
# Rename variables in dataset
df1 <- Du_et_al_2018_Data %>%
  dplyr::select(-Expected, -Proportions) %>%
  mutate(hospital_ID = row_number()) %>%
  rename(numerators = Observed,
         denominators = Surgeries)
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
################################################################################
# Function for calculating control limits multiplier
laneys_cl.fn <- function(x) {
  y <- x + ((1-x)/2)
  z <- qnorm(y)
  return(z)
}
# for 99cl 
cl <- laneys_cl.fn(0.997)
cl
glimpse(df4)
laneys_df1 <- data.frame(df4[, 1:3], df4[, 20:21])
ldf2 <- laneys_df1 %>%
  rename(UCL_Vidmar = UCL_p,
         LCL_Vidmar = LCL_p) %>%
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
# not sure why sz = sigma_z
# Create sigma_piz
ldf6 <- ldf5 %>%
  mutate(sigma_piz = spi*sigma_z)
glimpse(ldf6)
# create control limits (= 3 used for now).
# However this value can be obtained from Spiegelhalter's MAM model as
# shown in Vidmar's excel sheet
ldf7 <- ldf6 %>%
  mutate(ul99_laney = pm + cl*sigma_piz,
         ll99_laney = pm - cl*sigma_piz)
glimpse(ldf7)
################################################################################
MAMdf <- data.frame(
  ldf7[, 1:9],
  ldf7[, 18:19]
)
MAMdf2 <- MAMdf %>%
  mutate(trans_pi = asin(sqrt(pi)), # Equation 3.4
         trans_pm = asin(sqrt(pm)), # Equation 3.5
         trans_spi = 1/(2*sqrt(ni)), # Equation 3.6
# transformed z-score calculation and winsorise
        trans_zi = (trans_pi-trans_pm)/trans_spi)
MAMdf3 <- MAMdf2 %>%
  mutate(
    z_adj = Winsorize(MAMdf2$trans_zi, 
                      probs = c(0.10, 0.90)),
    phi = sum(z_adj^2/nrow(df4))
  )

glimpse(MAMdf3)
# Check for overdispersion
MAMdf3$phi > (nrow(MAMdf3)-1)/nrow(MAMdf3)
# Construct funnel plots
MAMdf4 <- MAMdf3 %>%
  mutate(ll99_MAM = pm - cl*spi*sqrt(phi),
         ul99_MAM = pm + cl*spi*sqrt(phi))
glimpse(MAMdf4)
###############################################################################  
# AREM

AREMdf <- MAMdf4 %>%
  mutate(s_squared_i = (pi*(1-pi))/ni, # Equation 3.12 in Dissertation
         w = 1/s_squared_i,
         tau_numerator = phi*nrow(MAMdf4) - (nrow(MAMdf4) - 1)) %>%
        filter(w != Inf) %>%
  mutate(tau_demoniator = sum(w) - (sum(w^2)/sum(w)),
         tau_squared = tau_numerator/tau_demoniator)

glimpse(AREMdf)

AREMdf2 <- AREMdf %>%
  mutate(
    ll99_AREM = pm - cl*(spi+sqrt(tau_squared)),
    ul99_AREM = pm + cl*(spi+sqrt(tau_squared))
  )
glimpse(AREMdf2)

ggplot(AREMdf2, aes(x=ni, y=pi,
)) +
  geom_point() +
  geom_line(aes(y=pm)) +
  geom_line(aes(y=ll99_laney, color = "Laney")) +
  geom_line(aes(y=ul99_laney, color = "Laney")) + 
  geom_line(aes(y = UCL_Vidmar, color = "Vidmar")) +
  geom_line(aes(y = LCL_Vidmar, color = "Vidmar")) +
  geom_line(aes(y = ll99_MAM, color = "MAM")) +
  geom_line(aes(y = ul99_MAM, color = "MAM")) +
  geom_line(aes(y = ll99_AREM, color = "AREM")) +
  geom_line(aes(y = ul99_AREM, color = "AREM")) +
  labs(title = "Funnel Plot Models fitted to AAA repairs dataset",
       x = "Volume of AAA repairs",
       y = "Adjusted in-hospital mortality rate",
      color = "99.7 Control Limits")

# Make table of outliers
glimpse(AREMdf2)


first_column <- c("Model", "Above 99CL", "Below 99CL")
data1 <- data.frame("Laney", 
             sum(AREMdf2$pi > AREMdf2$ul99_laney), 
             sum(AREMdf2$pi < AREMdf2$ll99_laney))
colnames(data1) <- first_column
data1[2,] <- c("Vidmar", 0, 0)
data1[3,] <- c("MAM", 0, 0)
data1[4,] <- c("AREM", 0, 0)
table_outlier <- data1
table_outlier
# Write table for LaT
xtable::xtable(table_outlier)
#############################################################################
# BGLMM
BGLMM <- AREMdf2[,1:3]
glimpse(BGLMM)
library(brms)
library(bayesplot)


data <- BGLMM %>%
  rename(
    hospital = hospital_ID,
    numerator = xi,
    denominator = ni
  )
glimpse(data)

PR_model = 
  # brm is the workhorse function which estimates the specified model
  # Here we specify that we have proportion data with numerators and
  # denominators, and these are clustered within hospitals
  brms::brm(numerator | trials(denominator) ~ 1 + (1 | hospital), data, 
      # The family function is essential to ensure that the right model is
      # applied to the dataset
      family = binomial(),
      # These parameters are for optimising performance, don't worry about them
      # for now
      normalize = FALSE, backend = "cmdstanr", cores = 4,
      # Suppress printing output progress
      silent = 2, refresh = 0)

