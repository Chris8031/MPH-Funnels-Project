#########################
# Preamble 
library(tidyr)
library(tidyverse)
library(DescTools)
# set critical values for control limits
cl99 <- qnorm(.999)
cl95 <- qnorm(0.975)
#  Generate fake dataset for standardised ratios
set.seed(2)
# 80 as this is roughly the number of hospitals
# under WA Health
denominator <- sample(1:20, size = 80, replace = TRUE)
quantiled <- Quantile(denominator)
quantiled
# For numerators, using 25% and 75% quantiled to try and 
# ensure overdispersion for standardised ratios
numerator <-  c(rpois(40, lambda = quantiled[2]),
                 (rpois(40, lambda = quantiled[4])))
numerator
                df1 <- data.frame(numerator, denominator)
df1
df2 <- data.frame(numerator, denominator) %>%
  mutate(hospital_ID = row_number())
df2$hospital_ID
glimpse(df2)
df2
df3 <- df2 %>%
  mutate(SR_i = numerator/denominator,
         target = 1,
         s_i = sqrt(1/denominator)) # from Appendix A.1 Spiegelhalter 2012
glimpse(df3)
#################################
# Multiplicative Adjustment Model
# Constructing z-scores
df4 <- df3 %>%
  mutate(z_trans = 2*(sqrt(numerator) - sqrt(denominator))) # Method from A.1 Speigelhalter 2012
# compare to other method for z-score 
df5 <- df4 %>%
  mutate(trans_SR = sqrt(numerator/denominator),
         trans_sd = 1/(2*sqrt(denominator))) %>%
  mutate(z_trans1 = (trans_SR - 1)/trans_sd)
# Both methods should create the same z-score
all.equal(df5$z_trans, df5$z_trans1)
# Winsorize 
df6 <- df5 %>%
  mutate(z_adj = Winsorize(df5$z_trans, probs = c(0.10, 0.90))) %>%
  select(-z_trans1)
# Double check to see if winsorized
summary(df6$z_adj)
summary(df6$z_trans)
# Creating an overdispersion ratio 
# From both Mainey's phd pg 194 & Spiegelhalter 2012, page 9 eq 2
phi_z <- (sum(df6$z_adj^2))/nrow(df6)
phi_z
# Test for overdispersion
# Method 1
(nrow(df6)-1)/nrow(df6)
phi_z > (nrow(df6-1))/nrow(df6)
#> True
# Method 2 page 169 DOI 10.1007/s10729-013-9264-9
1 + 2*(sqrt(2/nrow(df6)))
phi_z > 1 + 2*(sqrt(2/nrow(df6)))
#> Also true
glimpse(df6)
df6 <- df6 %>%
  mutate(ll99_MAM = target - cl99*s_i*sqrt(phi),
         up99_MAM = target + cl99*s_i*sqrt(phi),
         ll95_MAM = target - cl95*s_i*sqrt(phi),
         up95_MAM = target + cl95*s_i*sqrt(phi))
funplot_MAM1 <- ggplot(df6, aes(x=denominator, y=SR_i,
                                label = hospital_ID)) +
  geom_point() +
  geom_line(aes(y=target)) +
  geom_line(aes(y=ll99_MAM)) +
  geom_line(aes(y=up99_MAM)) +
  geom_line(aes(y=ll95_MAM)) +
  geom_line(aes(y=up95_MAM)) +
  labs(title = MAM)
funplot_MAM1
# Test for outliers
# Find outliers
outliers <- df6 %>%
  filter(SR_i >= up99_MAM |
           SR_i <= 0 | 
           SR_i <= ll99_MAM)

count(outliers)
# 8
# Worth noting that for outliers, all hospital_ID > 40. 
###############################################
# Spiegelhalter's AREM
# Clean up dataset prior to modelling
df8 <- df6 %>%
  select(-z_trans, -trans_SR, -trans_sd,
      -ll99_MAM, -ll95_MAM, -up99_MAM, -up95_MAM)
glimpse(df8)
# Estimate additional between hospital variance
df9 <- df8 %>%
  mutate(s_squared_i = numerator/(denominator^2)) %>% # Equation 3.13 in Dissertation
  mutate(w = 1/s_squared_i) %>% # Equation 3.14
  mutate(tau_numerator = phi_z*nrow(df6) - (nrow(df6) - 1),
         tau_demoniator = sum(w) - (sum(w^2))/sum(w)) %>%
  mutate(tau_squared = tau_numerator/tau_demoniator) %>% # Eq 3.15 
  mutate(se = 1/denominator)
glimpse(df9)
# Construct control limits
df10 <- df9 %>%
  mutate(ll99_AREM = target - cl99*sqrt(se + tau_squared),
         up99_AREM = target + cl99*sqrt(se + tau_squared),
         ll95_AREM = target - cl95*sqrt(se + tau_squared),
         up95_AREM = target + cl95*sqrt(se + tau_squared)) # Equation 3.16
glimpse(df10)
# Plot control Limits
funplot_AREM1 <- ggplot(df10, aes(x=denominator, 
                                  y=SR_i, label = hospital_ID)) +
  geom_point() +
  geom_line(aes(y=target)) +
  geom_line(aes(y=ll99_AREM)) +
  geom_line(aes(y=up99_AREM)) +
  geom_line(aes(y=ll95_AREM)) +
  geom_line(aes(y=up95_AREM))

funplot_AREM1  
# Find outliers
outliers_AREM <- df10 %>%
  filter(SR_i > up99_AREM |
           SR_i == 0 | 
           SR_i < ll99_AREM)
outliers_AREM           
count(outliers_AREM)
#> 11 outliers
#> 3 more outliers than MAM adjustment
# Compare with Mainney Package
FunnelPlotR::funnel_plot(numerator = df10$numerator, 
                         denominator = df10$denominator, 
                         group = df10$hospital_ID,
                         draw_unadjusted = FALSE,
                         draw_adjusted = TRUE, 
                         sr_method = "CQC",
                         limit = 99)











