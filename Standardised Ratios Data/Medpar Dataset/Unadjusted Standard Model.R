#################
# Preable
# Load Packages
library(FunnelPlotR)
library(COUNT)
library(tidyverse)
library(DescTools)
# set critical values for control limits
cl99 <- qnorm(.999)
cl95 <- qnorm(0.975)
# Create denominators from medpar dataset using poisson GLM to predict
# All from Funnel_Plot Vignette https://cran.rstudio.com/web/packages/FunnelPlotR/vignettes/funnel_plots.html
data(medpar)
medpar$provnum<-factor(medpar$provnum)
medpar$los<-as.numeric(medpar$los)
mod<- glm(los ~ hmo + died + age80 + factor(type), family="poisson", data=medpar)
summary(mod)
medpar$predict <- predict(mod, type = "response")
head(medpar)
# Cut predictor variables
df1 <- medpar 
############################################
# Basic unadjusted method
# Aggregate by provnum
df2 <- df1 %>%
  group_by(provnum) %>%
  summarise(across(c(los, predict), sum)) %>%
  ungroup()
df2 <- df2 %>%
  mutate(SR = los/predict,
         target = 1,
         cl = sqrt(1/predict)) 
df2
# Add control limits
df3 <- df2 %>%
  mutate(target = 1,
         s =sqrt(1/predict)) %>% # from Appendix A.1 Spiegelhalter 2012
  mutate(s_squared = 1/predict) %>%
  mutate(ll99 = target - cl99*s,
         up99 = target + cl99*s,
         ll95 = target - cl95*s,
         up95 = target + cl95*s)
# Construct unadjusted funnel plot 
funplot_unadjusted <- ggplot(df3, aes(x=predict, y=SR,
                                  label = provnum)) +
  geom_point() +
  geom_line(aes(y=target)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=up99)) +
  geom_line(aes(y=ll95)) +
  geom_line(aes(y=up95)) +
  theme_dark() +
  labs(title = "Unadjusted Funnel Plot",
       x = "Expected Number of Deaths",
       y = "HSMR")

funplot_unadjusted

#################################
# Multiplicative Adjustment Model
# Constructing z-scores
df4 <- df3 %>%
  mutate(z_trans = 2*(sqrt(los) - sqrt(predict))) # Method from A.1 Speigelhalter 2012
df4
# compare to other method for z-score 
df5 <- df4 %>%
  mutate(trans_SR = sqrt(los/predict),
         trans_sd = 1/(2*sqrt(predict))) %>%
  mutate(z_trans1 = (trans_SR - 1)/trans_sd)
# Both methods should create the same z-score
all.equal(df5$z_trans, df5$z_trans1)
df5
# Winsorize 
df6 <- df5 %>%
  mutate(z_adj = Winsorize(df5$z_trans, probs = c(0.10, 0.90)))
df6
summary(df6$z_adj)
# From output
  # z-score  10th percentile ~ -5.02
  # z-score  90th percentile ~ 3.64
# Creating an overdispersion ratio 
  # Method 1 (using poisson GLM from preamble)
  # From Mainey's Vignette https://cran.rstudio.com/web/packages/FunnelPlotR/vignettes/funnel_plots.html
phi <- sum(mod$weights * mod$residuals^2)/mod$df.residual
phi
#> 6.24
  # Method 2
  # From both Mainey's phd pg 194 & Spiegelhalter 2012, page 9 eq 2
phi_z <- (sum(df6$z_adj^2))/nrow(df6)
phi_z
#> 9.302
# No need to test for overdispersion as this is obvious
# Now to use phi & phi_z to widen control limits
# Method 1: phi
glimpse(df6)
df6 <- df6 %>%
mutate(ll99_MAM = target - cl99*trans_sd*sqrt(phi),
       up99_MAM = target + cl99*trans_sd*sqrt(phi),
       ll95_MAM = target - cl95*trans_sd*sqrt(phi),
       up95_MAM = target + cl95*trans_sd*sqrt(phi))
funplot_MAM1 <- ggplot(df6, aes(x=predict, y=SR,
                           label = provnum)) +
  geom_point() +
  geom_line(aes(y=target)) +
  geom_line(aes(y=ll99_MAM)) +
  geom_line(aes(y=up99_MAM)) +
  geom_line(aes(y=ll95_MAM)) +
  geom_line(aes(y=up95_MAM)) +
  theme_dark() +
  labs(title = "MAM with Phi")
funplot_MAM1
  #> 14 outliers - 6 above 99cl and 8 below 99cl
# Method 2: phi_z
df7 <- df6 %>%
  mutate(ll99_MAM = target - cl99*trans_sd*sqrt(phi_z),
         up99_MAM = target + cl99*trans_sd*sqrt(phi_z),
         ll95_MAM = target - cl95*trans_sd*sqrt(phi_z),
         up95_MAM = target + cl95*trans_sd*sqrt(phi_z))
funplot_MAM2 <- ggplot(df7, aes(x=predict, y=SR,
                           label = provnum)) +
  geom_point() +
  geom_line(aes(y=target)) +
  geom_line(aes(y=ll99_MAM)) +
  geom_line(aes(y=up99_MAM)) +
  geom_line(aes(y=ll95_MAM)) +
  geom_line(aes(y=up95_MAM)) +
  theme_dark() +
  labs(title = "MAM with Phi_z")
funplot_MAM2
  #> 1 outlier above 99
# To clarify, page 11 fig 3 SPiegelhalter 2012
# Says plot SR on original scale and that square-root transformation is 
# used only to calculate overdispersion.
# I was also wondering how to calculate percentage overdispersion. In the 2012
# paper page 11, percetange overdispersion is defined as
sqrt(phi_z) - 1
#> 2.05. Does this mean it is overdispersed by 205%?



#########################################################
# Spiegelhalter's AREM
  # Clean up dataset prior to modelling
df8 <- df7 %>%
  dplyr::select(-ll99, -up99, -ll95, -up95, -z_trans, -trans_SR, -trans_sd,
         -ll99_MAM, -ll95_MAM, -up99_MAM, -up95_MAM)
  # Estimate additional between hospital variance
  # Method 1: phi
glimpse(df8)
df9 <- df8 %>%
  mutate(s_squared_i = los/(predict^2)) %>%
  mutate(w = 1/s_squared_i) %>%
  mutate(tau_numerator = phi_z*nrow(df7) - (nrow(df7) - 1),
         tau_demoniator = sum(w) - (sum(w^2))/sum(w)) %>%
  mutate(tau_squared = tau_numerator/tau_demoniator)
glimpse(df9)
  # Construct control limits
df10 <- df9 %>%
mutate(ll99_AREM = target - cl99*sqrt(s + tau_squared),
       up99_AREM = target + cl99*sqrt(s + tau_squared),
       ll95_AREM = target - cl95*sqrt(s + tau_squared),
       up95_AREM = target + cl95*sqrt(s + tau_squared))
glimpse(df10)
# Plot control Limits
funplot_AREM1 <- ggplot(df10, aes(x=predict, y=SR, label = provnum)) +
  geom_point() +
  geom_line(aes(y=target)) +
  geom_line(aes(y=ll99_AREM)) +
  geom_line(aes(y=up99_AREM)) +
  geom_line(aes(y=ll95_AREM)) +
  geom_line(aes(y=up95_AREM)) 
funplot_AREM1  
