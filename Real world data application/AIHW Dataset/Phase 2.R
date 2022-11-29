#################
# Preable
# Load Packages
library(FunnelPlotR)
library(tidyverse)
library(DescTools)
# Import data
Import_data.fn <- function(datalocation){
library(readxl)
data <- read_excel(datalocation)
return(data)}
df1 <- Import_data.fn("Real world data application/AIHW Dataset/AIHW_GroupD1.xlsx")
# set critical values for control limits
cl99 <- qnorm(.999)
cl95 <- qnorm(0.975)
######################################
# Unadjusted
df2 <- df1 %>%
  mutate(HMSR = Observed/Expected,
         theta = 1,
         s_i = sqrt(1/Expected))
df3 <- df2 %>%
  mutate(ll99 = theta - cl99*s_i,
         ul99 = theta + cl99*s_i)
#############################################################
# Spiegelhalter's MAM
df4 <- df3 %>%
  mutate(HMSR_transformed = sqrt(Observed/Expected),
         s_i_transformed = 1/(2*sqrt(Expected)),
          z_transformed = 2*(sqrt(Observed) - sqrt(Expected)))
df5 <- df4 %>%
  mutate(z_adj = Winsorize(df4$z_transformed, probs = c(0.10, 0.90))) %>%
  dplyr::select(-z_transformed)
# Overdispersion test
phi <- (sum(df5$z_adj^2))/nrow(df5)
phi > (nrow(df5)-1)/nrow(df5) # Brackets matter here
df6 <- df5 %>%
  mutate(ll99_MAM = theta - cl99*s_i*sqrt(phi),
         ul99_MAM = theta + cl99*s_i*sqrt(phi),) # Use not transformed first 
############################################################
# Spiegelhalter AREM
df7 <- df6 %>%
  mutate(s_squared_i = Observed/(Expected^2),
         w = 1/s_squared_i)
# Remove first row due to odd value
df8 <- df7[-1,]
# Estimate additional between-hospital variance
df9 <- df8 %>%
  mutate(tau_numerator = phi*nrow(df8)- (nrow(df8) - 1),
         tau_denominator = sum(w) - ((sum(w^2))/sum(w)),
         tau_squared = tau_numerator/tau_denominator,
         se = 1/Expected)
# Construct control limits
df10 <- df9 %>%
  mutate(ll99_AREM = theta - cl99*sqrt(se + tau_squared),
         ul99_AREM = theta + cl99*sqrt(se + tau_squared))
##############################################################
# Construct control limits from Laney's model
df11 <- df10 %>% 
  mutate(d = sum(Observed)/sum(Expected),
    laney_sd_sr = sqrt(1/ Expected),
         laney_z_i = (HMSR - d) / laney_sd_sr,
    z_diff = laney_z_i - mean(laney_z_i),
    N = nrow(df10), 
    sum_z_diff_squared = sum(z_diff^2),
    sigma_z_squared = sum_z_diff_squared/ (N - 1),
    sigma_z = sqrt(abs(sigma_z_squared)),
    sigma_piz = laney_sd_sr*sigma_z,
        laney_sdz_sr = sd(DescTools::Winsorize(laney_sd_sr, probs = c(0.1, 0.9))),
        laney_ul_1 = theta + cl99 * laney_sd_sr * laney_sd_sr,
        laney_ll_1 = theta - cl99 * laney_sd_sr * laney_sd_sr,
    laney_ul_2 = theta + cl99 * sigma_piz,
    laney_ll_2 = theta - cl99 * sigma_piz)
        



df11 %>%
  mutate(
    HMSR = HMSR*100,
    theta = theta*100,
    ll99 = ll99*100,
    ul99 = ul99*100,
    ll99_MAM = ll99_MAM*100,
    ul99_MAM = ul99_MAM*100,
    ul99_AREM = ul99_AREM*100,
    ll99_AREM = ll99_AREM*100,
    laney_ll_2 = laney_ll_2*100,
    laney_ul_2 = laney_ul_2*100
  ) %>%
  ggplot(aes(
    x=Expected, 
    y=HMSR,
    label = ID)) +
  geom_point() +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=ll99, color = "Unadjusted")) +
  geom_line(aes(y=ul99, color = "Unadjusted")) +
  geom_line(aes(y=ll99_MAM, color = "MAM")) +
  geom_line(aes(y=ul99_MAM, color = "MAM")) +
  geom_line(aes(y=ll99_AREM, color = "AREM")) +
  geom_line(aes(y=ul99_AREM, color = "AREM")) +
  geom_line(aes(y = laney_ul_2, color = "Laneys")) +
  geom_line(aes(y = laney_ll_2, color = "Laneys")) +
  theme_bw() +
  labs(
    x = "Expected Number of Deaths",
    y = "HSMR", 
    color = "99.7 Control Limits")



  

  
  






