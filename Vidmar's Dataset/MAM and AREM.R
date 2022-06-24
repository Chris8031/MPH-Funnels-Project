#########################################################
#Preamble
library(tidyverse)
library(DescTools)
library(readxl)
Dataset <- read_excel("Vidmar's Dataset/Dataset.xlsx")
df1 <- Dataset %>%
  mutate(hospital_ID = row_number()) %>%
  rename(numerators = Numerator,
         denominators = Denominator)
# Rename variables to correspond with Vidmar's work
df2 <- df1 %>% # Equation 3.1 in dissertation
  mutate(pi = (numerators/denominators),
         pm = sum(numerators)/sum(denominators)) %>%
  rename(xi = numerators,
         ni = denominators) %>%
  mutate(spi = sqrt((pm*(1-pm))/ni),
         z_i = (pi-pm)/spi) %>%
  select(-hospital_ID)
glimpse(df2)
# Spiegelhalter's MAM
# apply variance stabilising/normal approximation transfromation to 
# target value, proportion and associated within-institution standard error
# Note: Vidmar didn't apply transformations to his z_score
df3 <- df2 %>%
  mutate(trans_pi = asin(sqrt(pi)), # Equation 3.4
         trans_pm = asin(sqrt(pm)), # Equation 3.5
         trans_spi = 1/(2*sqrt(ni))) # Equation 3.6
glimpse(df3)
# transformed z-score calculation and winsorise
df4 <- df3 %>%
  mutate(trans_zi = (trans_pi-trans_pm)/trans_spi,
         z_adj = Winsorize(df4$trans_zi, probs = c(0.10, 0.90)))
glimpse(df4)
# Inspect for winsorisation
winsorized <- c(summary(df4$z_i),
              summary(df4$z_adj),
              summary(df4$trans_zi))
winsorized
# Create overdisperion ratio
df5 <- df4 %>%
  mutate(phi = sum(z_adj^2/nrow(df4)))
glimpse(df5)
df5$phi > (nrow(df5)-1)/nrow(df5)
# Obviously true, as this dataset is insanely overdispersed
# Now to construct control limits 
a99 <-  qnorm(.999)  
df6 <- df5 %>%
  mutate(ll99 = pm - a99*spi*sqrt(phi),
         ul99 = pm + a99*spi*sqrt(phi))
funplot_MAM_Vidmar <- ggplot(df6, 
                             aes(x = ni,
                                 y = pi)) +
  geom_point() +
  geom_line(aes(y = pm)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=ul99))
funplot_MAM_Vidmar
# test for outliers
outliers <- df6 %>%
  filter(pi >= ul99 |
           pi <= 0 |
           pi <= ll99)
view(outliers)
#> 3 outliers
#> 2 above cl
#> 1 slightly below cl
# Method 2: using untransformed z_i
# This should replicate Vidmar's control limits exactly
df7 <- df6 %>%
  mutate(z_adj = Winsorize(df6$z_i, probs = c(0.10, 0.90)),
         phi = sum(z_adj^2/nrow(df4)),
         ll99 = pm - a99*spi*sqrt(phi),
         ul99 = pm + a99*spi*sqrt(phi)) 
glimpse(df7)
funplot_MAM_Vidmar2 <- ggplot(df7, 
                             aes(x = ni,
                                 y = pi)) +
  geom_point() +
  geom_line(aes(y = pm)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=ul99))
funplot_MAM_Vidmar2
outliers2 <- df7 %>%
  filter(pi >= ul99 |
           pi <= 0 |
           pi <= ll99)
view(outliers2)
# Same same really
