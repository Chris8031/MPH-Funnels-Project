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
glimpse(df1)
# Rename variables to correspond with Vidmar's work
df2 <- df1 %>%
  mutate(pi = (numerators/denominators),
         pm = sum(numerators)/sum(denominators)) %>%
  rename(xi = numerators,
         ni = denominators) %>%
  select(-hospital_ID)
#######################################################
# Laney's approach based on Original paper
# Compute the standard deviation for each proportion
df3 <- df2 %>%
  mutate(spi = sqrt((pm*(1-pm))/ni))
# Rearrange columns to align with excel sheet
df3 <- df3[,c(3, 1, 2, 5, 4)]
head(df3)
# Create standardised z-scores
df4 <- df3 %>%
  mutate(z_i = (pi-pm)/spi)
head(df4)
# Compute the standard deviation of the z_i values
df5 <- df4 %>%
  mutate(z_bar = mean(z_i)) %>%
  mutate(z_diff = z_i - z_bar) %>%
  mutate(sum_z_diff_squared = sum(z_diff^2),
         N = nrow(df4)) %>%
  mutate(sigma_z_squared = sum_z_diff_squared/N-1) %>%
  mutate(sigma_z = sqrt(abs(sigma_z_squared))) %>%
  mutate(sz = sd(z_i))
glimpse(df5)
# not sure why sz = sigma_z
# Create sigma_piz
df6 <- df5 %>%
  mutate(sigma_piz = spi*sigma_z)
glimpse(df6)
# create control limits (= 3 used for now).
# However this value can be obtained from Spiegelhalter's MAM model as
# shown in Vidmar's excel sheet
df7 <- df6 %>%
  mutate(ul99 = pm + 3*sigma_piz,
         ll99 = pm - 3*sigma_piz)
glimpse(df7)
# plot control limits
funplot_vidmar <- ggplot(df7, aes(x=ni, y=pi,
                               )) +
  geom_point() +
  geom_line(aes(y=pm)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=ul99)) 
funplot_vidmar
# test for outliers
outliers <- df7 %>%
  filter(pi >= ul99 |
           pi <= 0 |
           pi <= ll99)
view(outliers)
# Matches with row 146 and 172 (outliers) in Vidmar's xcel sheet
