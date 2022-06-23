#########################################################
#Preamble
library(tidyverse)
library(DescTools)
# set critical values for control limits
a99 <-  qnorm(.999)  
a95 <- qnorm(.975)
# Simulate dataset for random proportion indicator
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
df2 <- df1 %>%
  mutate(y_i = (numerators/denominators),
         p = sum(numerators)/sum(denominators))
#######################################################
# Laney's approach based on Original paper
# Compute the standard deviation for each proportion
df3 <- df2 %>%
  mutate(sigma_pi = sqrt((p*(1-p))/denominators))
df3
# Create standardised z-scores
df4 <- df3 %>%
  mutate(z_i = (y_i-p)/sigma_pi)
glimpse(df4)
# Compute the standard deviation of the z_i values
df5 <- df4 %>%
  mutate(z_bar = mean(z_i)) %>%
  mutate(z_diff = z_i - z_bar) %>%
  mutate(sum_z_diff_squared = abs(sum(z_diff^2)),
         N = sum(denominators)) %>%
  mutate(sigma_z_squared = sum_z_diff_squared/N-1) %>%
  mutate(sigma_z = sqrt(abs(sigma_z_squared))) %>%
  mutate(sz = sd(z_i))
glimpse(df5)
  
# Create sigma_piz
df6 <- df5 %>%
  mutate(sigma_piz = sigma_pi*sz)
glimpse(df6)
# create control limits
df7 <- df6 %>%
  mutate(ul99 = p + a99*sigma_piz,
         ll99 = p - a99*sigma_piz,
         ul95 = p + a95*sigma_piz,
         ll95 = p - a95*sigma_piz)
# plot control limits
funplot_LCM <- ggplot(df7, aes(x=denominators, y=y_i,
                               label = hospital)) +
  geom_point() +
  geom_line(aes(y=p)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=ul99), color = "blue") +
  geom_line(aes(y=ll95)) +
  geom_line(aes(y=ul95)) +
  theme_dark()
funplot_LCM













