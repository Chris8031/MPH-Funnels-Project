###################################
# Preamble
library(tidyverse)
library(DescTools)
# set critical values for control limits
a99 <-  qnorm(.999)  
a95 <- qnorm(.975)
# Generate Data with strong outliers/likely to have high overdispersion ratio
set.seed(2)
denominators <- sample(1:100, size = 100, replace = T)
denominators
# for numerators, make it a 30/40/30 split to ensure overdispersion
numerators <- c(rbinom(n=30, size = denominators[1:30], p = 0.25),
               rbinom(n=40, size = denominators[31:70], p = 0.5),
               rbinom(n=30, size = denominators[71:100], p = 0.75))
numerators               
hospital.fn <- function(length.out) {
  a <- rep(letters, length.out = length.out)
}
raw_data <- data.frame(numerators, denominators, hospital = hospital.fn(nrow(raw_data)))
head(raw_data)
# Convert to agg data
agg_data = raw_data %>%
  group_by(hospital) %>%
  summarise(across(c(numerators, denominators), sum)) %>%
  ungroup()
head(agg_data)
# In this mutation we just take target value p to be the 
# sum of numerators and denominators
plot_data2 <- agg_data %>%    # Equation 3.1 in dissertation
  mutate( y = numerators/denominators,
          target_p = sum(numerators)/sum(denominators),
          se = sqrt((target_p*(1-target_p))/denominators))
head(plot_data2)
# Spiegelhalter's MAM
# apply variance stabilising/normal approximation transfromation to 
# target value, proportion and associated within-institution standard error
plot_data2a <- plot_data2 %>%
  mutate(trans_y = asin(sqrt(y)), # Equation 3.4, 
        trans_target_p = asin(sqrt(target_p)), # Equation 3.5
        trans_se = 1/(2*sqrt(denominators)) # Equation 3.6
        )  
# transformed z-score calculation
plot_data3 <- plot_data2a %>%
  mutate(z_i = (trans_y-trans_target_p)/trans_se)
glimpse(plot_data3)
# adjust z scores via winsorisation
plot_data_winsor <- plot_data3 %>%
  mutate(z_adj = Winsorize(plot_data3$z_i, probs=c(0.10, 0.90)))
glimpse(plot_data_winsor)
# Double check to see if winsorized
view(plot_data_winsor %>%
       select(z_i, z_adj))
summary(plot_data_winsor$z_i)
summary(plot_data_winsor$z_adj)
# From output
#> 90th percentile is now z-score of 2.23
#> 10th percentile is now z-score of -2.6 
# Create an overdispersion ratio
phi <- ((sum(plot_data_winsor$z_adj^2)))/nrow(plot_data_winsor)
phi
# Test for overdispersion
phi > (nrow(plot_data_winsor)-1)/nrow(plot_data_winsor)
# If true, construct control limits
  # Method 1, used undertransformed se
plot_data4 <- plot_data3 %>%
  mutate(ll99_trans = target_p - a99*se*sqrt(phi),
         ul99_trans = target_p + a99*se*sqrt(phi),
         ll95_trans = target_p - a95*se*sqrt(phi),
         ul95_trans = target_p + a95*se*sqrt(phi))
head(plot_data4)
funplot_MAM_transformed <- ggplot(plot_data4, aes(x=denominators, 
                                      y = y,
                                      label = hospital)) +
  geom_point() +
  geom_line(aes(y=target_p)) +
  geom_line(aes(y=ll99_trans)) +
  geom_line(aes(y=ul99_trans)) +
  geom_line(aes(y=ll95_trans)) +
  geom_line(aes(y=ul95_trans))
funplot_MAM_transformed
  # Method 2: use transformed se and transformed target_p and bacn transform
glimpse(plot_data4)
plot_data4a <- plot_data3 %>%
  mutate(ll99_trans = trans_target_p - a99*trans_se*sqrt(phi),
         ul99_trans = trans_target_p + a99*trans_se*sqrt(phi),
         ll95_trans = trans_target_p - a95*trans_se*sqrt(phi),
         ul95_trans = trans_target_p + a95*trans_se*sqrt(phi))
head(plot_data4a)
  # Back transform limits to proportion scale
  plot_data5 <- plot_data4a %>%
    mutate(ll99 = sin(ll99_trans)^2,
           ul99 = sin(ul99_trans)^2,
           ll95 = sin(ll95_trans)^2,
           ul95 = sin(ul95_trans)^2)
  funplot_MAM1 <- ggplot(plot_data5, aes(x=denominators, y=y,
                                        label=hospital)) +
    geom_point() +
    geom_line(aes(y=target_p)) +
    geom_line(aes(y=ll99)) +
    geom_line(aes(y=ul99)) +
    geom_line(aes(y=ll95)) +
    geom_line(aes(y=ul95)) 
funplot_MAM1
  
  








