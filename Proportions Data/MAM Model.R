# Preamble
library(tidyverse)
library(DescTools)
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
View(raw_data)
# Convert to agg data
agg_data = raw_data %>%
  group_by(hospital) %>%
  summarise(across(c(numerators, denominators), sum)) %>%
  ungroup()
view(agg_data)
plot_data2 <- agg_data %>%
  mutate( y = asin(sqrt(numerators/denominators)),
          p = sum(numerators)/sum(denominators),
          target = asin(sqrt(p)),
          se = 1/ (2 * sqrt(denominators)))
View(plot_data2)
# set critical values for control limits
a99 <-  qnorm(.999)  
a95 <- qnorm(.975)
# add control limits
plot_data2 <- plot_data2 %>%
  mutate(ll99_trans = target - a99*se,
         ul99_trans = target + a99 * se,
         ll95_trans = target - a95 * se,
         ul95_trans = target + a95 * se)
head(plot_data2)
# Back-transform limits to proportion scale
plot_data2 <- plot_data2 %>%
  mutate(ll99 = sin(ll99_trans)^2,
         ul99 = sin(ul99_trans)^2,
         ll95 = sin(ll95_trans)^2,
         ul95 = sin(ul95_trans)^2)
# Construct funnel plot 
funplot <- ggplot(plot_data2, aes(x=denominators, y=numerators/denominators,
                                  label = hospital)) +
  geom_point() +
  geom_line(aes(y=p)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=ul99)) +
  geom_line(aes(y=ll95)) +
  geom_line(aes(y=ul95)) +
  theme_dark()
funplot
# Spiegelhalter's MAM
head(plot_data2)
# transformed z-score calculation
plot_data3 <- plot_data2 %>%
  mutate(z = {y-target}/se)
glimpse(plot_data3)
# adjust z scores via winsorisation
plot_data_winsor <- plot_data3 %>%
  mutate(z_adj = Winsorize(plot_data3$z, probs=c(0.10, 0.90)))
view(plot_data_winsor)
# Create an overdispersion ratio
phi <- ((sum(plot_data_winsor$z_adj^2)))/nrow(plot_data_winsor)
phi
# Test for overdispersion
phi > (nrow(plot_data_winsor)-1)/nrow(plot_data_winsor)
# If true, construct control limits
  # Method 1, insert overdispersio ratio prior to transformation
plot_data4 <- plot_data3 %>%
  mutate(ll99_trans = target - a99*phi*se,
         ul99_trans = target + a99*phi*se,
         ll95_trans = target - a95*phi*se,
         ul95_trans = target + a95*phi*se)
head(plot_data4)
  # Back transform limits to proportion scale
  plot_data5 <- plot_data4 %>%
    mutate(ll99 = sin(ll99_trans)^2,
           ul99 = sin(ul99_trans)^2,
           ll95 = sin(ll95_trans)^2,
           ul95 = sin(ul95_trans)^2)
  funplot_MAM <- ggplot(plot_data5, aes(x=denominators, y=numerators/denominators,
                                        label=hospital)) +
    geom_point() +
    geom_line(aes(y=p)) +
    geom_line(aes(y=ll99)) +
    geom_line(aes(y=ul99)) +
    geom_line(aes(y=ll95)) +
    geom_line(aes(y=ul95)) +
    theme_dark()
funplot_MAM








