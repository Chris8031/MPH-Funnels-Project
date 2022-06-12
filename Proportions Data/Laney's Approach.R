# Laneys model
# Preamble
library(tidyverse)
library(DescTools)
# Generate Data with strong outliers/likely to have high overdispersion ratio
set.seed(2)
denominators <- sample(1:100, size = 100, replace = T)
denominators
# for numerators, make it a 30/40/30 split to ensure overdispersion
numerators <- c(rbinom(n=30, size = denominators[1:30], p = 0.2),
                rbinom(n=40, size = denominators[31:70], p = 0.5),
                rbinom(n=30, size = denominators[71:100], p = 0.8))
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
# Create funnel plot using standard approach
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


# Start modelling laney's approach
# Compute the standard deviation for each 
  # Firstly create p_line: which is the overall proportion
agg_data1 <- agg_data %>%
  mutate(p_line = sum(numerators)/sum(denominators),
         p_i = numerators/denominators)

#i @Andrew
# Is the numbering changes in dataset preferred? or better just
# to continue transforming original dataset 
agg_data2 <- agg_data1 %>%
 mutate(sigma_pi = sqrt((p_line*(1-p_line))/denominators))
agg_data2
# Create standardised z-scores
agg_data3 <- agg_data2 %>%
  mutate(z_i = (p_i-p_line)/sigma_pi)
view(agg_data3)
# Compute the standard deviation of the z_i values
agg_data4 <- agg_data3 %>%
  mutate(sigma_z = sqrt(sum(z_i)/((nrow(agg_data3))-1)))
view(agg_data4)
# Create sigma_piz
agg_data5 <- agg_data4 %>%
  mutate(sigma_piz = sigma_pi*sigma_z)
view(agg_data5)
# Todo: Plot control limits














