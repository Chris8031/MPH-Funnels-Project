#Preamble
library(tidyverse)
library(DescTools)
# Simulate dataset for random proportion indicator
set.seed(80)
denominators <- sample(1:100, size = 100, replace = T)
seq_probability <- seq(0, 1, 0.1)
rand <-  sample(seq_probability, 1)
numerators <-  rbinom(n=100, size = denominators, p = rand)
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
# Method 1 Pure unadjusted 



# Method 2: transform variables 
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
plot_data1 <- plot_data %>%
  mutate(ll99_trans = target - a99*se,
         ul99_trans = target + a99 * se,
         ll95_trans = target - a95 * se,
         ul95_trans = target + a95 * se)
head(plot_data)
# Back-transform limits to proportion scale
plot_data1 <- plot_data1 %>%
  mutate(ll99 = sin(ll99_trans)^2,
         ul99 = sin(ul99_trans)^2,
         ll95 = sin(ll95_trans)^2,
         ul95 = sin(ul95_trans)^2)
# Construct funnel plot 
funplot <- ggplot(plot_data1, aes(x=denominators, y=numerators/denominators,
                                 label = hospital)) +
  geom_point() +
  geom_line(aes(y=p)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=ul99)) +
  geom_line(aes(y=ll95)) +
  geom_line(aes(y=ul95)) +
  theme_dark()
funplot




