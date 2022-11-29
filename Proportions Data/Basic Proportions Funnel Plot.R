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
# Method 1 Pure unadjusted 
df2 <- df1 %>%
  mutate(y_i = (numerators/denominators),
         p = sum(numerators)/sum(denominators),
         s_i = 1/(2 * sqrt(denominators)))
glimpse(df2)         
# Add control limits
df3_a <- df2 %>%
  mutate(ll99 = p - 3*sqrt(s_i),
         up99 = p + 3*sqrt(s_i),
         ll95 = p - 3*sqrt(s_i),
         up95 = p + 3*sqrt(s_i)
         )
# plot fp
funplot1 <- ggplot(df3_a,
                   aes(x=denominators,
                       y=numerators/denominators,
                       label = hospital)) +
  geom_point() +
  geom_line(aes(y=p)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=up99)) +
  geom_line(aes(y=ll95)) +
  geom_line(aes(y=up95))

funplot1
# Okay I have no clue

# Method 2: transform variables 
plot_data2 <- df3_a %>%
  mutate(trans_y_i = asin(sqrt(numerators/denominators)),
         trans_p = asin(sqrt(p)),
         se = 1/ (2 * sqrt(denominators)))
View(plot_data2)

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
  theme_bw()
funplot




