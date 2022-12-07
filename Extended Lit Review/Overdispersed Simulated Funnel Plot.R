library(tidyverse)
library(readxl)
AIHW_GroupD1 <- read_excel("Real world data application/AIHW Dataset/AIHW_GroupD1.xlsx")
id <- ids::random_id(100, bytes = 2) # generate random IDs for hospitals
set.seed(3)
SR<- big_dataset <- rnorm(100, mean = 1, sd = 0.5)
expected <- sample(1:400, size = 100)
observed <- SR*expected

data <- data.frame(
  id = id,
  observed = observed,
  Expected = expected,
  SR = SR
)
# Generating critical values for control limits
a99 <- qnorm(0.998)
# Compute control limits - make sure to assign result to output
data %>%
  mutate(
    theta = 1,
    s_i = sqrt(1/expected),
    ll99 = theta - a99*s_i,
    ul99 = theta + a99*s_i,
    outlier = (SR < ll99) | (SR > ul99)
  ) %>%
  ggplot(aes(x=Expected, 
                  y=SR)) +
  geom_point() +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=ul99)) +
  theme_bw()

