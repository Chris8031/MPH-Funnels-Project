#####################
# Generic SPC Chart #
#####################
# install.packages("qicharts2")
library(tidyverse)


# Adapted from: https://cran.r-project.org/web/packages/qicharts2/vignettes/qicharts2.html

library(qicharts2)
set.seed(2020)
run_chart <- qic(month, harms, days,
    data = gtt,
    multiply = 1000,
    title = "Run Chart of Patient Harm",
    ylab = "Adverse Events per 1000 Patient Days",
    xlab = "Month")

run_chart_data <- run_chart$data
run_chart_data2 <- run_chart_data %>%
  mutate(UCL = sd(y) + mean(y),
         LCL = mean(y) - sd(y),
         mean_y = mean(y))
run_chart_data2

# I'm not a fan of the default aesthetic from qicharts, so we can make our own
library(ggplot2)

run_chart_redux <- ggplot(run_chart_data2, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = y)) +
  geom_line(aes(y = mean_y)) +
  geom_line(aes(y = LCL)) +
  geom_line(aes(y = UCL)) +
  labs(title = "Run Chart of Patient Harm",
       y = "Adverse Events per 1000 Patient Days",
       x = "Month") +
  theme(plot.title = element_text(hjust = 0.5))
run_chart_redux

