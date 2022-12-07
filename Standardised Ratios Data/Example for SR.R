library(tidyverse)
set.seed(2020)

denominator <- sample(1:400, size = 100)

pr_incontrol <- 0.5

# Generate base normal variates for the probability of in-control and outlying observations
p_star_incontrol <- rnorm(90, mean = 0, sd = 1)
p_star_outlier <- rnorm(10, mean = 3.5, sd = 0.1)

# Apply the Normal CDF function with in-control distribution parameters
# to transform to Uniform(0, 1) scale
p_uniform_incontrol <- pnorm(p_star_incontrol, mean = 0, sd = 1)
p_uniform_outlier <- pnorm(p_star_outlier, mean = 0, sd = 1)


### Proportion Data - 5 negative & 5 positive outliers
pr_numerator_incontrol <- qbinom(p_uniform_incontrol, size = head(denominator, 90), prob = pr_incontrol)
denominator_outlier <- tail(denominator, 10)
pr_numerator_outlier <- c(
  qbinom(head(p_uniform_outlier, 5),
         size = head(denominator_outlier, 5),
         prob = pr_incontrol),
  qbinom(1-tail(p_uniform_outlier, 5),
         size = tail(denominator_outlier, 5),
         prob = pr_incontrol)
)

### Standardised-Ratio Data
sr_numerator_incontrol <- qpois(p_uniform_incontrol, lambda = head(denominator, 90))
sr_numerator_outlier <- c(
  qpois(head(p_uniform_outlier, 5),
        lambda = head(denominator_outlier, 5)),
  qpois(1-tail(p_uniform_outlier, 5),
        lambda = tail(denominator_outlier, 5))
)

data <- data.frame(
  d = denominator,
  c = c(sr_numerator_incontrol, sr_numerator_outlier),
  n = c(pr_numerator_incontrol, pr_numerator_outlier),
  outlier = factor(c(rep("In-control",90), rep("Outlier",10)))
)


unadjusted_limits <- function(data) {
  data %>%
    mutate(
      target_pr = sum(n) / sum(d),
      target_sr = 1,
      trans_target_pr = asin(sqrt(target_pr)),
      trans_target_sr = 1,
      y_pr = n / d,
      y_sr = c / d,
      trans_y_pr = asin(sqrt(y_pr)),
      trans_y_sr = sqrt(y_sr),
      y_se = 1 / (2*sqrt(d)),
      s_i_sr = 1/sqrt(d),
      unadj_ll_pr = sin(trans_target_pr + qnorm(0.001) * y_se)^2,
      unadj_ul_pr = sin(trans_target_pr + qnorm(0.999) * y_se)^2,
      unadj_ll_sr = (1 + qnorm(0.001) * s_i_sr),
      unadj_ul_sr = (1 + qnorm(0.999) * s_i_sr),
      unadj_95_ll = (1 + qnorm(0.05) * s_i_sr),
      unadj_95_ul = (1 + qnorm(0.95) * s_i_sr),
    )
}
data2 <- unadjusted_limits(data = data)

data3 <- data2 %>%
  mutate(pr = n/d, sr = c/d) %>%
  rename(Denominator = d) %>%
  pivot_longer(c(pr,sr), names_to = "Type", values_to = "Indicator Value") %>%
  mutate(Type = factor(Type, levels=c("pr", "sr"), labels = c("Proportion", "Standardised-Ratio")))



data3 %>%
  ggplot(aes(x=Denominator, y=`Indicator Value`, colour = outlier)) +
  geom_point() + 
  scale_color_manual(values=c("black","steelblue")) + 
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~Type, ncol=2, nrow=1, scales="free")

data3 %>%
  ggplot(aes(x = Denominator, y = `Indicator Value`)) +
  geom_point() +
  geom_line(aes(y = target_sr)) +
  geom_line(aes(y = unadj_ul_sr, color = "99.7 Control Limits")) +
  geom_line(aes(y = unadj_ll_sr, color = "99.7 Control Limits")) +
  geom_line(aes(y = unadj_95_ll, color = "95 Control Limits")) +
  geom_line(aes(y = unadj_95_ul, color = "95 Control Limits")) +
  labs(colour = "Control Limits", y = "Standardised Ratios") +
  theme_minimal()


