#########################
# Preamble 
library(tidyr)
library(tidyverse)
library(DescTools)
# set critical values for control limits
cl99 <- qnorm(.999)
cl95 <- qnorm(0.975)
#  Generate fake dataset for standardised ratios
set.seed(2)
# 80 as this is roughly the number of hospitals
# under WA Health
denominator <- sample(1:100, size = 80, replace = TRUE)
denominator
quantiled <- Quantile(denominator)
quantiled[3]
# For numerators, using 25% and 75% quantiled to try and 
# ensure overdispersion for standardised ratios
numerator <-  c(rpois(40, lambda = quantiled[2]),
                (rpois(40, lambda = quantiled[4])))
numerator
df1 <- data.frame(numerator, denominator)
df1
df2 <- data.frame(numerator, denominator) %>%
  mutate(hospital_ID = row_number())
df2$hospital_ID
glimpse(df2)
df2
df3 <- df2 %>%
  mutate(SR_i = numerator/denominator,
         target = 1,
         s_i = sqrt(1/denominator),
         s_i_squared = 1/denominator) # from Appendix A.1 Spiegelhalter 2012
glimpse(df3)
##################################
# Unadjusted version
  # Add control limits
df4 <- df3 %>%
  mutate(ll99 = target - cl99*s_i,
         up99 = target + cl99*s_i,
         ll95 = target - cl95*s_i,
         up95 = target + cl95*s_i
         )
glimpse(df4)
# Plot funnel plot 
funplot_unadjusted <- ggplot(df4, aes(x=denominator, 
                                      y=SR_i,
                                      label = hospital_ID)) +
  geom_point() +
  geom_line(aes(y=target)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=up99)) +
  geom_line(aes(y=ll95)) +
  geom_line(aes(y=up95)) +
  theme_bw()
funplot_unadjusted
# Find outliers
outliers <- df4 %>%
  filter(SR_i > up99 |
          SR_i == 0 | 
           SR_i < ll99)
outliers           
count(outliers)
#> 18 outliers
# Compare with Mainney Package
FunnelPlotR::funnel_plot(numerator = df4$numerator, 
                         denominator = df4$denominator, 
                         group = df4$hospital_ID,
                         draw_unadjusted = TRUE,
                         draw_adjusted = FALSE, 
                         limit = 99)

