#################################
# Preamble
library(tidyr)
library(tidyverse)
library(DescTools)
#  Generate fake dataset for standardised ratios
set.seed(80)
denominator <- sample(1:20, size = 100, replace = TRUE)
numerator <-  rpois(100, lambda = denominators)
hospital.fn <- function(length.out) {
  a <- rep(letters, length.out = length.out)
}
df1 <- data.frame(numerator, denominator, hospital = hospital.fn(nrow(raw_data)))
df1 <- df1 %>%
  mutate(SR = sqrt(numerator/denominator),
         target = 1 ,
         se = 1/ (2 * sqrt(denominator))) %>%
        rename(Transformed_SR = SR)
df2 <- df1 %>%
  mutate(z_score = (Transformed_SR - target) / se)
View(df2)

# Scatter plot in ggplot
a<-ggplot(df1, aes(x=denominator, y= Transformed_SR))+
  geom_point()

a
# Now add a central line, in a ration like this, 1 is the average/expected value.
a<- a+geom_hline(aes(yintercept=1))
a

# Add a 95% Poisson limit, by using the density function to get the quantile value for each 'expected'.
lkup<-data.frame(id=seq(1, max(df1$denominator), 1))
lkup$Upper95<-(qpois(0.975,lambda = lkup$id) - 0.025) / lkup$id
lkup$Lower95<-(qpois(0.025,lambda = lkup$id) - 0.975) / lkup$id
# Add 99.8% Poisson Limits
lkup$Upper99 <- (qpois(0.998, lambda = lkup$id - 0.002)) / lkup$id
lkup$Lower99 <- (qpois(0.002, lambda = lkup$id - 0.998)) / lkup$id
lkup1<-gather(lkup, key, value,-id)
lkup1

Poisson_FP <- a+ geom_line(aes(x=id, y=value, col=key), data=lkup1)
Poisson_FP

