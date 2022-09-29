#################
# Preable
# Load Packages
library(FunnelPlotR)
library(tidyverse)
library(DescTools)
library(MASS)
# Import data
library(readxl)
AIHW_GroupD1 <- read_excel("D:/Github/MPH-Funnels-Project/Standardised Ratios Data/AIHW Dataset/AIHW_GroupD1.xlsx")
df1 <- AIHW_GroupD1
# set critical values for control limits
cl99 <- qnorm(.999)
cl95 <- qnorm(0.975)
summary(df1) # Looking at descriptive stats for dataset
######################################
df2 <- df1 %>%
  mutate(HMSR = Observed/Expected,
         theta = 1,
         s_i = sqrt(1/Expected))
df3 <- df2 %>%
  mutate(ll99 = theta - cl99*s_i,
         ul99 = theta + cl99*s_i,
         ll95 = theta - cl95*s_i,
         ul95 = theta + cl95*s_i)
############################

sd(df2$HMSR)
mean(df2$HMSR)

#########################
df3
funplot_unadjusted <- ggplot(df3, aes(x=Expected, y=HMSR,
                                      label = ID)) +
  geom_point() +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=ll99)) +
  geom_line(aes(y=ul99)) +
  geom_line(aes(y=ll95)) +
  geom_line(aes(y=ul95)) +
  theme_dark() +
  labs(title = "Unadjusted Funnel Plot",
       x = "Expected Number of Deaths",
       y = "HSMR")

funplot_unadjusted

df3
funplot_unadjusted_outliers <- df3 %>%
  filter(HMSR >= ul99 |
           HMSR <= ll99)
funplot_unadjusted_outliers
funplot_unadjusted_outliers_count <- funplot_unadjusted_outliers %>%
  mutate(outlier = case_when(HMSR >= ul99 ~ 'Above Control Limits',
                                HMSR <= ll99 ~ 'Below Control Limits')) %>%
  count(outlier)
funplot_unadjusted_outliers_count
# From the output, 12 hospitals lie above the control limit and 1 lies below it. 
#############################################################
# Spiegelhalter's MAM
glimpse(df3)
df4 <- df3 %>%
  mutate(HMSR_transformed = sqrt(Observed/Expected),
         s_i_transformed = 1/(2*sqrt(Expected)),
          z_transformed = 2*(sqrt(Observed) - sqrt(Expected)))
df5 <- df4 %>%
  mutate(z_adj = Winsorize(df4$z_transformed, probs = c(0.10, 0.90))) %>%
  dplyr::select(-z_transformed)
glimpse(df5)
# Overdispersion test
phi <- (sum(df5$z_adj^2))/nrow(df5)
phi > (nrow(df5)-1)/nrow(df5) # Brackets matter here
glimpse(df5)
df6 <- df5 %>%
  mutate(ll99_MAM = theta - cl99*s_i*sqrt(phi),
         ul99_MAM = theta + cl99*s_i*sqrt(phi),) # Use not transformed first 

glimpse(df6)  
funplot_MAM <- ggplot(df6, aes(x=Expected, y=HMSR,
                                      label = ID)) +
  geom_point() +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=ll99, colour = "ll99 unadjusted")) +
  geom_line(aes(y=ul99, color = "ul99 unadjusted")) +
  geom_line(aes(y=ll99_MAM, color = "ll99 MAM")) +
  geom_line(aes(y=ul99_MAM, color = "ul99 MAM")) +
  labs(title = "Funnel Plot with both unadjusted and mam control limits",
       x = "Expected Number of Deaths",
       y = "HSMR")
funplot_MAM
funplot_MAM_outliers <- df6 %>%
  filter(HMSR >= ul99_MAM |
           HMSR <= ll99_MAM)

funplot_MAM_outliers

funplot_MAM_outliers_count <- funplot_MAM_outliers %>%
  mutate(outlier = case_when(HMSR >= ul99_MAM ~ 'Above Control Limits',
                             HMSR <= ll99_MAM ~ 'Below Control Limits')) %>%
  count(outlier)
  
funplot_MAM_outliers_count
# > 4 points above control limits. 
############################################################
# Spiegelhalter AREM
glimpse(df6)
df7 <- df6 %>%
  mutate(s_squared_i = Observed/(Expected^2),
         w = 1/s_squared_i)
# Remove first row due to odd value
df8 <- df7[-1,]
# Estimate additional between-hospital variance
df9 <- df8 %>%
  mutate(tau_numerator = phi*nrow(df8)- (nrow(df8) - 1),
         tau_denominator = sum(w) - ((sum(w^2))/sum(w)),
         tau_squared = tau_numerator/tau_denominator,
         se = 1/Expected)
glimpse(df9)
# Construct control limits
df10 <- df9 %>%
  mutate(ll99_AREM = theta - cl99*sqrt(se + tau_squared),
         ul99_AREM = theta + cl99*sqrt(se + tau_squared))
# Funnel plot with AREM Control Limits Only
funplot_AREM <- ggplot(df10, 
                       aes(
                         x = Expected,
                         y = HMSR,
                         label = ID)) +
  geom_point() +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=ll99_AREM)) +
  geom_line(aes(y=ul99_AREM))
funplot_AREM
# Funnel Plot with unadjusted, MAM and AREM
funplot_AREM_MAM_Unadj <- ggplot(df10, aes(
  x=Expected, 
  y=HMSR,
  label = ID)) +
  geom_point() +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=ll99, colour = "ll99 unadjusted")) +
  geom_line(aes(y=ul99, color = "ul99 unadjusted")) +
  geom_line(aes(y=ll99_MAM, color = "ll99 MAM")) +
  geom_line(aes(y=ul99_MAM, color = "ul99 MAM")) +
  geom_line(aes(y=ll99_AREM, color = "ll99 AREM")) +
  geom_line(aes(y=ul99_AREM, color = "ul99 AREM")) +
  labs(title = "Funnel Plot with Unadjusted, MAM and AREM CL",
       x = "Expected Number of Deaths",
       y = "HSMR")
funplot_AREM_MAM_Unadj
# Funnel plot with just MAM and AREM
funplot_AREM_MAM <- ggplot(df10, aes(
  x=Expected, 
  y=HMSR,
  label = ID)) +
  geom_point() +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=ll99_MAM, color = "ll99 MAM")) +
  geom_line(aes(y=ul99_MAM, color = "ul99 MAM")) +
  geom_line(aes(y=ll99_AREM, color = "ll99 AREM")) +
  geom_line(aes(y=ul99_AREM, color = "ul99 AREM")) +
  labs(title = "Funnel Plot with just MAM and AREM CL",
       x = "Expected Number of Deaths",
       y = "HSMR")


ggplot(df10, aes(
  x=Expected, 
  y=HMSR,
  label = ID)) +
  geom_point() +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=ll99, color = "Unadjusted")) +
  geom_line(aes(y=ul99, color = "Unadjusted")) +
  geom_line(aes(y=ll99_MAM, color = "MAM")) +
  geom_line(aes(y=ul99_MAM, color = "MAM")) +
  geom_line(aes(y=ll99_AREM, color = "AREM")) +
  geom_line(aes(y=ul99_AREM, color = "AREM")) +
  labs(
       x = "Expected Number of Deaths",
       y = "HSMR", 
       color = "99.7 Control Limits")


df10 %>%
  mutate(
    HSMR = HMSR * 100,
    theta_
  )

glimpse(df10)
outlier_table <- df10 %>%
  summarise(
    Unadjusted_Outliers = sum(HSMR > ul99 | HSMR < ll99),
    MAM_Outliers = sum(HSMR > ul99_MAM | HSMR < ll99_MAM),
    AREM_Outliers = sum(HSMR > ul99_AREM | HSMR < ll99_AREM),
  )

outlier_table

# Converting wide to long data
df10_long
df10_long <- gather(df10, 
                    key =`Control Limit Type`, 
                    value, 
                    ll99, 
                    ul99, 
                    ll99_MAM, 
                    ll99_AREM, 
                    ul99_MAM,
                    ll99_AREM,
                    ul99_AREM)
glimpse(df10_long)
df10_long2 <- df10_long %>%
  mutate(`Control Limit` = case_when(`Control Limit Type` == 'll99' |
                                       `Control Limit Type` == 'ul99' ~ 'unadjusted',
                                     `Control Limit Type` == 'll99_MAM' |
                                       `Control Limit Type` == 'ul99_MAM' ~ 'MAM',
                                     `Control Limit Type` == 'll99_AREM' |
                                       `Control Limit Type` == 'ul99_AREM' ~ 'AREM',
  ))

unique(df10_long2$`Control Limit`)

# replot
funplot_MAM_AREM2 <- ggplot(df10_long2, 
                            aes(x = Expected,
                                y = HMSR,
                                label = ID,
                                ))+ 
  geom_point() +
  geom_line(aes(y=theta)) +
  geom_line(aes(y=value, linetype = `Control Limit Type`, color = `Control Limit`)) 
funplot_MAM_AREM2


df10_long3 <-  df10_long2 %>%
  mutate(ll99_value = ifelse(`Control Limit Type` == "ll99", value, 0)
         )
view(df10_long3)  


ggplot(df10_long3,
       aes(x = Expected,
        y = HMSR,
                                label = ID,
                            ))+ 
  geom_point() +
  geom_line(aes(y=theta)) +
  geom_line(aes(y = ll99_value[1:108]))
                
          
df10_long3

na.o









