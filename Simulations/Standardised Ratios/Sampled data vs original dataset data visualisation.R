# Preamble
# Load Packages
library(tidyverse)
library(DescTools)
library(MASS)
library(ids)
library(furrr)
set.seed(8)
library(readxl)
library(ggpubr)
###################################################
observed_counts.fn <- function(iteration) {
AIHW_GroupD1 <- read_excel("Real world data application/AIHW Dataset/AIHW_GroupD1.xlsx")
AIHW_GroupD1$Observed
sample_observed <- rpois(109, lambda = AIHW_GroupD1$Observed)
data <- data.frame(
  iteration, 
  sample_observed
)
}
################################################
observed_counts <- future_map_dfr(
  1:100,
  observed_counts.fn,
  .progress = TRUE,
  .options = furrr_options(seed = 8)
)
View(observed_counts)
observed_counts2 <- observed_counts %>%
  mutate(iteration.fct = as_factor(iteration)) 

AIHW_data <- data.frame(
  AIHW_GroupD1$Observed
)

AIHW_data2 <- AIHW_data %>%
  rename(sample_observed = AIHW_GroupD1.Observed) %>%
  mutate(iteration = 101,
         itertation.fct = as.factor(iteration)
         )
AIHW_data3 <- AIHW_data2 %>%
  relocate(iteration)
glimpse(observed_counts2)
glimpse(AIHW_data3)


iteration_new <- as.integer(AIHW_data3$iteration)
sample_observed_new <- as.integer(AIHW_data3$sample_observed)
AIHW_data3 <- data.frame(
  iteration_new,
  sample_observed_new,
  AIHW_data3$itertation.fct
)
AIHW_data4 <- AIHW_data3 %>%
  rename(
    iteration.fct = AIHW_data3.itertation.fct
  )
AIHW_data5 <- AIHW_data4 %>%
  rename(iteration = iteration_new,
         sample_observed = sample_observed_new)
  


glimpse(observed_counts2)
glimpse(AIHW_data5)
combined1 <- plyr::rbind.fill(
  observed_counts2,
  AIHW_data5
)
  
combined2 <- combined1 %>%
  mutate(dataset = 
           ifelse(iteration != 101, "Sample", "Original"))

unique(combined2$dataset)

combined3 <- combined2 %>%
  mutate(dataset.fct = as.factor(combined2$dataset)) %>%
  dplyr::select(-dataset)

combined3 %>%
  ggplot(aes(x = sample_observed, color = iteration.fct, fill = dataset.fct)) +
  geom_density(alpha = 0.7)




combined3 %>%
  ggplot(aes(x = sample_observed,  fill = dataset.fct, color = iteration.fct)) +
  geom_density(alpha = 0.5) +
  labs(
    x = "Observed Number of Counts",
    y = "Density",
    fill = "Dataset Used"
  ) +
  labs_pubr() 



combined_no_AIHW <- combined3 %>%
  filter(iteration.fct != 101)
#####
combined_no_AIHW %>%
  ggplot(aes(x = sample_observed, fill = iteration.fct, color = iteration.fct)) +
  geom_density(alpha = 0.5)  +
  labs(
    x = "Observed Number of Counts",
    y = "Density",
    fill = "Sample Number",
    color = "Sample Number"
  ) +
  labs_pubr() +
  theme_pubr()
###########
combined3 %>%
  ggplot(aes(x = sample_observed,  fill = dataset.fct)) +
  geom_boxplot() +
  labs(
    title = "Boxplot Comparing the Distribution of the Original AIHW Dataset to Generated Samples",
  x = "Observed Number of Counts",
  color = "Dataset Used"
  ) +
  ggpubr::theme_pubclean() +
    labs_pubr() +
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank())
  
#####################################################################
kstest.fn <- function(iteration) {
  AIHW_GroupD1 <- read_excel("Real world data application/AIHW Dataset/AIHW_GroupD1.xlsx")
  observed <- AIHW_GroupD1$Observed
  sample_observed <- rpois(109, lambda = AIHW_GroupD1$Observed)
  ks_result <- ks.test(observed, sample_observed)
  ks_result[2]
  p_value <- as.numeric(ks_result[2])
  p_value
  
  data <- data.frame(
    iteration, 
    p_value
  )
}
ks_results_final <- future_map_dfr(
  1:100,
  kstest.fn,
  .progress = TRUE,
  .options = furrr_options(seed = 8)
)
view(ks_results_final)
ks_results_final %>%
  ggplot(aes(x = p_value)) +
  geom_histogram(bins = 10, fill = "blue", alpha = 0.6) +
  labs(x = "P-Value",
       y = "Count") +
  labs_pubr() +
  ggpubr::theme_pubr()

min(ks_results_final$p_value)







