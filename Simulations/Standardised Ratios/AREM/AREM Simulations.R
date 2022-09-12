#################
# Preable
# Load Packages
library(FunnelPlotR)
library(tidyverse)
library(DescTools)
library(MASS)
library(ids)
library(furrr)
library(readxl)
library(writexl)
set.seed(8)
###################################################
# Define function
test_limits <- function(iteration) {
  # Import data from AIHW dataset
  AIHW_GroupD1 <- read_excel("D:/Github/MPH-Funnels-Project/Standardised Ratios Data/AIHW Dataset/AIHW_GroupD1.xlsx")
  AIHW_GroupD1 <- AIHW_GroupD1 %>%
    mutate(HSMR = Observed/Expected)
  id <- ids::random_id(100, bytes = 2) # generate random IDs for hospitals
  # Generate randomised parameters for generating standardised ratios
  mean_parameter <- rnorm(1, mean = 1, sd = 0.25) # Rationale based on prior
  # Assumption that 95% of the time the mean should lie between
  # 0.5 and 1.5 and there should not be a lot of dispersion in terms of the mean SR
  # Create large dataset to sample standardised ratios from (no outliers)
  big_dataset <- rnorm(10000, mean = mean_parameter, sd = 0.5)
  # SD based on SD from AIHW Dataset
  filtered_big_dataset <- big_dataset[big_dataset > 0]
  # Draw sample from this dataset
  SR_no_outlier <- sample(filtered_big_dataset, 99)
  outlier_parameter <- mean(filtered_big_dataset) + 3.5*(sd(filtered_big_dataset))
  # reason for 3.5 is that greater buffer for outlier, although 3 sd should be 
  outlier_SR <- rnorm(1, mean = outlier_parameter, sd = 0.1)
  # Should approximate 5% of the outlier_parameter, meaning that 95% of the time,
  # this outlier ratio should fall between 10% of mean + 3.5sigma 
  outlier_SR
  SR <- c(SR_no_outlier, outlier_SR) # Combine SR with outlier
  # Generate Observed Counts from big dataset
  big_dataset2 <- rpois(100000, lambda = AIHW_GroupD1$Observed)
  filtered_big_dataset2 <- big_dataset2[big_dataset2 > 0]
  # for counts of zero, control limits are mathematically deemed as
  # inf/-inf. Whilst observed counts of zero deaths certainly exist, for the sake of 
  # this simulation, counts of 0 have been excluded.
  observed <- sample(filtered_big_dataset2, 100)
  expected <- observed/SR
  data <- data.frame(
    iteration = iteration, 
    id = id,
    observed = observed,
    expected = expected,
    SR = SR,
    true_outlier = c(rep(FALSE, 99), rep(TRUE, 1))
  )
  # Generating critical values for control limits
  a99 <- qnorm(0.999)
  # Compute control limits - make sure to assign result to output
  data1 <- data %>%
    mutate(
      theta = 1,
      s_i = sqrt(1/expected),
      z_trans = 2*(sqrt(observed) - sqrt(expected)))
  data2 <- data1 %>%
    mutate(z_adj = DescTools::Winsorize(data1$z_trans, probs = c(0.1, 0.9)))
  phi <- (sum(data2$z_adj^2))/nrow(data2)
  data3 <- data2 %>%
    mutate(overdispersed = if_else(phi > (nrow(data2)-1)/nrow(data2) 
                                   & phi > 1 + 2*(sqrt(2/nrow(data2))), TRUE, FALSE)
    )
  # Now we create control limits
  glimpse(data3)
  data3 %>%
    mutate(s_squared_i = observed/(expected^2)) %>%
    mutate(w = 1/s_squared_i) %>% # Equation 3.14
    mutate(tau_numerator = phi*nrow(data3) - (nrow(data3) - 1),
           tau_denominator = sum(w) - (sum(w^2))/sum(w)) %>%
    mutate(tau_squared = tau_numerator/tau_denominator) %>%
    mutate(se = 1/expected) %>%
    mutate(ll99_AREM = theta - a99 * sqrt(se + tau_squared),
           ul99_AREM = theta + a99 * sqrt(se + tau_squared),
           outlier = (SR < ll99_AREM) | (SR > ul99_AREM))
}
# Initialise R workers to perform the simulations in parallel
future::plan(multisession)

# Perform the simulations
# The future_map_dfr function applies a function to each element in a
#   sequence/list/vector, and 'stacks' each result into a single large dataframe
outlier_test <- future_map_dfr(1:100,
                               test_limits,
                               # Show a progress bar
                               .progress = TRUE,
                               # Need to specify a random seed for
                               #   parallel-safe RNG
                               .options = furrr_options(seed = 8)
)
# Close the initialised R workers since we don't need them any more
plan(sequential)

confusion_matrix_iter2 <- outlier_test %>%
  group_by(iteration) %>%
  filter(overdispersed == TRUE) %>%
  summarise(
    true_positive = sum(outlier == TRUE & true_outlier == TRUE),
    false_positive = sum(outlier == TRUE & true_outlier == FALSE),
    true_negative = sum(outlier == FALSE & true_outlier == FALSE),
    false_negative = sum(outlier == FALSE & true_outlier == TRUE),
    total_number_outliers_labelled = sum(outlier == TRUE),
    sensitivity = true_positive / (true_positive + false_negative),
    specificity = true_negative / (false_positive + true_negative),
    outlier_found = ifelse(true_positive > 0, TRUE, FALSE),
    number_of_false_outliers = false_positive,
    number_correct = sum(outlier == true_outlier),
    number_incorrect = sum(outlier != true_outlier),
    predictive_accuracy = number_correct/(number_correct + number_incorrect),
    test_error_rate = 1 - predictive_accuracy
  ) %>%
  ungroup()

view(confusion_matrix_iter2)

confusion_matrix_summary <- confusion_matrix_iter2 %>%
  summarise(
    "Number of iterations" = n(),
    "Mean number of True Positives" = mean(true_positive),
    "Mean number of False Positive" = mean(false_positive),
    "Mean number of True Negatives" = mean(true_negative),
    "Mean number of False Negatives" = mean(false_negative),
    "Mean number of Outliers Detected" = mean(total_number_outliers_labelledd),
    "Overall Sensitivity" = mean(sensitivity),
    "Overall Specificity" = mean(specificity),
    "Overall Predictive Accuracy" = mean(predictive_accuracy)
  )

view(confusion_matrix_summary)
write_xlsx(confusion_matrix_summary, path = "D:/Github/MPH-Funnels-Project/Simulations/Standardised Ratios/AREM/summary_100.xlsx")
