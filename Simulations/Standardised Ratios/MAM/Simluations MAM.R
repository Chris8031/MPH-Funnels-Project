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
library(pROC)
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
  data3 %>%
    mutate(ll99_MAM = theta - a99 * s_i * sqrt(phi),
           ul99_MAM = theta + a99 * s_i * sqrt(phi),
           outlier = (SR < ll99_MAM) | (SR > ul99_MAM)       
    )
}


# Initialise R workers to perform the simulations in parallel
future::plan(multisession)

# Perform the simulations
# The future_map_dfr function applies a function to each element in a
#   sequence/list/vector, and 'stacks' each result into a single large dataframe
outlier_test <- future_map_dfr(1:10000,
                               test_limits,
                               # Show a progress bar
                               .progress = TRUE,
                               # Need to specify a random seed for
                               #   parallel-safe RNG
                               .options = furrr_options(seed = 8)
)

# Close the initialised R workers since we don't need them any more
plan(sequential)

glimpse(outlier_test)


confusion_matrix_iter2 <- outlier_test %>%
  filter(overdispersed != FALSE) %>%
  group_by(iteration) %>%
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
writexl::write_xlsx(confusion_matrix_iter2, 
                    path = "D:\\Github\\MPH-Funnels-Project\\Simulations\\Standardised Ratios\\MAM\\Speigelhalter_MAM_confusion_matrix_10000.xlsx")


#######################################################
# Produce ROC curve and AUC
roc_plot <- pROC::roc(confusion_matrix_iter2$sensitivity, confusion_matrix_iter2$specificity, plot = TRUE)
roc_plot
pROC::plot.roc(confusion_matrix_iter2$sensitivity, confusion_matrix_iter2$specificity)
############################################################################

confusion_matrix_summary <- confusion_matrix_iter2 %>%
  summarise(
    "Number of iterations" = n(),
    "Mean number of True Positives" = mean(true_positive),
    "Mean number of False Positive" = mean(false_positive),
    "Mean number of True Negatives" = mean(true_negative),
    "Mean number of False Negatives" = mean(false_negative),
    "Mean number of Outliers Detected" = mean(total_number_outliers_labelled),
    "Overall Sensitivity" = mean(sensitivity),
    "Overall Specificity" = mean(specificity),
    "Overall Predictive Accuracy" = mean(predictive_accuracy)
    
  )


view(confusion_matrix_summary)
writexl::write_xlsx(confusion_matrix_summary, 
                    path = "D:\\Github\\MPH-Funnels-Project\\Simulations\\Standardised Ratios\\MAM\\Speigelhalter_MAM_10000.xlsx")















####################################################################
# Remove iterations when overdispersion = FALSE
confusion_matrix_iter2 <- confusion_matrix_iter %>%
  filter(overdispersed != FALSE)

# Summarise findings of the whole simulation
confusion_matrix_overall <- confusion_matrix_iter2 %>%
  group_by(true_outlier) %>%
  summarise(
    correct_m = mean(correct),
    correct_l95 = quantile(correct, p = 0.025),
    correct_u95 = quantile(correct, p = 0.975),
    incorrect_m = mean(incorrect),
    incorrect_l95 = quantile(incorrect, p = 0.025),
    incorrect_u95 = quantile(incorrect, p = 0.975)
  )
view(confusion_matrix_overall)
sensitivity <- confusion_matrix_overall %>%
  filter(true_outlier == TRUE) 
specificity <- confusion_matrix_overall %>%
  filter(true_outlier == FALSE)

confusion_matrix_overall2 <- data.frame(c(specificity[2], sensitivity[2]))

confusion_matrix_overall3 <- confusion_matrix_overall2 %>%
  rename("Specificity" = correct_m,
         "Sensitivity" = correct_m.1) %>%
  gather(key = "Metric",
         value = "Value")

confusion_matrix_overall3


MAM_plt <- confusion_matrix_overall3 %>%
  ggplot(aes(x = Metric, y = Value, fill = Metric)) +
  geom_col() +
  labs(title = "Plot of Sensitivity and Specificity for Simulations Using MAM")
MAM_plt
