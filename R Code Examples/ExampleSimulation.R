library(tidyverse)
library(furrr)

set.seed(2021)

# Define a function which will generate a random dataset containing outliers,
#   generate control limits, and check whether outliers were detected.
# We also record whether a given datapoint was generated as an outlier, so that
#   we can assess the sensitivity & specificity
test_limits <- function(iteration) {
  # Generate the denominators for the sample - this has the size and range
  #   hard-coded, but will likely specify those as parameters in the future
  denominators <- sample(1:100, size = 30, replace = T)

  # Generate the observed counts
  # To simulate outliers, we generate a subset of observations with a greater
  #   probability of the event occurring. These are placed at the end of the
  #   observations so that we can label them easily.
  numerators <- c(
    rbinom(numerators = 20, size = denominators[1:20], p = 0.5),
    rbinom(numerators = 10, size = denominators[21:30], p = 0.8)
  )

  # Pack the generated observations into a single dataframe
  data <- data.frame(
    # Will be used to identify observations from a given iteration
    # of the simulation
    iteration = iteration,
    # Use letters of the alphabet as group labels
    id = letters[1:30],
    numerators = numerators,
    denominators = denominators,
    # Label whether the observation was generated as an outlier or not
    true_outlier = c(rep(FALSE, 20), rep(TRUE, 10))
  )

  # Generate the critical values to use for the control limits
  #   We ignore the 95% limits for now
  a99 <- qnorm(.999) # 3.090232....

  # This dplyr code performs the limit calculations and assesses whether the
  #   observation was outlying
  # By not assigning this result to an output, it is automatically returned from
  #   the function
  data %>%
    mutate(
      p = numerators / denominators,
      target = sum(numerators) / sum(denominators),
      target_trans = asin(sqrt(target)),
      se = 1 / (2 * sqrt(denominators)),
      ll99_trans = target_trans - a99 * se,
      ul99_trans = target_trans + a99 * se,
      ll99 = sin(ll99_trans)^2,
      ul99 = sin(ul99_trans)^2,
      # Flag whether the observation was below/above the control limits
      outlier = (p < ll99) | (p > ul99)
    ) %>%
    # Remove the transformed target/limits from the returned dataframe, since we
    #   won't need them
    select(-c(target_trans, ll99_trans, ul99_trans))
}

# Initialise R workers to perform the simulations in parallel
plan(multisession)

# Perform the simulations
# The future_map_dfr function applies a function to each element in a
#   sequence/list/vector, and 'stacks' each result into a single large dataframe
outlier_test <- future_map_dfr(1:10000,
  test_limits,
  # Show a progress bar
  .progress = TRUE,
  # Need to specify a random seed for
  #   parallel-safe RNG
  .options = furrr_options(seed = 2021)
)

# Close the initialised R workers since we don't need them any more
plan(sequential)

# Next we use this large results table to check the proportion of true/false
#   positives/negatives at each iteration
confusion_matrix_iter <- outlier_test %>%
  # Perform the summary for each iteration separately, so that we can assess
  #  the variance across simulations
  # Group the summary by whether or not the observation was truly an outlier
  group_by(iteration, true_outlier) %>%
  # Calculate the proportion of times that the control limits agreed with the
  #   data generation - this is our sensitivity & specificity
  summarise(
    correct = mean(outlier == true_outlier),
    incorrect = mean(outlier != true_outlier)
  ) %>%
  ungroup()

# Summarise findings of the whole simulation
confusion_matrix_overall <- confusion_matrix_iter %>%
  group_by(true_outlier) %>%
  summarise(
    correct_m = mean(correct),
    correct_l95 = quantile(correct, p = 0.025),
    correct_u95 = quantile(correct, p = 0.975),
    incorrect_m = mean(incorrect),
    incorrect_l95 = quantile(incorrect, p = 0.025),
    incorrect_u95 = quantile(incorrect, p = 0.975)
  )

sensitivity <- confusion_matrix_overall %>%
  filter(true_outlier == TRUE)

confusion_matrix_iter %>%
  filter(true_outlier == TRUE) %>%
  ggplot(., aes(x = correct)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = sensitivity$correct_m) +
  geom_vline(xintercept = sensitivity$correct_l95, linetype = "dashed") +
  geom_vline(xintercept = sensitivity$correct_u95, linetype = "dashed") +
  theme_bw()
