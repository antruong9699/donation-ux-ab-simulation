# Load the data.table library
library(data.table)

# Set the seed to make sure we can repeat results
set.seed(123)

# Define the sample size
n <- 551 * 2  # sample size

# Create dataset with donation frequencies
donations_frequency <- data.table(
  Donor_ID = 1:n,
  Small_Preset_Frequency = rpois(n, lambda = 3),  # Small Preset Frequency
  Large_Preset_Frequency = rpois(n, lambda = 2)   # Large Preset Frequency
)

# Convert donation frequencies to binary success/failure
donations_frequency[, Small_Donated := ifelse(Small_Preset_Frequency > 0, 1, 0)]
donations_frequency[, Large_Donated := ifelse(Large_Preset_Frequency > 0, 1, 0)]

# Calculate the total number of successes for each group
small_successes <- sum(donations_frequency$Small_Donated)
large_successes <- sum(donations_frequency$Large_Donated)

# Calculate total sample sizes for each group
total_small <- n
total_large <- n

# Apply proportion test
prop_test_result <- prop.test(c(small_successes, large_successes), c(total_small, total_large))

# Print the result of the proportion test
print(prop_test_result)


#################################################################################

# Load required libraries
library(data.table)
library(DT)
library(pwr)

# Simulating data for Control and Experiment groups
group_Control <- rnorm(551, 14, 5.5)
group_Experiment <- rnorm(551, 18, 37.3)

# Perform a t-test comparing Control and Experiment groups
t_test_result <- t.test(group_Control, group_Experiment, var.equal = TRUE)

# Print the t-test results
print(t_test_result)

# Create a data frame to combine and view the generated data
generated_data <- data.frame(
  group = rep(c("Control", "Experiment"), each = 551),
  value = c(group_Control, group_Experiment)
)

# Print the generated data frame in the console
print(generated_data)

head(generated_data[generated_data$group == "Control", ])
head(generated_data[generated_data$group == "Experiment", ])

# View the generated data interactively using DT (if you are using an environment that supports it)
datatable(generated_data)

# Simulate the t-test process 1000 times to collect p-values and determine significance proportion
save_ps <- numeric(1000)
for(i in 1:1000){
  group_Control <- rnorm(551, 14, 5.5)
  group_Experiment <- rnorm(551, 18, 37.3)
  t_results <- t.test(group_Control, group_Experiment, var.equal = TRUE)
  save_ps[i] <- t_results$p.value
}

# Calculate the proportion of significant p-values (p < 0.05)
prop_p <- length(save_ps[save_ps < 0.05]) / 1000

# Print the proportion of significant p-values
print(prop_p)

# Identify and count the non-significant results (p > 0.05)
non_significant_ps <- save_ps[save_ps > 0.05]
num_non_significant <- length(non_significant_ps)
prop_non_significant <- num_non_significant / 1000

# Print the number and proportion of non-significant results
print(paste("Number of non-significant p-values (p > 0.05):", num_non_significant))
print(paste("Proportion of non-significant p-values (p > 0.05):", prop_non_significant))

##############################################################################


# Load required libraries
library(DT)

# Define parameters
num_simulations <- 1000
sample_size_per_group <- 551
mean_control <- 14
sd_control <- 5.5
mean_experiment <- 18
sd_experiment <- 37.3
alpha <- 0.05

# Initialize vectors to store results
p_values <- numeric(num_simulations)
effect_sizes <- numeric(num_simulations)

# Run 1000 simulations
for (i in 1:num_simulations) {
  # Generate data for control and experimental groups
  group_Control <- rnorm(sample_size_per_group, mean_control, sd_control)
  group_Experiment <- rnorm(sample_size_per_group, mean_experiment, sd_experiment)
  
  # Perform t-test
  t_results <- t.test(group_Control, group_Experiment, var.equal = TRUE)
  
  # Store p-value and effect size (mean difference)
  p_values[i] <- t_results$p.value
  effect_sizes[i] <- mean(group_Experiment) - mean(group_Control)
}

# Calculate true effect size
true_effect <- mean_experiment - mean_control

# Calculate the 95% confidence interval for the effect sizes from the simulations
ci_95 <- quantile(effect_sizes, c(0.025, 0.975))
ci_95

# Classify results based on p-value and true effect
false_positives <- sum(p_values < alpha & true_effect == 0) / num_simulations * 100  # Type I error rate
true_negatives <- sum(p_values >= alpha & true_effect == 0) / num_simulations * 100  # True negative rate
false_negatives <- sum(p_values >= alpha & true_effect != 0) / num_simulations * 100 # Type II error rate
true_positives <- sum(p_values < alpha & true_effect != 0) / num_simulations * 100   # True positive rate

# Create a summary table of the results
results_table <- data.frame(
  Scenario = c("Scenario 1: False Positives & True Negatives", "Scenario 2: False Negatives & True Positives"),
  True_Effect = true_effect,
  CI_95_Lower = ci_95[1],
  CI_95_Upper = ci_95[2],
  False_Positives_Percentage = c(false_positives, NA),
  True_Negatives_Percentage = c(true_negatives, NA),
  False_Negatives_Percentage = c(NA, false_negatives),
  True_Positives_Percentage = c(NA, true_positives)
)

# Display the summary table using DT::datatable()
DT::datatable(results_table, 
          options = list(pageLength = 5, autoWidth = TRUE),
          caption = "Summary of Simulation Results for Effect Sizes, False Positives, True Negatives, False Negatives, and True Positives")


################################################################################
# Load required libraries
library(DT)      # For datatable visualization
library(dplyr)   # For pipe operator (%>%)
library(pwr)     # For power calculation

# Define parameters
num_simulations <- 1000
sd_control <- 11.5
sd_experiment <- 14
alpha <- 0.05
true_effect <- 14 - 11.5 # Effect size of 2.5

# Calculate the required sample size for power of 0.8
effect_size <- true_effect / sd_experiment; effect_size
sample_size_per_group <- ceiling(pwr.t.test(d = effect_size, power = 0.8, sig.level = alpha, type = "two.sample", alternative = "greater")$n); sample_size_per_group

# Scenario 1: No True Effect (mean_control = mean_experiment)
mean_control <- 4
mean_experiment <- 4  # No true effect

# Initialize vectors to store results
p_values_no_effect <- numeric(num_simulations)

# Run simulations for Scenario 1 (No Effect)
for (i in 1:num_simulations) {
  group_Control <- rnorm(sample_size_per_group, mean_control, sd_control)
  group_Experiment <- rnorm(sample_size_per_group, mean_experiment, sd_experiment)
  
  # Perform Student's t-test (assuming equal variance, one-tailed test for greater mean)
  t_results <- t.test(group_Control, group_Experiment, var.equal = TRUE, alternative = "greater")
  
  # Store p-value
  p_values_no_effect[i] <- t_results$p.value
}

# Calculate False Positives and True Negatives for Scenario 1
false_positives <- sum(p_values_no_effect < alpha) / num_simulations * 100  # Type I error rate
true_negatives <- sum(p_values_no_effect >= alpha) / num_simulations * 100  # True negative rate

# Scenario 2: True Effect (mean_control != mean_experiment)
mean_control <- 11.5
mean_experiment <- 14  # True effect of 2.5

# Initialize vectors to store results
p_values_true_effect <- numeric(num_simulations)
effect_sizes <- numeric(num_simulations)

# Run simulations for Scenario 2 (True Effect)
for (i in 1:num_simulations) {
  group_Control <- rnorm(sample_size_per_group, mean_control, sd_control)
  group_Experiment <- rnorm(sample_size_per_group, mean_experiment, sd_experiment)
  
  # Perform Student's t-test (assuming equal variance, one-tailed test for greater mean)
  t_results <- t.test(group_Control, group_Experiment, var.equal = TRUE, alternative = "greater")
  
  # Store p-value and effect size
  p_values_true_effect[i] <- t_results$p.value
  effect_sizes[i] <- mean(group_Experiment) - mean(group_Control)
}

# Calculate True Effect Size and 95% Confidence Interval
ci_95 <- quantile(effect_sizes, c(0.025, 0.975))

# Calculate False Negatives and True Positives for Scenario 2
false_negatives <- sum(p_values_true_effect >= alpha) / num_simulations * 100  # Type II error rate
true_positives <- sum(p_values_true_effect < alpha) / num_simulations * 100   # True positive rate

# Create a summary table of the results
results_table <- data.frame(
  Research_Question = c("Question 1", "Question 1", "Question 2", "Question 2"),
  Scenario = c("No Effect", "Effect: (True Effect Size = 2.5)", "No Effect", "Effect: (True Effect Size = 2.5)"),
  Mean_Effect_in_Simulated_Data = c(0, true_effect, 0, true_effect),
  CI_95_Lower = c(NA, ci_95[1], NA, ci_95[1]),
  CI_95_Upper = c(NA, ci_95[2], NA, ci_95[2]),
  Percentage_of_False_Positives = c(false_positives, NA, false_positives, NA),
  Percentage_of_True_Negatives = c(true_negatives, NA, true_negatives, NA),
  Percentage_of_False_Negatives = c(NA, false_negatives, NA, false_negatives),
  Percentage_of_True_Positives = c(NA, true_positives, NA, true_positives)
)

# Display the summary table using DT::datatable() with custom formatting
results_table %>%
  datatable(rownames = FALSE,
            options = list(pageLength = 5, autoWidth = TRUE),
            caption = paste("Summary of Simulation Results for Research Questions, Effect Sizes,",
                            "False Positives, True Negatives, False Negatives, and True Positives",
                            "(Power = 0.8, Sample Size per Group =", sample_size_per_group, ")")) %>%
  formatStyle(
    columns = c('Scenario', 'Mean_Effect_in_Simulated_Data', 'CI_95_Lower', 'CI_95_Upper',
                'Percentage_of_False_Positives', 'Percentage_of_True_Negatives',
                'Percentage_of_False_Negatives', 'Percentage_of_True_Positives'),
    fontWeight = 'bold',
    textAlign = 'center'
  )



#################################################################################

# Load required library
library(DT)

# Define parameters
num_simulations <- 1000
sample_size_per_group <- 551
alpha <- 0.05

# Scenario 1: No True Effect (generate binary data and calculate probabilities afterward)

# Initialize vectors to store results
p_values_no_effect <- numeric(num_simulations)
prob_control_empirical <- numeric(num_simulations)
prob_experiment_empirical <- numeric(num_simulations)

# Run simulations for Scenario 1 (No Effect)
for (i in 1:num_simulations) {
  # Generate binary outcomes (0 or 1) for control and experimental groups without predefined probabilities
  group_Control <- rbinom(sample_size_per_group, 1, 0.5)
  group_Experiment <- rbinom(sample_size_per_group, 1, 0.5)
  
  # Calculate empirical probabilities
  prob_control_empirical[i] <- mean(group_Control)
  prob_experiment_empirical[i] <- mean(group_Experiment)
  
  # Perform proportion test
  success_counts <- c(sum(group_Control), sum(group_Experiment))
  sample_sizes <- c(sample_size_per_group, sample_size_per_group)
  prop_results <- prop.test(success_counts, sample_sizes)
  
  # Store p-value
  p_values_no_effect[i] <- prop_results$p.value
}

# Calculate False Positives and True Negatives for Scenario 1
false_positives <- sum(p_values_no_effect < alpha) / num_simulations * 100  # Type I error rate
true_negatives <- sum(p_values_no_effect >= alpha) / num_simulations * 100  # True negative rate

# Scenario 2: True Effect (generate binary data and calculate probabilities afterward)

# Initialize vectors to store results
p_values_true_effect <- numeric(num_simulations)
effect_sizes <- numeric(num_simulations)
prob_control_empirical_2 <- numeric(num_simulations)
prob_experiment_empirical_2 <- numeric(num_simulations)

# Run simulations for Scenario 2 (True Effect)
for (i in 1:num_simulations) {
  # Generate binary outcomes (0 or 1) for control and experimental groups without predefined probabilities
  group_Control <- rbinom(sample_size_per_group, 1, 0.5)
  group_Experiment <- rbinom(sample_size_per_group, 1, 0.6)
  
  # Calculate empirical probabilities
  prob_control_empirical_2[i] <- mean(group_Control)
  prob_experiment_empirical_2[i] <- mean(group_Experiment)
  
  # Perform proportion test
  success_counts <- c(sum(group_Control), sum(group_Experiment))
  sample_sizes <- c(sample_size_per_group, sample_size_per_group)
  prop_results <- prop.test(success_counts, sample_sizes)
  
  # Store p-value and effect size (difference in proportions)
  p_values_true_effect[i] <- prop_results$p.value
  effect_sizes[i] <- (sum(group_Experiment) / sample_size_per_group) - (sum(group_Control) / sample_size_per_group)
}

# Calculate True Effect Size and 95% Confidence Interval
true_effect <- mean(prob_experiment_empirical_2) - mean(prob_control_empirical_2)
ci_95 <- quantile(effect_sizes, c(0.025, 0.975))

# Calculate False Negatives and True Positives for Scenario 2
false_negatives <- sum(p_values_true_effect >= alpha) / num_simulations * 100  # Type II error rate
true_positives <- sum(p_values_true_effect < alpha) / num_simulations * 100   # True positive rate

# Create a summary table of the results
results_table <- data.frame(
  Research_Question = c("Question 1", "Question 1", "Question 2", "Question 2"),
  Scenario = c("No Effect", "Effect: (True Effect Size = calculated)", "No Effect", "Effect: (True Effect Size = calculated)"),
  Mean_Effect_in_Simulated_Data = c(0, true_effect, 0, true_effect),
  CI_95_Lower = c(NA, ci_95[1], NA, ci_95[1]),
  CI_95_Upper = c(NA, ci_95[2], NA, ci_95[2]),
  Percentage_of_False_Positives = c(false_positives, NA, false_positives, NA),
  Percentage_of_True_Negatives = c(true_negatives, NA, true_negatives, NA),
  Percentage_of_False_Negatives = c(NA, false_negatives, NA, false_negatives),
  Percentage_of_True_Positives = c(NA, true_positives, NA, true_positives)
)

# Display the summary table using DT::datatable() with custom formatting
datatable(results_table, 
          rownames = FALSE,
          options = list(pageLength = 5, autoWidth = TRUE),
          caption = "Summary of Simulation Results for Research Questions, Effect Sizes, False Positives, True Negatives, False Negatives, and True Positives") %>%
  formatStyle(
    columns = c('Scenario', 'Mean_Effect_in_Simulated_Data', 'CI_95_Lower', 'CI_95_Upper',
                'Percentage_of_False_Positives', 'Percentage_of_True_Negatives',
                'Percentage_of_False_Negatives', 'Percentage_of_True_Positives'),
    fontWeight = 'bold',
    textAlign = 'center'
  )
################################################################################
# Load required libraries
library(DT)      # For datatable visualization
library(dplyr)   # For pipe operator (%>%)

# Define parameters
num_simulations <- 1000
sample_size_per_group <- 551
alpha <- 0.05

# Scenario 1: No True Effect (generate binary data and calculate probabilities afterward)

# Initialize vectors to store results
p_values_no_effect <- numeric(num_simulations)
prob_control_empirical <- numeric(num_simulations)
prob_experiment_empirical <- numeric(num_simulations)

# Run simulations for Scenario 1 (No Effect)
for (i in 1:num_simulations) {
  # Generate binary outcomes (0 or 1) for control and experimental groups without predefined probabilities
  group_Control <- rbinom(sample_size_per_group, 1, 0.5)
  group_Experiment <- rbinom(sample_size_per_group, 1, 0.5)
  
  # Calculate empirical probabilities
  prob_control_empirical[i] <- mean(group_Control)
  prob_experiment_empirical[i] <- mean(group_Experiment)
  
  # Perform proportion test
  success_counts <- c(sum(group_Control), sum(group_Experiment))
  sample_sizes <- c(sample_size_per_group, sample_size_per_group)
  prop_results <- prop.test(success_counts, sample_sizes)
  
  # Store p-value
  p_values_no_effect[i] <- prop_results$p.value
}

# Calculate False Positives and True Negatives for Scenario 1
false_positives <- sum(p_values_no_effect < alpha) / num_simulations * 100  # Type I error rate
true_negatives <- sum(p_values_no_effect >= alpha) / num_simulations * 100  # True negative rate

# Scenario 2: True Effect (generate binary data and calculate probabilities afterward)

# Initialize vectors to store results
p_values_true_effect <- numeric(num_simulations)
effect_sizes <- numeric(num_simulations)
prob_control_empirical_2 <- numeric(num_simulations)
prob_experiment_empirical_2 <- numeric(num_simulations)

# Run simulations for Scenario 2 (True Effect)
for (i in 1:num_simulations) {
  # Generate binary outcomes (0 or 1) for control and experimental groups with true effect
  group_Control <- rbinom(sample_size_per_group, 1, 0.5)
  group_Experiment <- rbinom(sample_size_per_group, 1, 0.6)
  
  # Calculate empirical probabilities
  prob_control_empirical_2[i] <- mean(group_Control)
  prob_experiment_empirical_2[i] <- mean(group_Experiment)
  
  # Perform proportion test
  success_counts <- c(sum(group_Control), sum(group_Experiment))
  sample_sizes <- c(sample_size_per_group, sample_size_per_group)
  prop_results <- prop.test(success_counts, sample_sizes)
  
  # Store p-value and effect size (difference in proportions)
  p_values_true_effect[i] <- prop_results$p.value
  effect_sizes[i] <- mean(group_Experiment) - mean(group_Control)
}

# Calculate True Effect Size and 95% Confidence Interval for Scenario 2
true_effect <- mean(prob_experiment_empirical_2) - mean(prob_control_empirical_2)
ci_95 <- quantile(effect_sizes, c(0.025, 0.975))

# Calculate False Negatives and True Positives for Scenario 2
false_negatives <- sum(p_values_true_effect >= alpha) / num_simulations * 100  # Type II error rate
true_positives <- sum(p_values_true_effect < alpha) / num_simulations * 100   # True positive rate

# Scenario 3: Extreme True Effect (272.5% Effect Size)

# Initialize vectors to store results
p_values_extreme_effect <- numeric(num_simulations)
effect_sizes_extreme <- numeric(num_simulations)
prob_control_empirical_3 <- numeric(num_simulations)
prob_experiment_empirical_3 <- numeric(num_simulations)

# Run simulations for Scenario 3 (Extreme Effect Size)
for (i in 1:num_simulations) {
  # Generate binary outcomes (0 or 1) for control group and experimental group with an extreme true effect
  group_Control <- rbinom(sample_size_per_group, 1, 0.5)
  group_Experiment <- rbinom(sample_size_per_group, 1, 1)  # Probability = 1 for extreme effect

  # Calculate empirical probabilities
  prob_control_empirical_3[i] <- mean(group_Control)
  prob_experiment_empirical_3[i] <- mean(group_Experiment)

  # Perform proportion test
  success_counts <- c(sum(group_Control), sum(group_Experiment))
  sample_sizes <- c(sample_size_per_group, sample_size_per_group)
  prop_results <- prop.test(success_counts, sample_sizes)

  # Store p-value and effect size (difference in proportions)
  p_values_extreme_effect[i] <- prop_results$p.value
  effect_sizes_extreme[i] <- mean(group_Experiment) - mean(group_Control)
}

# Calculate True Effect Size and 95% Confidence Interval for Scenario 3
true_effect_extreme <- mean(prob_experiment_empirical_3) - mean(prob_control_empirical_3)
ci_95_extreme <- quantile(effect_sizes_extreme, c(0.025, 0.975))

# Calculate False Negatives and True Positives for Scenario 3
false_negatives_extreme <- sum(p_values_extreme_effect >= alpha) / num_simulations * 100  # Type II error rate
true_positives_extreme <- sum(p_values_extreme_effect < alpha) / num_simulations * 100   # True positive rate

# Create a summary table of the results
results_table <- data.frame(
  Research_Question = c("Question 1", "Question 1", "Question 2", "Question 2", "Question 3", "Question 3"),
  Scenario = c("No Effect", "Effect: (True Effect Size = calculated)", "No Effect", "Effect: (True Effect Size = calculated)", "Extreme Effect: (True Effect Size = 272.5%)", "Extreme Effect: (True Effect Size = 272.5%)"),
  Mean_Effect_in_Simulated_Data = c(0, true_effect, 0, true_effect, true_effect_extreme, true_effect_extreme),
  CI_95_Lower = c(NA, ci_95[1], NA, ci_95[1], ci_95_extreme[1], ci_95_extreme[1]),
  CI_95_Upper = c(NA, ci_95[2], NA, ci_95[2], ci_95_extreme[2], ci_95_extreme[2]),
  Percentage_of_False_Positives = c(false_positives, NA, false_positives, NA, NA, NA),
  Percentage_of_True_Negatives = c(true_negatives, NA, true_negatives, NA, NA, NA),
  Percentage_of_False_Negatives = c(NA, false_negatives, NA, false_negatives, false_negatives_extreme, NA),
  Percentage_of_True_Positives = c(NA, true_positives, NA, true_positives, true_positives_extreme, NA)
)

# Display the summary table using DT::datatable() with custom formatting
results_table %>%
  datatable(rownames = FALSE,
            options = list(pageLength = 5, autoWidth = TRUE),
            caption = "Summary of Simulation Results for Research Questions, Effect Sizes, False Positives, True Negatives, False Negatives, and True Positives") %>%
  formatStyle(
    columns = c('Scenario', 'Mean_Effect_in_Simulated_Data', 'CI_95_Lower', 'CI_95_Upper',
                'Percentage_of_False_Positives', 'Percentage_of_True_Negatives',
                'Percentage_of_False_Negatives', 'Percentage_of_True_Positives'),
    fontWeight = 'bold',
    textAlign = 'center'
  )
################################################################################

# Load required library
library(DT)

# Define parameters
num_simulations <- 1000
sample_size_per_group <- 551
alpha <- 0.05

# Scenario 1: No True Effect (generate binary data and calculate probabilities afterward)

# Initialize vectors to store results
p_values_no_effect <- numeric(num_simulations)
prob_control_empirical <- numeric(num_simulations)
prob_experiment_empirical <- numeric(num_simulations)

# Run simulations for Scenario 1 (No Effect)
for (i in 1:num_simulations) {
  # Generate binary outcomes (0 or 1) for control and experimental groups without predefined probabilities
  group_Control <- rbinom(sample_size_per_group, 1, 0.20)
  group_Experiment <- rbinom(sample_size_per_group, 1, 0.20)
  
  # Calculate empirical probabilities
  prob_control_empirical[i] <- mean(group_Control)
  prob_experiment_empirical[i] <- mean(group_Experiment)
  
  # Perform proportion test
  success_counts <- c(sum(group_Control), sum(group_Experiment))
  sample_sizes <- c(sample_size_per_group, sample_size_per_group)
  prop_results <- prop.test(success_counts, sample_sizes)
  
  # Store p-value
  p_values_no_effect[i] <- prop_results$p.value
}

# Calculate False Positives and True Negatives for Scenario 1
false_positives <- sum(p_values_no_effect < alpha) / num_simulations * 100  # Type I error rate
true_negatives <- sum(p_values_no_effect >= alpha) / num_simulations * 100  # True negative rate

# Scenario 2: True Effect (generate binary data and calculate probabilities afterward)

# Initialize vectors to store results
p_values_true_effect <- numeric(num_simulations)
effect_sizes <- numeric(num_simulations)
prob_control_empirical_2 <- numeric(num_simulations)
prob_experiment_empirical_2 <- numeric(num_simulations)

# Run simulations for Scenario 2 (True Effect)
for (i in 1:num_simulations) {
  # Generate binary outcomes (0 or 1) for control and experimental groups without predefined probabilities
  group_Control <- rbinom(sample_size_per_group, 1, 0.20)
  group_Experiment <- rbinom(sample_size_per_group, 1, 0.25)
  
  # Calculate empirical probabilities
  prob_control_empirical_2[i] <- mean(group_Control)
  prob_experiment_empirical_2[i] <- mean(group_Experiment)
  
  # Perform proportion test
  success_counts <- c(sum(group_Control), sum(group_Experiment))
  sample_sizes <- c(sample_size_per_group, sample_size_per_group)
  prop_results <- prop.test(success_counts, sample_sizes)
  
  # Store p-value and effect size (difference in proportions)
  p_values_true_effect[i] <- prop_results$p.value
  effect_sizes[i] <- (sum(group_Experiment) / sample_size_per_group) - (sum(group_Control) / sample_size_per_group)
}

# Calculate True Effect Size and 95% Confidence Interval
true_effect <- mean(prob_experiment_empirical_2) - mean(prob_control_empirical_2)
ci_95 <- quantile(effect_sizes, c(0.025, 0.975))

# Calculate False Negatives and True Positives for Scenario 2
false_negatives <- sum(p_values_true_effect >= alpha) / num_simulations * 100  # Type II error rate
true_positives <- sum(p_values_true_effect < alpha) / num_simulations * 100   # True positive rate

# Create a summary table of the results
results_table <- data.frame(
  Research_Question = c("Question 1", "Question 1", "Question 2", "Question 2"),
  Scenario = c("No Effect", "Effect: (True Effect Size = calculated)", "No Effect", "Effect: (True Effect Size = calculated)"),
  Mean_Effect_in_Simulated_Data = c(0, true_effect, 0, true_effect),
  CI_95_Lower = c(NA, ci_95[1], NA, ci_95[1]),
  CI_95_Upper = c(NA, ci_95[2], NA, ci_95[2]),
  Percentage_of_False_Positives = c(false_positives, NA, false_positives, NA),
  Percentage_of_True_Negatives = c(true_negatives, NA, true_negatives, NA),
  Percentage_of_False_Negatives = c(NA, false_negatives, NA, false_negatives),
  Percentage_of_True_Positives = c(NA, true_positives, NA, true_positives)
)

# Display the summary table using DT::datatable() with custom formatting
datatable(results_table, 
          rownames = FALSE,
          options = list(pageLength = 5, autoWidth = TRUE),
          caption = "Summary of Simulation Results for Research Questions, Effect Sizes, False Positives, True Negatives, False Negatives, and True Positives") %>%
  formatStyle(
    columns = c('Scenario', 'Mean_Effect_in_Simulated_Data', 'CI_95_Lower', 'CI_95_Upper',
                'Percentage_of_False_Positives', 'Percentage_of_True_Negatives',
                'Percentage_of_False_Negatives', 'Percentage_of_True_Positives'),
    fontWeight = 'bold',
    textAlign = 'center'
  )
#############################################################################

# Load required library
library(DT)

# Define parameters
num_simulations <- 1000
sample_size_per_group <- 551
alpha <- 0.05

# Scenario 1: No True Effect (generate binary data and calculate probabilities afterward)

# Initialize vectors to store results
p_values_no_effect <- numeric(num_simulations)
prob_control_empirical <- numeric(num_simulations)
prob_experiment_empirical <- numeric(num_simulations)

# Run simulations for Scenario 1 (No Effect)
for (i in 1:num_simulations) {
  # Generate binary outcomes (0 or 1) for control and experimental groups without predefined probabilities
  group_Control <- rbinom(sample_size_per_group, 1, 0.20)
  group_Experiment <- rbinom(sample_size_per_group, 1, 0.20)
  
  # Calculate empirical probabilities
  prob_control_empirical[i] <- mean(group_Control)
  prob_experiment_empirical[i] <- mean(group_Experiment)
  
  # Perform proportion test
  success_counts <- c(sum(group_Control), sum(group_Experiment))
  sample_sizes <- c(sample_size_per_group, sample_size_per_group)
  prop_results <- prop.test(success_counts, sample_sizes)
  
  # Store p-value
  p_values_no_effect[i] <- prop_results$p.value
}

# Calculate False Positives and True Negatives for Scenario 1
false_positives <- sum(p_values_no_effect < alpha) / num_simulations * 100  # Type I error rate
true_negatives <- sum(p_values_no_effect >= alpha) / num_simulations * 100  # True negative rate

# Scenario 2: True Effect (generate binary data and calculate probabilities afterward)

# Initialize vectors to store results
p_values_true_effect <- numeric(num_simulations)
effect_sizes <- numeric(num_simulations)
prob_control_empirical_2 <- numeric(num_simulations)
prob_experiment_empirical_2 <- numeric(num_simulations)

# Run simulations for Scenario 2 (True Effect)
for (i in 1:num_simulations) {
  # Generate binary outcomes (0 or 1) for control and experimental groups without predefined probabilities
  group_Control <- rbinom(sample_size_per_group, 1, 0.20)
  group_Experiment <- rbinom(sample_size_per_group, 1, 0.25)
  
  # Calculate empirical probabilities
  prob_control_empirical_2[i] <- mean(group_Control)
  prob_experiment_empirical_2[i] <- mean(group_Experiment)
  
  # Perform proportion test
  success_counts <- c(sum(group_Control), sum(group_Experiment))
  sample_sizes <- c(sample_size_per_group, sample_size_per_group)
  prop_results <- prop.test(success_counts, sample_sizes)
  
  # Store p-value and effect size (difference in proportions)
  p_values_true_effect[i] <- prop_results$p.value
  effect_sizes[i] <- (sum(group_Experiment) / sample_size_per_group) - (sum(group_Control) / sample_size_per_group)
}

# Calculate True Effect Size and 95% Confidence Interval
true_effect <- mean(prob_experiment_empirical_2) - mean(prob_control_empirical_2)
ci_95 <- quantile(effect_sizes, c(0.025, 0.975))

# Calculate False Negatives and True Positives for Scenario 2
false_negatives <- sum(p_values_true_effect >= alpha) / num_simulations * 100  # Type II error rate
true_positives <- sum(p_values_true_effect < alpha) / num_simulations * 100   # True positive rate

# Create a summary table of the results
results_table <- data.frame(
  Research_Question = c("Question 1", "Question 1", "Question 2", "Question 2"),
  Scenario = c("No Effect", "Effect: (True Effect Size = calculated)", "No Effect", "Effect: (True Effect Size = calculated)"),
  Mean_Effect_in_Simulated_Data = c(0, true_effect, 0, true_effect),
  CI_95_Lower = c(NA, ci_95[1], NA, ci_95[1]),
  CI_95_Upper = c(NA, ci_95[2], NA, ci_95[2]),
  Percentage_of_False_Positives = c(false_positives, NA, false_positives, NA),
  Percentage_of_True_Negatives = c(true_negatives, NA, true_negatives, NA),
  Percentage_of_False_Negatives = c(NA, false_negatives, NA, false_negatives),
  Percentage_of_True_Positives = c(NA, true_positives, NA, true_positives)
)

# Display the summary table using DT::datatable() with custom formatting
datatable(results_table, 
          rownames = FALSE,
          options = list(pageLength = 5, autoWidth = TRUE),
          caption = "Summary of Simulation Results for Research Questions, Effect Sizes, False Positives, True Negatives, False Negatives, and True Positives") %>%
  formatStyle(
    columns = c('Scenario', 'Mean_Effect_in_Simulated_Data', 'CI_95_Lower', 'CI_95_Upper',
                'Percentage_of_False_Positives', 'Percentage_of_True_Negatives',
                'Percentage_of_False_Negatives', 'Percentage_of_True_Positives'),
    fontWeight = 'bold',
    textAlign = 'center'
  )


####################################################

# Load required libraries
library(DT)      # For datatable visualization
library(dplyr)   # For pipe operator (%>%)
library(pwr)     # For power calculation

# Define parameters
num_simulations <- 1000
sd_control <- 18
sd_experiment <- 32
alpha <- 0.05
true_effect <- 32 - 18  # Effect size of 14 (not 2.5)

# Calculate the required sample size for power of 0.8
effect_size <- true_effect / sqrt((sd_experiment^2 + sd_control^2)/2)  # Corrected effect size calculation
sample_size_per_group <- ceiling(pwr.t.test(d = effect_size, 
                                            power = 0.8, 
                                            sig.level = alpha, 
                                            type = "two.sample", 
                                            alternative = "greater")$n)

# Scenario 1: No True Effect (mean_control = mean_experiment)
mean_control <- 18
mean_experiment <- 18  # No true effect

# Initialize vectors to store results
p_values_no_effect <- numeric(num_simulations)

# Run simulations for Scenario 1 (No Effect)
set.seed(123)  # For reproducibility
for (i in 1:num_simulations) {
  group_Control <- rnorm(sample_size_per_group, mean_control, sd_control)
  group_Experiment <- rnorm(sample_size_per_group, mean_experiment, sd_experiment)
  
  t_results <- t.test(group_Experiment, group_Control, 
                      var.equal = FALSE,  # Changed to FALSE due to different SDs
                      alternative = "greater")
  
  p_values_no_effect[i] <- t_results$p.value
}

# Calculate False Positives and True Negatives for Scenario 1
false_positives <- sum(p_values_no_effect < alpha) / num_simulations * 100
true_negatives <- sum(p_values_no_effect >= alpha) / num_simulations * 100

# Scenario 2: True Effect
mean_control <- 18
mean_experiment <- 32  # True effect of 14

# Initialize vectors for Scenario 2
p_values_true_effect <- numeric(num_simulations)
effect_sizes <- numeric(num_simulations)

# Run simulations for Scenario 2
for (i in 1:num_simulations) {
  group_Control <- rnorm(sample_size_per_group, mean_control, sd_control)
  group_Experiment <- rnorm(sample_size_per_group, mean_experiment, sd_experiment)
  
  t_results <- t.test(group_Experiment, group_Control, 
                      var.equal = FALSE, 
                      alternative = "greater")
  
  p_values_true_effect[i] <- t_results$p.value
  effect_sizes[i] <- mean(group_Experiment) - mean(group_Control)
}

# Calculate CI and rates
ci_95 <- quantile(effect_sizes, c(0.025, 0.975))
false_negatives <- sum(p_values_true_effect >= alpha) / num_simulations * 100
true_positives <- sum(p_values_true_effect < alpha) / num_simulations * 100

# Create results table
results_table <- data.frame(
  Research_Question = c("Question 1", "Question 1", "Question 2", "Question 2"),
  Scenario = c("No Effect", paste0("Effect: (True Effect Size = ", true_effect, ")"),
               "No Effect", paste0("Effect: (True Effect Size = ", true_effect, ")")),
  Mean_Effect_in_Simulated_Data = c(0, true_effect, 0, true_effect),
  CI_95_Lower = c(NA, ci_95[1], NA, ci_95[1]),
  CI_95_Upper = c(NA, ci_95[2], NA, ci_95[2]),
  Percentage_of_False_Positives = c(false_positives, NA, false_positives, NA),
  Percentage_of_True_Negatives = c(true_negatives, NA, true_negatives, NA),
  Percentage_of_False_Negatives = c(NA, false_negatives, NA, false_negatives),
  Percentage_of_True_Positives = c(NA, true_positives, NA, true_positives)
)

# Display results
results_table %>%
  datatable(rownames = FALSE,
            options = list(pageLength = 5, autoWidth = TRUE),
            caption = paste("Summary of Simulation Results",
                            "(Power = 0.8, Sample Size per Group =", 
                            sample_size_per_group, ")")) %>%
  formatStyle(
    columns = names(results_table),
    fontWeight = 'bold',
    textAlign = 'center'
  )

sample_size_per_group <- ceiling(pwr.t.test(d = effect_size, 
                                            power = 0.8, 
                                            sig.level = alpha, 
                                            type = "two.sample", 
                                            alternative = "greater")$n)
sample_size_per_group


#################################################
# Define parameters
num_simulations <- 1000
sd_control <- 2.48
sd_experiment <- 2.41
alpha <- 0.05
true_effect <- 47.62 - 47.27

# Set the sample size per group to 551
sample_size_per_group <- 102

# Scenario 1: No True Effect (mean_control = mean_experiment)
mean_control <- 47.27
mean_experiment <- 47.27  # No true effect

# Initialize vectors to store results
p_values_no_effect <- numeric(num_simulations)

# Run simulations for Scenario 1 (No Effect)
set.seed(123)  # For reproducibility
for (i in 1:num_simulations) {
  group_Control <- rnorm(sample_size_per_group, mean_control, sd_control)
  group_Experiment <- rnorm(sample_size_per_group, mean_experiment, sd_experiment)
  
  t_results <- t.test(group_Experiment, group_Control, 
                      var.equal = FALSE,  # Changed to FALSE due to different SDs
                      alternative = "greater")
  
  p_values_no_effect[i] <- t_results$p.value
}

# Calculate False Positives and True Negatives for Scenario 1
false_positives <- sum(p_values_no_effect < alpha) / num_simulations * 100
true_negatives <- sum(p_values_no_effect >= alpha) / num_simulations * 100

# Scenario 2: True Effect
mean_control <- 47.27
mean_experiment <- 47.62  # True effect of 14

# Initialize vectors for Scenario 2
p_values_true_effect <- numeric(num_simulations)
effect_sizes <- numeric(num_simulations)

# Run simulations for Scenario 2
for (i in 1:num_simulations) {
  group_Control <- rnorm(sample_size_per_group, mean_control, sd_control)
  group_Experiment <- rnorm(sample_size_per_group, mean_experiment, sd_experiment)
  
  t_results <- t.test(group_Experiment, group_Control, 
                      var.equal = FALSE, 
                      alternative = "greater")
  
  p_values_true_effect[i] <- t_results$p.value
  effect_sizes[i] <- mean(group_Experiment) - mean(group_Control)
}

# Calculate CI and rates
ci_95 <- quantile(effect_sizes, c(0.025, 0.975))
false_negatives <- sum(p_values_true_effect >= alpha) / num_simulations * 100
true_positives <- sum(p_values_true_effect < alpha) / num_simulations * 100

# Create results table
results_table <- data.frame(
  Research_Question = c("Question 1", "Question 1", "Question 2", "Question 2"),
  Scenario = c("No Effect", paste0("Effect: (True Effect Size = ", true_effect, ")"),
               "No Effect", paste0("Effect: (True Effect Size = ", true_effect, ")")),
  Mean_Effect_in_Simulated_Data = c(0, true_effect, 0, true_effect),
  CI_95_Lower = c(NA, ci_95[1], NA, ci_95[1]),
  CI_95_Upper = c(NA, ci_95[2], NA, ci_95[2]),
  Percentage_of_False_Positives = c(false_positives, NA, false_positives, NA),
  Percentage_of_True_Negatives = c(true_negatives, NA, true_negatives, NA),
  Percentage_of_False_Negatives = c(NA, false_negatives, NA, false_negatives),
  Percentage_of_True_Positives = c(NA, true_positives, NA, true_positives)
)

# Display results
results_table %>%
  datatable(rownames = FALSE,
            options = list(pageLength = 5, autoWidth = TRUE),
            caption = paste("Summary of Simulation Results",
                            "(Power = 0.8, Sample Size per Group =", 
                            sample_size_per_group, ")")) %>%
  formatStyle(
    columns = names(results_table),
    fontWeight = 'bold',
    textAlign = 'center'
  )




# Calculate the required sample size per group for power = 0.8
sample_size_per_group <- ceiling(pwr.t.test(d = 0.35, 
                                            power = 0.8, 
                                            sig.level = 0.05, 
                                            type = "two.sample", 
                                            alternative = "greater")$n)

# Print the sample size per group
print(sample_size_per_group)



# Given Data
mean_control <- 47.62
mean_treatment2 <- 47.27
se_control <- 2.48
se_treatment2 <- 2.41

# Sample size (n) for both groups (assumed to be the same)
n <- 10548  # Update this value if a different sample size is known

# Calculate Standard Deviations from Standard Errors
sd_control <- se_control * sqrt(n)
sd_treatment2 <- se_treatment2 * sqrt(n)

# Calculate Pooled Standard Deviation
sd_pooled <- sqrt((sd_control^2 + sd_treatment2^2) / 2)

# Calculate Effect Size (Cohen's d)
effect_size <- (mean_control - mean_treatment2) / sd_pooled

# Print the effect size
cat("Effect Size (Cohen's d):", effect_size, "\n")
#########################################################

library(data.table)
library(DT)
library(pwr)

# Set up the number of iterations
iterations <- 1000

# Initializing vectors to save values
save_ps <- numeric(iterations)     # p-values
save_means_A <- numeric(iterations) # Means of group A
save_means_B <- numeric(iterations) # Means of group B
save_effect_size <- numeric(iterations) # Effect sizes (Cohen's d)

# Running the simulation for t-test
set.seed(123) # For reproducibility

for (i in 1:iterations) {
  group_A <- rnorm(551, 100, 7)  # Group A: n=10, mean=100, SD=7
  group_B <- rnorm(551, 105, 7)  # Group B: n=10, mean=105, SD=7
  
  # Calculate t-test
  t_results <- t.test(group_A, group_B, var.equal = TRUE)
  
  # Save p-value
  save_ps[i] <- t_results$p.value
  
  # Save means
  save_means_A[i] <- mean(group_A)
  save_means_B[i] <- mean(group_B)
  
  # Calculate and save effect size (Cohen's d)
  pooled_sd <- sqrt(((length(group_A) - 1) * var(group_A) + (length(group_B) - 1) * var(group_B)) / 
                      (length(group_A) + length(group_B) - 2))
  cohen_d <- (mean(group_B) - mean(group_A)) / pooled_sd
  save_effect_size[i] <- cohen_d
}

# Proportion of significant p-values
prop_p <- length(save_ps[save_ps < 0.05]) / iterations
print(prop_p)

# Displaying some descriptive statistics from the saved data
mean_A_overall <- mean(save_means_A)
mean_B_overall <- mean(save_means_B)
mean_effect_size <- mean(save_effect_size)

cat("Average Mean of Group A:", mean_A_overall, "\n")
cat("Average Mean of Group B:", mean_B_overall, "\n")
cat("Average Effect Size (Cohen's d):", mean_effect_size, "\n")


