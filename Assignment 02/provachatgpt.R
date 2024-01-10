# Set the seed for reproducibility
set.seed(123)

# Parameters
population_mean <- 10
population_sd <- 2
t_max <- 10000

# Function to calculate coverage probability
calculate_coverage_probability <- function(population_mean, population_sd, t_max, num_simulations = 1000) {
  coverage_sequence <- numeric(t_max)
  coverage_clt <- numeric(t_max)
  
  for (sim in 1:num_simulations) {
    simulated_data <- matrix(rnorm(t_max, mean = population_mean, sd = population_sd), ncol = t_max)
    confidence_sequence <- matrix(0, nrow = t_max, ncol = 2)
    clt_confidence <- matrix(0, nrow = t_max, ncol = 2)
    
    for (t in 2:t_max) {
      # Confidence Sequence
      mean_val_sequence <- mean(simulated_data[1:t])
      se_sequence <- sqrt((log(log(2 * t)) + 0.72 * log(5.2/0.05)) / t)
      confidence_sequence[t, ] <- c(mean_val_sequence - se_sequence, mean_val_sequence + se_sequence)
      
      # CLT Confidence Interval
      mean_val_clt <- mean(simulated_data[1:t])
      se_clt <- sd(simulated_data[1:t]) / sqrt(t)
      margin_of_error_clt <- qnorm(1 - (0.05 / 2)) * se_clt
      clt_confidence[t, ] <- c(mean_val_clt - margin_of_error_clt, mean_val_clt + margin_of_error_clt)
    }
    
    # Check coverage
    true_in_sequence <- population_mean >= confidence_sequence[, 1] & population_mean <= confidence_sequence[, 2]
    true_in_clt <- population_mean >= clt_confidence[, 1] & population_mean <= clt_confidence[, 2]
    
    coverage_sequence <- coverage_sequence + true_in_sequence
    coverage_clt <- coverage_clt + true_in_clt
  }
  
  # Calculate coverage probabilities
  coverage_probability_sequence <- coverage_sequence / num_simulations
  coverage_probability_clt <- coverage_clt / num_simulations
  
  return(list(miscoverage_probability_sequence = 1-coverage_probability_sequence,
              miscoverage_probability_clt = 1-coverage_probability_clt))
}

# Calculate coverage probabilities
coverage_probabilities <- calculate_coverage_probability(population_mean, population_sd, t_max)

# Plot
plot(1:t_max, coverage_probabilities$miscoverage_probability_sequence, type = 'l', col = 'red', lty = 2,
     ylim = c(0, 1), xlab = 'Sample Size (t)', ylab = 'Coverage Probability',
     main = 'Coverage Probability Over Time')
lines(1:t_max, coverage_probabilities$miscoverage_probability_clt, col = 'blue', lty = 2)
legend("bottomright", legend = c("Confidence Sequence", "CLT Confidence Interval"),
       col = c("red", "blue"), lty = 2)


