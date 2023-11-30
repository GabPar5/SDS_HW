# Set the parameters
total_students <- 150   # Total number of students
p <- 0.5                # Probability for honest student
q <- 0.7                # Probability for liar 
lambda <- 0.01          # Weighting factor

# Function to calculate the score for given N, p, q, std_dev, alpha and beta
score_function <- function(N, p, q, lambda) {
  std_dev <- sqrt(N * p * (1 - p)) # Standard deviation
  T <- N * p + 2 * std_dev  # Set the threshold at mean + 2 standard deviations
  
  beta <- pbinom(T, N, q) # Calculate the false negative rate
  alpha <- 1 - pbinom(T, N, p) # Calculate the false positive rate
  
  TN_rate <- 1 - alpha # True negative rate for honest students
  TP_rate <- 1 - beta # True positive rate for lying students
  
  balanced_accuracy <- (TP_rate + TN_rate) / 2 # Balanced accuracy
  
  score <- balanced_accuracy - (lambda * alpha * beta) # Final score
  
  return(score)
}

# Prepare a range of N values to explore
N_values <- seq(10, 100, by = 1)

# Data frame to store results
results <- data.frame(K = integer(), N = integer(), Score = numeric())

# Iterate over possible values of k and N
for (k in 1:(total_students - 1)) {
  for (N in N_values) {
    score <- score_function(N, p, q, lambda)
    results <- rbind(results, data.frame(K = k, N = N, Score = score))
  }
}


tail(results)
max(results$Score)
max_score_k <- results$K[which.max(results$Score)]
max_score_k

