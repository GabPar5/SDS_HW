# set the parameters
total_students <- 150   # total number of students given by the text of the exercise
p <- 0.5                # probability for honest student (fair coin)
q <- 0.7                # probability for liar (biased coin)
alpha <- 0.1            # my choise of false positive rate
lambda <- 0.01
# randomly generate N within a specified range
set.seed(123)  # set seed for reproducibility
N <- sample(10^7:10^9,1)  # randomly choose one value between 10 and 100

# score function for each students
score_function <- function(N, p, q, alpha) {
  
  # calculate the threshold T using the qbinom function for the false positive rate alpha
  T <- N*q
  
  # calculate the true negative rate for honest students
  TN_rate <- pbinom(T, N, p)
  
  # calculate the true positive rate for lying students
  TP_rate <- 1 - pbinom(T, N, q)
  
  # calculate the false negative rate
  beta <- pbinom(T, N, q)
  
  # calculate balanced accuracy
  balanced_accuracy <- (TP_rate + TN_rate) / 2
  
  # calculate the final score
  score <- balanced_accuracy - (lambda * alpha * beta)
  
  return(score)
}

# simulate the score calculation for the entire class
scores <- replicate(total_students, score_function(N, p, q, alpha))

# calculate the average score for the class
average_score <- mean(scores)

# print the average score
print(paste("Average Score for the Class with N =", N, ": ", average_score))


# the accuracy as we studied in the FDS course is the ratio of correct predictions 
# (both true positives and true negatives) to the total number of cases, but here
# we have a imbalanced datasets, where one class significantly outnumbers the other, 
# so this accuracy can be misleading, so to bypass this scenario we thought about a
# balanced accuracy that take into account the average of the proportion of correct 
# predictions in each class individually so in few words it's the average of the 
# true positive rate and the true negative rate. In this way this 'new' accuracy
# treats both classes (honest and liars) equally, regardless of their size. 
# In our opinion it gives a more honest picture of the model's effectiveness, 
# especially in scenarios where one class is underrepresented as can be our case of study.