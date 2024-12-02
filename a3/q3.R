library(MPV)

# Load required libraries
library(car)

# Function to perform hypothesis test using linearHypothesis()
perform_linear_hypothesis_test <- function(model) {
  # Test the hypothesis that β1 = β2 = β3 = β4
  # We'll use a matrix to specify the linear constraints
  hypothesis_matrix <- matrix(c(1, -1, 0, 0, 0, 
                                0, 1, -1, 0, 0, 
                                0, 0, 1, -1, 0,
                                0, 0, 0, 1, -1),
                              nrow = 4, ncol = 5, byrow = TRUE)
  
  # Specify the hypothesis (all coefficients equal to 0)
  hypothesis_values <- c(0, 0, 0, 0)
  
  # Perform the linear hypothesis test
  linear_hypothesis_result <- linearHypothesis(
    model, 
    hypothesis_matrix, 
    hypothesis_values
  )
  
  return(linear_hypothesis_result)
}


# Fit the full model
full_model <- lm(y ~ x1 + x2 + x3 + x4, data = table.b1)

# Perform linear hypothesis test
hypothesis_test_result <- perform_linear_hypothesis_test(full_model)

# Print the results
print(hypothesis_test_result)