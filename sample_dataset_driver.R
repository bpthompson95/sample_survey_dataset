# Functions for generating both binary and ordinal/Likert-type data using probability density functions.
# Required inputs are the dataset, any demographic or other factor variables, and mean and sd targets for the factors.
# When using multiple factors, mean and sd targets are averaged across rows.

generate_data_1factor = function(data, factor1, codes, factor1_means, factor1_sd) {
  n <- nrow(data)
  individual_means <- factor1_means[data[[factor1]]]
  individual_sd <- factor1_sd[data[[factor1]]]
  generated_variable <- sapply(1:n, function(i) {
    probs <- dnorm(codes, mean = individual_means[i], sd = individual_sd[i])
    probs <- probs / sum(probs)
    sample(codes, 1, replace = TRUE, prob = probs)
  })
  return(generated_variable)
}


generate_data_2factor <- function(data, factor1, factor2, codes, 
                                  factor1_means, factor2_means, 
                                  factor1_sd, factor2_sd) {
  # Extract stratification levels
  n <- nrow(data)
  # Compute individual means based on factor1 (e.g., strata) and factor2 (e.g., urbanicity)
  individual_means <- (factor1_means[data[[factor1]]] + factor2_means[data[[factor2]]])/2
  # Compute individual standard deviations based on factor1 and factor2
  individual_sd <- (factor1_sd[data[[factor1]]] + factor2_sd[data[[factor2]]])/2
  # Generate the new variable
  generated_variable <- sapply(1:n, function(i) {
    # Compute probability distribution using dnorm()
    probs <- dnorm(codes, mean = individual_means[i], sd = individual_sd[i])
    # Normalize probabilities to sum to 1
    probs <- probs / sum(probs)
    # Sample from the given categorical response codes
    sample(codes, 1, replace = TRUE, prob = probs)
  })
  return(generated_variable)
}


generate_data_3factor <- function(data, factor1, factor2, factor3, codes, 
                                  factor1_means, factor2_means, factor3_means,
                                  factor1_sd, factor2_sd, factor3_sd) {
  # Extract stratification levels
  n <- nrow(data)
  # Compute individual means based on factor1 (e.g., strata) and factor2 (e.g., urbanicity)
  individual_means <- (factor1_means[data[[factor1]]] + factor2_means[data[[factor2]]] + factor3_means[data[[factor3]]])/3
  # Compute individual standard deviations based on factor1 and factor2
  individual_sd <- (factor1_sd[data[[factor1]]] + factor2_sd[data[[factor2]]] + factor3_sd[data[[factor3]]])/3
  # Generate the new variable
  generated_variable <- sapply(1:n, function(i) {
    # Compute probability distribution using dnorm()
    probs <- dnorm(codes, mean = individual_means[i], sd = individual_sd[i])
    # Normalize probabilities to sum to 1
    probs <- probs / sum(probs)
    # Sample from the given categorical response codes
    sample(codes, 1, replace = TRUE, prob = probs)
  })
  return(generated_variable)
}
