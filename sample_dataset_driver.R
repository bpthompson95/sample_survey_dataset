# Function for generating both binary and ordinal/Likert-type data...
#   Required inputs are the stratification variable (strata), the variable codes, the desired mean values per stratum, and the standard deviation per stratum.
#   Allows for specification of variable means and standard deviations within strata.
generate_data = function(strata, codes, mean_values, sd) {
  question_data <- sapply(strata, function(s) {
    sample(codes, 1, replace = TRUE, prob = dnorm(codes, mean = mean_values[s], sd = sd[s]))
  })
  return(question_data)
}
