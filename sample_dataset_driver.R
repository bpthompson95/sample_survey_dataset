# Function for generating both binary and ordinal/Likert-type data...
# -- Required inputs are the stratification variable (strata), an array of the variable's codes, an array of the desired mean values per stratum, and an array of the standard deviations per stratum.
# -- Allows for specification of response means and dispersion within each stratum.
generate_data = function(strata, codes, mean_values, sd) {
  question_data <- sapply(strata, function(s) {
    sample(codes, 1, replace = TRUE, prob = dnorm(codes, mean = mean_values[s], sd = sd[s]))
  })
  return(question_data)
}
