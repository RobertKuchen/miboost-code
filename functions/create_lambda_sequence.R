create_lambda_sequence <- function(selected_lambdas,
                                   n_lambda = 100, buffer_factor = 2){
  # Ensure positivity
  selected_lambdas <- selected_lambdas[selected_lambdas > 0]
  
  # Expand range a bit
  lambda_min <- min(selected_lambdas) / buffer_factor
  lambda_max <- max(selected_lambdas) * buffer_factor
  
  # Log-spaced sequence
  custom_lambda <- exp(seq(log(lambda_max), log(lambda_min), length.out = n_lambda))
  return(custom_lambda)
}