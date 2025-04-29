# Function to calculate largest residuals
find_largest_residuals <- function(data, response_var, explanatory_var, p) {
  
  # Fit a linear model to predict the response variable from the explanatory variable
  model <- lm(formula = as.formula(paste(response_var, "~", explanatory_var)), data = data)
  
  # Get the residuals from the model
  residuals <- residuals(model)
  
  # Calculate the absolute residuals
  abs_residuals <- abs(residuals)
  
  # Identify the p largest residuals (in terms of absolute value)
  largest_residuals_threshold <- sort(abs_residuals, decreasing = TRUE)[p]
  
  # Create a logical vector indicating TRUE for the p largest residuals and FALSE otherwise
  large_residuals_flag <- abs_residuals >= largest_residuals_threshold
  
  # Return the logical vector
  return(large_residuals_flag)
}