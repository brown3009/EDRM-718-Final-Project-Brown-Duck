# likert_functions.R

# Function to calculate the total score for a Likert scale survey, with reverse scoring
calculate_likert_score <- function(data, scale, reverse_items) {
  # Create a copy of the data to avoid modifying the original data
  data_copy <- data
  
  # Reverse score the specified items
  for (item in reverse_items) {
    # Calculate the reverse score
    data_copy[[paste0("Item_", item)]] <- scale + 1 - data_copy[[paste0("Item_", item)]]
  }
  
  # Sum all the item responses for each respondent
  data_copy$total_score <- rowSums(data_copy[, grep("^Item_", colnames(data_copy))], na.rm = TRUE)
  
  # Return the modified data frame with the total score
  return(data_copy)
}