# Define a function to calculate peak prominence
calculate_prominence <- function(data, peak_index) {
  peak_height <- data[peak_index]
  
  # Find the indices of values to the left and right of the peak
  left_indices <- (peak_index - 1):1
  right_indices <- (peak_index + 1):length(data)
  
  # Find the index of the leftmost value where data starts to decrease
  left_valley_index <- ifelse(
    length(left_indices) > 0,
    max(left_indices[data[left_indices] <= data[peak_index]]),
    0
  )
  
  # Find the index of the rightmost value where data starts to decrease
  right_valley_index <- ifelse(
    length(right_indices) > 0,
    min(right_indices[data[right_indices] <= data[peak_index]]),
    length(data) + 1
  )
  
  # Calculate prominence
  prominence <- peak_height - max(data[left_valley_index], data[right_valley_index])
  
  return(prominence)
}

