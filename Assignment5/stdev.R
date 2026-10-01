# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 5: Technical Analysis using R, Development Phase
# Script: stdev.R
# Author: Rachna Lekh
# Description: Custom implementation of Standard Deviation (stdev) from scratch
#              without external financial indicator packages or stats::sd.
# ==============================================================================

#' Standard Deviation (stdev)
#'
#' Computes the sample/population standard deviation from first principles.
#'
#' @param data A numeric vector of values.
#' @return A numeric scalar representing the standard deviation.
stdev <- function(data) {
  n <- length(data)
  if (n <= 1) return(0)
  
  # Calculate the mean of the data
  mean_value <- sum(data) / n
  
  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value
  
  # Calculate the squared differences
  squared_diff <- diff_values * diff_values
  
  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)
  
  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}

# Standalone execution demo
if (sys.nframe() == 0) {
  cat("--- Standard Deviation (stdev) Demo ---\n")
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  stdev_result <- stdev(data)
  cat("Input data:         ", paste(data, collapse = ", "), "\n")
  cat("Standard Deviation: ", round(stdev_result, 4), "\n")
}
