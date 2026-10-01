# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 5: Technical Analysis using R, Development Phase
# Script: sma.R
# Author: Rachna Lekh
# Description: Custom implementation of Simple Moving Average (SMA) from scratch
#              without external financial indicator packages.
# ==============================================================================

#' Simple Moving Average (SMA)
#'
#' Calculates the simple moving average of a numeric vector across a rolling window.
#' Returns n - period + 1 values (no NA padding) as per the assignment pseudocode.
#' Callers that need a length-n vector must pad explicitly:
#'   c(rep(NA, period - 1), sma(data, period))
#'
#' @param data A numeric vector representing the time series data (e.g. closing prices).
#' @param period An integer specifying the window size for the average.
#' @return A numeric vector of length n - period + 1 containing SMA values.
sma <- function(data, period) {
  # Calculate the length of the data array
  n <- length(data)

  # Check if period is greater than length of data
  if (period > n) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize an array to store SMA values (length = n - period + 1)
  sma_values <- numeric(n - period + 1)

  # Loop through the data array starting from index 'period'
  for (i in period:n) {
    # Calculate the sum of the elements in the window
    window_data <- data[(i - period + 1):i]
    mean_val <- sum(window_data) / period

    # Store the mean value (offset index so result starts at 1)
    sma_values[i - period + 1] <- mean_val
  }

  return(sma_values)
}

# Standalone execution demo
if (sys.nframe() == 0) {
  cat("--- Simple Moving Average (SMA) Demo ---\n")
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  sma_result <- sma(data, period = 3)
  cat("Input data:  ", paste(data, collapse = ", "), "\n")
  cat("SMA (k=3):   ", paste(round(sma_result, 3), collapse = ", "), "\n")
  cat("(Returns", length(sma_result), "values; pad with", 3 - 1, "leading NAs if length-n vector needed)\n")
}
