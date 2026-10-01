# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 5: Technical Analysis using R, Development Phase
# Script: ema.R
# Author: Rachna Lekh
# Description: Custom implementation of Exponential Moving Average (EMA) from scratch
#              without external financial indicator packages.
# ==============================================================================

#' Exponential Moving Average (EMA)
#'
#' Calculates the exponential moving average of a numeric vector using recursive smoothing.
#'
#' @param data A numeric vector representing the time series data.
#' @param period An integer specifying the EMA period.
#' @return A numeric vector containing the calculated EMA values.
ema <- function(data, period) {
  n <- length(data)
  if (n == 0) return(numeric(0))
  if (period <= 0) stop("Period must be a positive integer")
  
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)
  
  # Initialize an empty array to store EMA values
  ema_values <- numeric(n)
  
  # Loop through the data array
  for (i in 1:n) {
    # Calculate EMA for the first data point
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }
  
  return(ema_values)
}

# Standalone execution demo
if (sys.nframe() == 0) {
  cat("--- Exponential Moving Average (EMA) Demo ---\n")
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  ema_result <- ema(data, period = 3)
  cat("Input data:  ", paste(data, collapse = ", "), "\n")
  cat("EMA (k=3):   ", paste(round(ema_result, 3), collapse = ", "), "\n")
}
