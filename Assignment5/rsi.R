# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 5: Technical Analysis using R, Development Phase
# Script: rsi.R
# Author: Rachna Lekh
# Description: Custom implementation of Relative Strength Index (RSI) using
#              Wilder's smoothing method without external packages.
# ==============================================================================

#' Relative Strength Index (RSI)
#'
#' Computes the Relative Strength Index (0 to 100) following the assignment pseudocode.
#'
#' Design note: This implementation follows the assignment pseudocode exactly.
#' The Wilder smoothing loop runs from i = period+1 (1-based data index), updating
#' avg_gain/avg_loss using gains[i-1] (1-based), i.e. the gain one step BEFORE position i.
#' As a result, the first RSI value (at data index period+1) is already Wilder-smoothed
#' (it uses gains[period] 1-based to update the seed avg), unlike the textbook Wilder
#' variant which seeds the first RSI using a plain mean without smoothing.
#'
#' @param data A numeric vector representing historical prices.
#' @param period The lookback period (typically 14, or 5 in short tests).
#' @return A numeric vector of length equal to data, with NA for the first `period`
#'         indices and RSI values from index period+1 onward.
rsi <- function(data, period) {
  n <- length(data)

  # Return all NA if data is not longer than period
  if (n <= period) {
    return(rep(NA, n))
  }

  # Calculate the differences between consecutive data points
  diff_values <- diff(data)
  n_diff <- length(diff_values)

  # Initialize two vectors to store the gains and losses
  gains  <- numeric(n_diff)
  losses <- numeric(n_diff)

  # Calculate gains and losses
  for (i in 1:n_diff) {
    if (diff_values[i] > 0) {
      gains[i]  <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the initial average gain and average loss from the first `period` diffs
  avg_gain <- sum(gains[1:period])  / period
  avg_loss <- sum(losses[1:period]) / period

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA, n)

  # Wilder smoothing loop from i = period+1 to n (1-based data index).
  # At each step, gains[i-1] (1-based) updates the running average before
  # computing RSI[i]. This follows the assignment pseudocode literally.
  # Note: gains[i-1] in 1-based R = gains at the position one step before i.
  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}

# Standalone execution demo
if (sys.nframe() == 0) {
  cat("--- Relative Strength Index (RSI) Demo ---\n")
  data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
  rsi_result <- rsi(data, period = 5)
  cat("Input data:  ", paste(data, collapse = ", "), "\n")
  cat("RSI (k=5):   ", paste(round(rsi_result, 2), collapse = ", "), "\n")
  cat("Expected:    NA, NA, NA, NA, NA, 50.53, 68.93, 71.84, 78.21, 66.86\n")
}
