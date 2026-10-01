# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 5: Technical Analysis using R, Development Phase
# Script: crossover.R
# Author: Rachna Lekh
# Description: Custom implementation of Crossover detector without external libraries.
# ==============================================================================

#' Crossover Function
#'
#' Evaluates when a faster series crosses above or below a slower series.
#' NA-safe: if any of arr1[i], arr2[i], arr1[i-1], arr2[i-1] is NA, the signal
#' for that index is "None".
#'
#' @param arr1 The first array (e.g. short-term moving average).
#' @param arr2 The second array (e.g. long-term moving average).
#' @return A character vector of signals: "Up", "Down", or "None".
crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  n <- length(arr1)

  # Initialize an array to store the crossover signals
  crossover_signals <- rep("None", n)

  # Check for crossovers at each data point starting from index 2
  if (n >= 2) {
    for (i in 2:n) {
      # NA-safe: if any value is NA, keep "None" for this index
      if (is.na(arr1[i]) || is.na(arr2[i]) || is.na(arr1[i - 1]) || is.na(arr2[i - 1])) {
        crossover_signals[i] <- "None"
      } else if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}

# Standalone execution demo
if (sys.nframe() == 0) {
  cat("--- Crossover Signals Demo ---\n")
  arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
  crossover_signals <- crossover(arr1, arr2)
  cat("arr1:    ", paste(arr1, collapse = ", "), "\n")
  cat("arr2:    ", paste(arr2, collapse = ", "), "\n")
  cat("Signals: ", paste(crossover_signals, collapse = ", "), "\n")
}
