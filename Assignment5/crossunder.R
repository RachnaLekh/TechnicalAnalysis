# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 5: Technical Analysis using R, Development Phase
# Script: crossunder.R
# Author: Rachna Lekh
# Description: Custom implementation of Crossunder detector without external libraries.
# ==============================================================================

#' Crossunder Function
#'
#' Detects whether the first array crosses strictly under the second array.
#' NA-safe: if any of arr1[i], arr2[i], arr1[i-1], arr2[i-1] is NA, the signal
#' for that index is "False" (consistent with the pseudocode's True/False return format).
#' Index 1 always returns "None" (n < 2 edge case preserved).
#'
#' @param arr1 The first array.
#' @param arr2 The second array.
#' @return A character vector: "None" at index 1, then "True"/"False" strings.
crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  n <- length(arr1)

  # Initialize an array to store the crossunder signals
  crossunder_signals <- rep("None", n)

  # Check for crossunder signals at each data point starting from index 2
  if (n >= 2) {
    for (i in 2:n) {
      # NA-safe: if any value is NA, the signal is "False"
      if (is.na(arr1[i]) || is.na(arr2[i]) || is.na(arr1[i - 1]) || is.na(arr2[i - 1])) {
        crossunder_signals[i] <- "False"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }

  return(crossunder_signals)
}

# Standalone execution demo
if (sys.nframe() == 0) {
  cat("--- Crossunder Signals Demo ---\n")
  arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
  crossunder_signals <- crossunder(arr1, arr2)
  cat("arr1:    ", paste(arr1, collapse = ", "), "\n")
  cat("arr2:    ", paste(arr2, collapse = ", "), "\n")
  cat("Signals: ", paste(crossunder_signals, collapse = ", "), "\n")
}
