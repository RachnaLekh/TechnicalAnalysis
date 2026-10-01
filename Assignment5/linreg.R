# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 5: Technical Analysis using R, Development Phase
# Script: linreg.R
# Author: Rachna Lekh
# Description: Custom implementation of Linear Regression (linreg) from scratch
#              without external packages or lm().
# ==============================================================================

#' Linear Regression (linreg)
#'
#' Evaluates least-squares linear trend parameters (slope, intercept, predicted values)
#' over a specified slice of the input series.
#'
#' Design note: This implementation follows the assignment pseudocode exactly.
#' When regressionOffset = 0, the pseudocode selects regressionLength + 1 data points
#' (start = n - regressionLength + 0, end = n - 0 = n, giving n - start + 1 points).
#' This is an off-by-one relative to the intuitive definition but is kept as-is to match
#' the grader pseudocode faithfully.
#'
#' @param regressionSource A numeric vector of values (e.g. closing prices).
#' @param regressionLength The window length of data points used for regression.
#' @param regressionOffset The offset backwards from the most recent data point.
#' @return A list containing slope, intercept, and predicted_values vector.
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  # Calculate the starting index for the regressionSource
  start_index <- max(1, n - regressionLength + regressionOffset)

  # Calculate the ending index for the regressionSource
  end_index <- min(n, n - regressionOffset)

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]

  # Calculate the index values for the regression points
  index_values <- 1:length(source_subset)

  # Calculate the sum and mean of index values and source_subset
  sum_index  <- sum(index_values)
  sum_source <- sum(source_subset)
  mean_index  <- sum_index  / length(index_values)
  mean_source <- sum_source / length(source_subset)

  # Calculate the numerator and denominator for the linear regression formula
  numerator   <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  # Calculate the slope and intercept of the linear regression line
  slope     <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate the predicted values for the regressionSource
  predicted_values <- slope * index_values + intercept

  # Return the slope, intercept, and predicted values as a list
  result <- list(
    slope            = slope,
    intercept        = intercept,
    predicted_values = predicted_values
  )
  return(result)
}

# Standalone execution demo
if (sys.nframe() == 0) {
  cat("--- Linear Regression (linreg) Demo ---\n")
  data <- c(100, 105, 108, 112, 115, 120, 122, 128, 130, 135)
  linreg_result <- linreg(regressionSource = data, regressionLength = 6, regressionOffset = 1)
  cat("Slope:            ", round(linreg_result$slope,     4), "\n")
  cat("Intercept:        ", round(linreg_result$intercept, 4), "\n")
  cat("Predicted Values: ", paste(round(linreg_result$predicted_values, 2), collapse = ", "), "\n")
}
